;;; server.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Web server and routes for Happening analytics

(in-package :happening)

(version-string:define-version-parameter +version+ :happening)

;;; ----------------------------------------------------------------------------
;;; JSON encoding helper
;;; ----------------------------------------------------------------------------

(defun plist-to-alist (plist)
  "Convert a plist to an alist for JSON encoding."
  (loop for (key value) on plist by #'cddr
        collect (cons key value)))

(defun convert-for-json (data)
  "Recursively convert plists to alists for proper JSON object encoding.
   cl-json's guessing encoder treats plists as arrays but alists as objects."
  (cond
    ;; Null
    ((null data) nil)
    ;; Atom (number, string, symbol)
    ((atom data) data)
    ;; Plist (starts with a keyword)
    ((and (listp data) (keywordp (car data)))
     (loop for (key value) on data by #'cddr
           collect (cons (string-downcase (symbol-name key))
                         (convert-for-json value))))
    ;; List of items (e.g., list of plists from database)
    ((listp data)
     (mapcar #'convert-for-json data))
    ;; Fallback
    (t data)))

;;; ----------------------------------------------------------------------------
;;; Server machinery
;;; ----------------------------------------------------------------------------

(defvar *shutdown-cv* (bt:make-condition-variable))
(defvar *server-lock* (bt:make-lock))
(defvar *acceptor* nil)

(defclass my-acceptor (easy-routes:easy-routes-acceptor)
  ()
  (:documentation "Custom acceptor for Happening."))

(defclass https-redirect-acceptor (hunchentoot:acceptor)
  ((https-port :initarg :https-port :accessor redirect-https-port :initform 443))
  (:documentation "Acceptor that redirects all HTTP requests to HTTPS."))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor https-redirect-acceptor) request)
  "Redirect all requests to HTTPS."
  (let* ((host (hunchentoot:host request))
         ;; Strip port from host if present
         (hostname (first (cl-ppcre:split ":" host)))
         (https-port (redirect-https-port acceptor))
         (path (hunchentoot:request-uri request))
         ;; Build HTTPS URL (omit :443 as it's the default)
         (https-url (if (= https-port 443)
                        (format nil "https://~A~A" hostname path)
                        (format nil "https://~A:~A~A" hostname https-port path))))
    (setf (hunchentoot:return-code*) hunchentoot:+http-moved-permanently+)
    (setf (hunchentoot:header-out :location) https-url)
    ;; Return empty body
    ""))

;; Use embedded static assets (no external files needed)
(defparameter *static-dispatch-table* (make-embedded-dispatch-table))

;;; ----------------------------------------------------------------------------
;;; Middleware helpers
;;; ----------------------------------------------------------------------------

(defun require-setup-or-auth ()
  "Redirect to setup if needed, otherwise require authentication."
  (cond
    ((needs-setup-p)
     (unless (string= (hunchentoot:script-name*) "/setup")
       (hunchentoot:redirect "/setup")))
    ((not (authenticated-p))
     (unless (member (hunchentoot:script-name*)
                     '("/login" "/api/event")
                     :test #'string=)
       (hunchentoot:redirect "/login")))))

;;; ----------------------------------------------------------------------------
;;; Public routes
;;; ----------------------------------------------------------------------------

;; Home page - redirect to dashboard or login
(easy-routes:defroute index ("/" :method :get) ()
  (cond
    ((needs-setup-p)
     (hunchentoot:redirect "/setup"))
    ((authenticated-p)
     (let ((sites (list-sites)))
       (if sites
           (hunchentoot:redirect (format nil "/dashboard/~A" (getf (first sites) :|id|)))
           (hunchentoot:redirect "/sites/new"))))
    (t
     (hunchentoot:redirect "/login"))))

;;; ----------------------------------------------------------------------------
;;; Setup routes
;;; ----------------------------------------------------------------------------

(easy-routes:defroute setup-page ("/setup" :method :get) ()
  (if (needs-setup-p)
      (progn
        (setf (hunchentoot:content-type*) "text/html")
        (render-setup-page))
      (hunchentoot:redirect "/")))

(easy-routes:defroute setup-submit ("/setup" :method :post) ()
  (require-valid-csrf)
  (if (needs-setup-p)
      (let ((username (hunchentoot:post-parameter "username"))
            (password (hunchentoot:post-parameter "password"))
            (password-confirm (hunchentoot:post-parameter "password_confirm"))
            (domain (hunchentoot:post-parameter "domain"))
            (site-name (hunchentoot:post-parameter "site_name")))
        (multiple-value-bind (success result)
            (process-setup-form username password password-confirm domain site-name)
          (if success
              (let ((snippet (get-tracking-snippet result)))
                ;; Log in the new user
                (let ((user (authenticate-user username password)))
                  (when user
                    (let ((token (create-session (getf user :|id|))))
                      (set-session-cookie token))))
                (setf (hunchentoot:content-type*) "text/html")
                (render-setup-complete-page result snippet))
              (progn
                (setf (hunchentoot:content-type*) "text/html")
                (render-setup-page result)))))
      (hunchentoot:redirect "/")))

;;; ----------------------------------------------------------------------------
;;; Auth routes
;;; ----------------------------------------------------------------------------

(easy-routes:defroute login-page ("/login" :method :get) ()
  (if (authenticated-p)
      (hunchentoot:redirect "/")
      (progn
        (setf (hunchentoot:content-type*) "text/html")
        (render-login-page))))

(easy-routes:defroute login-submit ("/login" :method :post) ()
  (require-valid-csrf)
  (let ((username (hunchentoot:post-parameter "username"))
        (password (hunchentoot:post-parameter "password")))
    (let ((user (authenticate-user username password)))
      (if user
          (let ((token (create-session (getf user :|id|))))
            (set-session-cookie token)
            (hunchentoot:redirect "/"))
          (progn
            (setf (hunchentoot:content-type*) "text/html")
            (render-login-page "Invalid username or password"))))))

(easy-routes:defroute logout ("/logout" :method :get) ()
  (let ((token (get-session-cookie)))
    (when token
      (delete-session token))
    (clear-session-cookie)
    (hunchentoot:redirect "/login")))

;;; ----------------------------------------------------------------------------
;;; Dashboard routes
;;; ----------------------------------------------------------------------------

(easy-routes:defroute dashboard-redirect ("/dashboard" :method :get) ()
  (require-auth)
  (let ((sites (list-sites)))
    (if sites
        (hunchentoot:redirect (format nil "/dashboard/~A" (getf (first sites) :|id|)))
        (hunchentoot:redirect "/sites/new"))))

(easy-routes:defroute dashboard ("/dashboard/:site-id" :method :get)
    (&get range)
  (require-auth)
  (let ((site (get-site site-id)))
    (if site
        (let* ((days (cond
                       ((string= range "7d") 7)
                       ((string= range "90d") 90)
                       (t 30)))
               (end-date (today))
               (start-date (days-ago days))
               (stats (get-dashboard-stats site-id start-date end-date)))
          (setf (hunchentoot:content-type*) "text/html")
          (render-dashboard site stats start-date end-date))
        (progn
          (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
          "Site not found"))))

;;; ----------------------------------------------------------------------------
;;; Sites routes
;;; ----------------------------------------------------------------------------

(easy-routes:defroute sites-list ("/sites" :method :get) ()
  (require-auth)
  (setf (hunchentoot:content-type*) "text/html")
  (render-sites-page))

(easy-routes:defroute new-site-page ("/sites/new" :method :get) ()
  (require-auth)
  (setf (hunchentoot:content-type*) "text/html")
  (render-new-site-page))

(easy-routes:defroute new-site-submit ("/sites/new" :method :post) ()
  (require-auth)
  (require-valid-csrf)
  (let ((domain (hunchentoot:post-parameter "domain"))
        (site-name (hunchentoot:post-parameter "site_name")))
    (cond
      ((or (null domain) (string= domain ""))
       (setf (hunchentoot:content-type*) "text/html")
       (render-new-site-page "Domain is required"))
      ((or (null site-name) (string= site-name ""))
       (setf (hunchentoot:content-type*) "text/html")
       (render-new-site-page "Site name is required"))
      (t
       (let ((site-id (create-site domain site-name)))
         (if site-id
             (hunchentoot:redirect (format nil "/sites/~A" site-id))
             (progn
               (setf (hunchentoot:content-type*) "text/html")
               (render-new-site-page "Domain already exists"))))))))

(easy-routes:defroute site-details ("/sites/:site-id" :method :get) ()
  (require-auth)
  (let ((site (get-site site-id)))
    (if site
        (progn
          (setf (hunchentoot:content-type*) "text/html")
          (render-site-details-page site))
        (progn
          (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
          "Site not found"))))

;;; ----------------------------------------------------------------------------
;;; API routes
;;; ----------------------------------------------------------------------------

;; Tracking event endpoint (public, no auth required)
(easy-routes:defroute api-event ("/api/event" :method :post) ()
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (case (process-tracking-event body)
      (:ok
       (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)
       "")
      (:invalid-site
       (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
       "Invalid site")
      (:invalid-data
       (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
       "Invalid data"))))

;; Dashboard stats API (JSON)
(easy-routes:defroute api-stats ("/api/stats/:site-id" :method :get)
    (&get start end)
  (require-auth)
  (let ((site (get-site site-id)))
    (if site
        (let* ((start-date (or start (days-ago 30)))
               (end-date (or end (today)))
               (stats (get-dashboard-stats site-id start-date end-date)))
          (setf (hunchentoot:content-type*) "application/json")
          ;; Convert plists to alists for proper JSON object encoding
          (cl-json:encode-json-to-string (convert-for-json stats)))
        (progn
          (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
          (setf (hunchentoot:content-type*) "application/json")
          "{\"error\": \"Site not found\"}"))))

;;; ----------------------------------------------------------------------------
;;; Server start/stop
;;; ----------------------------------------------------------------------------

(defun start-server (port &key https-port)
  "Start the Happening web server.
   If *base-url* is HTTPS, automatically obtains Let's Encrypt certificates."
  ;; Always catch errors to prevent crashes
  (setf hunchentoot:*catch-errors-p* t)

  ;; Only show detailed errors in development mode (DEBUG=true)
  (let ((debug-mode (string= (uiop:getenv "DEBUG") "true")))
    (setf hunchentoot:*show-lisp-errors-p* debug-mode)
    (setf hunchentoot:*show-lisp-backtraces-p* debug-mode)
    (when debug-mode
      (llog:info "Debug mode enabled - detailed errors will be shown")))

  ;; Configure trusted proxy settings (TRUST_PROXY="true" or comma-separated IPs)
  (let ((trust-proxy-env (uiop:getenv "TRUST_PROXY")))
    (when trust-proxy-env
      (setf *trust-proxy*
            (cond
              ((string= trust-proxy-env "true") t)
              ((string= trust-proxy-env "false") nil)
              (t (mapcar (lambda (s) (string-trim " " s))
                         (cl-ppcre:split "," trust-proxy-env)))))
      (llog:info (format nil "Proxy trust configured: ~A" *trust-proxy*))))

  (llog:info (format nil "Starting Happening v~A (embedded assets)" +version+))

  ;; Set up static file handlers (for easy-routes dispatch)
  (setf hunchentoot:*dispatch-table* *static-dispatch-table*)

  (if (https-configured-p)
      ;; Start HTTPS server with automatic ACME certificate management
      (let* ((domain (configured-domain))
             (tls-port (or https-port 443))
             (email (or (uiop:getenv "ACME_EMAIL")
                        (format nil "admin@~A" domain)))
             (production (not (string= (uiop:getenv "ACME_STAGING") "true")))
             (cert-path (uiop:getenv "ACME_CERT_PATH"))
             (renewal-days (let ((env-val (uiop:getenv "ACME_RENEWAL_DAYS")))
                             (when env-val (parse-integer env-val :junk-allowed t)))))
        (llog:info (format nil "Starting HTTPS server on port ~A" tls-port))
        (llog:info (format nil "Domain: ~A, Email: ~A" domain email))
        (llog:info (format nil "Let's Encrypt: ~A" (if production "production" "staging")))
        (when cert-path
          (llog:info (format nil "Certificate store: ~A" cert-path)))

        ;; Enable handshake debug logging if requested
        (when (string= (uiop:getenv "TLS_DEBUG") "true")
          (llog:info "TLS handshake debug logging enabled")
          (setf pure-tls::*handshake-debug* t))

        ;; The acme-acceptor handles certificate acquisition/renewal automatically
        (setf *acceptor* (make-happening-acme-acceptor
                          domain email
                          :port tls-port
                          :production production
                          :cert-store-path cert-path
                          :renewal-days (or renewal-days *default-renewal-days*)))
        (hunchentoot:start *acceptor*)
        (llog:info (format nil "HTTPS server started on port ~A" tls-port))

        ;; Start HTTP server that redirects all traffic to HTTPS
        (llog:info (format nil "Starting HTTP redirect server on port ~A" port))
        (let ((http-acceptor (make-instance 'https-redirect-acceptor
                                            :port port
                                            :https-port tls-port)))
          (hunchentoot:start http-acceptor)
          (llog:info (format nil "HTTP server started on port ~A (301 -> HTTPS)" port))))

      ;; Start HTTP-only server
      (progn
        (llog:info (format nil "Starting HTTP server on port ~A" port))
        (setf *acceptor* (make-instance 'my-acceptor :port port))
        (hunchentoot:start *acceptor*)
        (llog:info (format nil "HTTP server started on port ~A" port))))

  *acceptor*)

(defun stop-server ()
  "Stop the Happening web server."
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)
    (llog:info "Server stopped")))
