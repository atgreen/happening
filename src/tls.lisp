;;; tls.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; TLS server integration using pure-tls/acme Hunchentoot integration

(in-package #:happening)

;;; ----------------------------------------------------------------------------
;;; Configuration
;;; ----------------------------------------------------------------------------

(defparameter *cert-store-path* nil
  "Custom path for certificate storage. If NIL, uses pure-tls/acme platform defaults:
   - Linux: ~/.local/state/pure-tls/
   - macOS: ~/Library/Application Support/pure-tls/
   - Windows: %LOCALAPPDATA%/pure-tls/")

(defparameter *default-renewal-days* 30
  "Number of days before expiry to trigger automatic certificate renewal.")

;;; ----------------------------------------------------------------------------
;;; Logging adapter for pure-tls/acme
;;; ----------------------------------------------------------------------------

(defun happening-acme-logger (level format-string &rest args)
  "Logging function for the ACME acceptor."
  (let ((message (apply #'format nil format-string args)))
    (case level
      (:debug (llog:debug message))
      (:info (llog:info message))
      (:warn (llog:info message))  ; llog doesn't have warn, use info
      (:error (llog:error message))
      (otherwise (llog:info message)))))

;;; ----------------------------------------------------------------------------
;;; ACME Acceptor Constructor
;;; ----------------------------------------------------------------------------
;;;
;;; Uses pure-tls/acme:make-acme-acceptor directly. The acme-acceptor class
;;; extends hunchentoot:easy-acceptor, which handles *dispatch-table* for
;;; easy-routes integration automatically.

(defun make-happening-acme-acceptor (domains email &key
                                                     (port 443)
                                                     (production nil)
                                                     (renewal-days *default-renewal-days*)
                                                     (cert-store-path *cert-store-path*))
  "Create an ACME acceptor with automatic Let's Encrypt certificate management.

   Uses pure-tls/acme:make-acme-acceptor which provides:
   - Automatic certificate acquisition on startup
   - TLS-ALPN-01 challenge handling (no port 80 needed)
   - Background certificate renewal
   - Integration with hunchentoot:easy-acceptor for route dispatching

   DOMAINS - Single domain string or list of domains for the certificate.
             The first domain is the primary (used for certificate storage).
   EMAIL - Contact email for Let's Encrypt account.
   PORT - HTTPS port (default 443).
   PRODUCTION - Use Let's Encrypt production (default NIL = staging).
   RENEWAL-DAYS - Renew certificate when it expires within this many days (default 30).
   CERT-STORE-PATH - Custom path for certificate storage (default: platform-specific).

   Example:
     ;; Single domain
     (make-happening-acme-acceptor \"example.com\" \"admin@example.com\"
                                   :production t)

     ;; Multiple domains (SAN certificate)
     (make-happening-acme-acceptor '(\"example.com\" \"www.example.com\")
                                   \"admin@example.com\"
                                   :production t)"
  (let ((store (when cert-store-path
                 (pure-tls/acme:make-cert-store :path (pathname cert-store-path)))))
    (pure-tls/acme:make-acme-acceptor
     domains email
     :port port
     :production production
     :renewal-days renewal-days
     :logger #'happening-acme-logger
     :store store)))

;;; ----------------------------------------------------------------------------
;;; URL parsing and HTTPS detection
;;; ----------------------------------------------------------------------------

(defun parse-base-url ()
  "Parse *base-url* and return (values scheme host port)."
  (when *base-url*
    (let ((uri (quri:uri *base-url*)))
      (values (quri:uri-scheme uri)
              (quri:uri-host uri)
              (or (quri:uri-port uri)
                  (if (string= (quri:uri-scheme uri) "https") 443 80))))))

(defun https-configured-p ()
  "Return T if *base-url* is configured for HTTPS."
  (multiple-value-bind (scheme host port)
      (parse-base-url)
    (declare (ignore host port))
    (string= scheme "https")))

(defun configured-domain ()
  "Return the domain from *base-url*."
  (multiple-value-bind (scheme host port)
      (parse-base-url)
    (declare (ignore scheme port))
    host))
