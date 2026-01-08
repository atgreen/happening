;;; main.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Main entry point for Happening analytics

(in-package #:happening)

(defun fatal-error (format-string &rest args)
  "Log a fatal error and exit."
  (llog:error (apply #'format nil format-string args))
  (uiop:quit 1))

(defun extract-domain-from-url (url)
  "Extract domain from a URL like https://example.com -> example.com"
  (let ((url-str (string-trim '(#\Space #\Tab #\/) url)))
    ;; Remove protocol
    (when (search "://" url-str)
      (setf url-str (subseq url-str (+ 3 (search "://" url-str)))))
    ;; Remove path
    (when (position #\/ url-str)
      (setf url-str (subseq url-str 0 (position #\/ url-str))))
    ;; Remove port
    (when (position #\: url-str)
      (setf url-str (subseq url-str 0 (position #\: url-str))))
    url-str))

(defun make-setup-command ()
  "Create the setup subcommand for non-interactive setup."
  (let ((admin-opt (clingon:make-option
                    :string
                    :short-name #\a
                    :long-name "admin"
                    :key :admin
                    :description "Admin username"
                    :initial-value nil))
        (password-opt (clingon:make-option
                       :string
                       :short-name #\P
                       :long-name "password"
                       :key :password
                       :description "Admin password (min 8 characters)"
                       :initial-value nil))
        (url-opt (clingon:make-option
                  :string
                  :short-name #\u
                  :long-name "url"
                  :key :url
                  :description "Public URL (e.g. https://analytics.example.com)"
                  :initial-value nil))
        (email-opt (clingon:make-option
                    :string
                    :short-name #\e
                    :long-name "email"
                    :key :email
                    :description "Email for Let's Encrypt certificate notifications"
                    :initial-value nil))
        (db-opt (clingon:make-option
                 :string
                 :short-name #\d
                 :long-name "database"
                 :key :database
                 :description "Path to SQLite database file"
                 :initial-value nil)))
    (clingon:make-command
     :name "setup"
     :description "Set up Happening with admin account and first site"
     :usage "[OPTIONS]"
     :options (list admin-opt password-opt url-opt email-opt db-opt)
     :handler (lambda (cmd)
                (let ((admin (clingon:getopt cmd :admin))
                      (password (clingon:getopt cmd :password))
                      (url (clingon:getopt cmd :url))
                      (email (clingon:getopt cmd :email))
                      (db-path (clingon:getopt cmd :database)))

                  ;; Initialize database
                  (init-database db-path)

                  ;; If all CLI options provided, do non-interactive setup
                  (if (and admin password url)
                      (progn
                        ;; Validate inputs
                        (when (< (length admin) 3)
                          (fatal-error "Admin username must be at least 3 characters"))
                        (when (< (length password) 8)
                          (fatal-error "Password must be at least 8 characters"))

                        ;; Check if setup needed
                        (unless (needs-setup-p)
                          (fatal-error "Setup has already been completed"))

                        ;; Extract domain from URL
                        (let ((domain (extract-domain-from-url url)))
                          (format t "Setting up Happening...~%")
                          (format t "  Admin: ~A~%" admin)
                          (format t "  URL: ~A~%" url)
                          (format t "  Domain: ~A~%" domain)

                          ;; Set and persist base URL
                          (setf *base-url* url)
                          (set-config "base_url" url)

                          ;; Create admin user
                          (let ((user-id (create-user admin password)))
                            (unless user-id
                              (fatal-error "Failed to create admin user")))

                          ;; Create site
                          (let ((site-id (create-site domain domain)))
                            (unless site-id
                              (fatal-error "Failed to create site"))

                            (format t "~%Setup complete!~%")
                            (format t "~%Start the server with:~%")
                            (if (and email (https-configured-p))
                                ;; HTTPS setup - show required environment variables
                                (progn
                                  (format t "  ACME_EMAIL=~A ./happening -u ~A~%" email url)
                                  (format t "~%For production Let's Encrypt certificates:~%")
                                  (format t "  ACME_EMAIL=~A ACME_PRODUCTION=true ./happening -u ~A~%" email url)
                                  (format t "~%TLS certificates will be obtained automatically on first start.~%"))
                                ;; HTTP-only setup
                                (format t "  ./happening -u ~A~%" url))
                            (format t "~%Then visit ~A to access your dashboard.~%" url))))

                      ;; Otherwise run TUI wizard
                      (progn
                        (if (needs-setup-p)
                            (if (run-setup-wizard)
                                (format t "Setup complete! Run './happening' to start the server.~%")
                                (format t "Setup cancelled.~%"))
                            (format t "Setup has already been completed.~%")))))))))

(defun make-app ()
  "Create and return the command-line application."
  (let ((port-opt (clingon:make-option
                   :integer
                   :short-name #\p
                   :long-name "port"
                   :key :port
                   :description "HTTP server port"
                   :initial-value 8080))
        (slynk-opt (clingon:make-option
                    :integer
                    :short-name #\s
                    :long-name "slynk-port"
                    :key :slynk-port
                    :description "Slynk server port for remote REPL"
                    :initial-value nil))
        (db-opt (clingon:make-option
                 :string
                 :short-name #\d
                 :long-name "database"
                 :key :database
                 :description "Path to SQLite database file"
                 :initial-value nil))
        (url-opt (clingon:make-option
                  :string
                  :short-name #\u
                  :long-name "url"
                  :key :url
                  :description "Public base URL for tracking snippets (e.g. https://analytics.example.com)"
                  :initial-value nil)))
    (clingon:make-command
     :name "happening"
     :version +version+
     :description "A privacy-focused, self-hosted web analytics platform"
     :authors (list "Anthony Green <green@moxielogic.com>")
     :license "MIT"
     :usage "[OPTIONS]"
     :options (list port-opt slynk-opt db-opt url-opt)
     :handler (lambda (cmd)
                (let ((port (clingon:getopt cmd :port))
                      (slynk-port (clingon:getopt cmd :slynk-port))
                      (db-path (clingon:getopt cmd :database))
                      (base-url (clingon:getopt cmd :url)))

                  ;; Load environment variables if .env exists
                  (let ((.env-pathname (merge-pathnames ".env")))
                    (handler-case
                        (.env:load-env .env-pathname)
                      (file-error (_)
                        (declare (ignore _)))
                      (.env:malformed-entry (_)
                        (declare (ignore _))
                        (fatal-error "Malformed entry in ~S" .env-pathname))
                      (.env:duplicated-entry (_)
                        (declare (ignore _))
                        (fatal-error "Duplicated entry in ~S" .env-pathname))))

                  ;; Initialize database
                  (llog:info "Initializing Happening analytics...")
                  (init-database db-path)

                  ;; Load GeoIP database if available
                  (load-geoip-database)

                  ;; Set public base URL for tracking snippets
                  ;; Priority: CLI option > database config
                  (setf *base-url* (or base-url
                                       (get-config "base_url")
                                       *base-url*))

                  ;; Run TUI setup wizard if needed
                  (when (and (needs-setup-p) (not (run-setup-wizard)))
                    (llog:info "Setup cancelled.")
                    (uiop:quit 0))

                  ;; Note: HTTPS certificate acquisition is now automatic via acme-acceptor
                  ;; when the server starts. No pre-check needed.

                  (bt:with-lock-held (*server-lock*)
                    ;; Start Slynk server if requested
                    (when slynk-port
                      (slynk:create-server :port slynk-port
                                           :interface "0.0.0.0"
                                           :dont-close t)
                      (llog:info (format nil "Started Slynk server on port ~A" slynk-port)))

                    ;; Start web server
                    (start-server port)

                    ;; Show startup message with actual URL
                    (if *base-url*
                        (llog:info (format nil "Dashboard available at ~A" *base-url*))
                        (llog:info (format nil "Dashboard available at http://localhost:~A" port)))

                    (llog:info "Waiting for connections...")

                    ;; Wait forever
                    (bt:condition-wait *shutdown-cv* *server-lock*))))

     :sub-commands (list (make-setup-command))
     :examples '(("Run on default port 8080:"
                  . "happening")
                 ("Run with public URL for tracking snippets:"
                  . "happening -u https://analytics.example.com")
                 ("Run on port 3000 with Slynk:"
                  . "happening -p 3000 -s 4005")
                 ("Use custom database location:"
                  . "happening -d /var/lib/happening/data.db")
                 ("Non-interactive setup:"
                  . "happening setup -a admin -P mypassword -u https://analytics.example.com -e admin@example.com")))))

(defun main ()
  "The main entrypoint."
  (handler-case
      (clingon:run (make-app))
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      (uiop:quit 1))))
