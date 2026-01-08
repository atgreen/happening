;;; auth.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Authentication system for Happening analytics

(in-package #:happening)

;;; ----------------------------------------------------------------------------
;;; Password hashing (using PBKDF2 via Ironclad)
;;; ----------------------------------------------------------------------------

(defparameter *pbkdf2-iterations* 100000
  "Number of PBKDF2 iterations for password hashing.")

(defparameter *salt-length* 16
  "Length of random salt in bytes.")

(defun generate-random-bytes (length)
  "Generate LENGTH cryptographically secure random bytes.
   Uses Ironclad's OS-seeded CSPRNG for security-sensitive randomness."
  (ironclad:random-data length))

(defun bytes-to-hex (bytes)
  "Convert a byte vector to a hexadecimal string."
  (with-output-to-string (s)
    (loop for byte across bytes
          do (format s "~2,'0x" byte))))

(defun hex-to-bytes (hex-string)
  "Convert a hexadecimal string to a byte vector."
  (let* ((len (/ (length hex-string) 2))
         (bytes (make-array len :element-type '(unsigned-byte 8))))
    (loop for i from 0 below len
          for j from 0 by 2
          do (setf (aref bytes i)
                   (parse-integer hex-string :start j :end (+ j 2) :radix 16)))
    bytes))

(defun hash-password (password)
  "Hash a password using PBKDF2-SHA256. Returns a string of format: salt$hash"
  (let* ((salt (generate-random-bytes *salt-length*))
         (password-bytes (ironclad:ascii-string-to-byte-array password))
         (derived-key (ironclad:derive-key
                       (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)
                       password-bytes
                       salt
                       *pbkdf2-iterations*
                       32)))
    (format nil "~A$~A" (bytes-to-hex salt) (bytes-to-hex derived-key))))

(defun verify-password (password stored-hash)
  "Verify a password against a stored hash. Returns T if valid, NIL otherwise."
  (let* ((parts (cl-ppcre:split "\\$" stored-hash))
         (salt (hex-to-bytes (first parts)))
         (expected-hash (hex-to-bytes (second parts)))
         (password-bytes (ironclad:ascii-string-to-byte-array password))
         (derived-key (ironclad:derive-key
                       (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)
                       password-bytes
                       salt
                       *pbkdf2-iterations*
                       32)))
    (equalp derived-key expected-hash)))

;;; ----------------------------------------------------------------------------
;;; Session management
;;; ----------------------------------------------------------------------------

(defparameter *session-duration* (* 24 60 60)
  "Session duration in seconds (default: 24 hours).")

(defparameter *session-cookie-name* "happening_session"
  "Name of the session cookie.")

(defun generate-session-token ()
  "Generate a secure random session token."
  (bytes-to-hex (generate-random-bytes 32)))

(defun create-session (user-id)
  "Create a new session for a user. Returns the session token."
  (let ((token (generate-session-token))
        (now (unix-timestamp))
        (expires (+ (unix-timestamp) *session-duration*)))
    (execute-sql
     "INSERT INTO auth_sessions (token, user_id, created_at, expires_at) VALUES (?, ?, ?, ?)"
     token user-id now expires)
    token))

(defun validate-session (token)
  "Validate a session token. Returns user info if valid, NIL otherwise."
  (when token
    (let ((session (fetch-one
                    "SELECT s.user_id, s.expires_at, u.username
                     FROM auth_sessions s
                     JOIN users u ON s.user_id = u.id
                     WHERE s.token = ?"
                    token)))
      (when (and session
                 (> (getf session :|expires_at|) (unix-timestamp)))
        session))))

(defun delete-session (token)
  "Delete a session by token."
  (when token
    (execute-sql "DELETE FROM auth_sessions WHERE token = ?" token)))

(defun cleanup-expired-sessions ()
  "Remove all expired sessions from the database."
  (execute-sql "DELETE FROM auth_sessions WHERE expires_at < ?" (unix-timestamp)))

;;; ----------------------------------------------------------------------------
;;; User management
;;; ----------------------------------------------------------------------------

(defun create-user (username password)
  "Create a new user. Returns the user ID or NIL if username already exists."
  (when (fetch-one "SELECT id FROM users WHERE username = ?" username)
    (return-from create-user nil))
  (let ((password-hash (hash-password password))
        (now (unix-timestamp)))
    (execute-sql
     "INSERT INTO users (username, password_hash, created_at) VALUES (?, ?, ?)"
     username password-hash now)
    (last-insert-id)))

(defun authenticate-user (username password)
  "Authenticate a user by username and password.
   Returns user info if valid, NIL otherwise."
  (let ((user (fetch-one
               "SELECT id, username, password_hash FROM users WHERE username = ?"
               username)))
    (when (and user (verify-password password (getf user :|password_hash|)))
      ;; Update last login time
      (execute-sql "UPDATE users SET last_login = ? WHERE id = ?"
                   (unix-timestamp) (getf user :|id|))
      user)))

(defun get-user-by-id (user-id)
  "Get user info by ID."
  (fetch-one "SELECT id, username, created_at, last_login FROM users WHERE id = ?"
             user-id))

(defun user-count ()
  "Return the total number of users in the system."
  (let ((result (fetch-one "SELECT COUNT(*) as count FROM users")))
    (getf result :|count|)))

(defun has-users-p ()
  "Check if any users exist in the system."
  (> (user-count) 0))

;;; ----------------------------------------------------------------------------
;;; Request helpers
;;; ----------------------------------------------------------------------------

(defun get-session-cookie ()
  "Get the session token from the request cookie."
  (hunchentoot:cookie-in *session-cookie-name*))

(defun set-session-cookie (token)
  "Set the session cookie in the response."
  (hunchentoot:set-cookie *session-cookie-name*
                          :value token
                          :path "/"
                          :http-only t
                          :secure (hunchentoot:ssl-p)
                          :same-site :lax
                          :max-age *session-duration*))

(defun clear-session-cookie ()
  "Clear the session cookie."
  (hunchentoot:set-cookie *session-cookie-name*
                          :value ""
                          :path "/"
                          :max-age 0))

(defun current-user ()
  "Get the currently authenticated user, or NIL if not authenticated."
  (alexandria:when-let ((token (get-session-cookie)))
    (validate-session token)))

(defun authenticated-p ()
  "Check if the current request is authenticated."
  (not (null (current-user))))

;;; ----------------------------------------------------------------------------
;;; Auth middleware / route protection
;;; ----------------------------------------------------------------------------

(defun require-auth ()
  "Redirect to login if not authenticated and abort request processing.
   Call this at the start of protected routes."
  (unless (authenticated-p)
    (hunchentoot:redirect "/login")
    (hunchentoot:abort-request-handler)))

(defun require-setup ()
  "Redirect to setup if no users exist and abort request processing.
   Call this at the start of any route."
  (unless (has-users-p)
    (hunchentoot:redirect "/setup")
    (hunchentoot:abort-request-handler)))

;;; ----------------------------------------------------------------------------
;;; CSRF Protection
;;; ----------------------------------------------------------------------------

(defparameter *csrf-cookie-name* "happening_csrf"
  "Name of the CSRF token cookie.")

(defparameter *csrf-form-field* "_csrf"
  "Name of the hidden form field for CSRF token.")

(defun generate-csrf-token ()
  "Generate a new CSRF token."
  (bytes-to-hex (generate-random-bytes 32)))

(defun get-csrf-token ()
  "Get the CSRF token for the current request, creating one if needed.
   Sets the token in a cookie and returns it for embedding in forms."
  (let ((existing (hunchentoot:cookie-in *csrf-cookie-name*)))
    (if (and existing (> (length existing) 0))
        existing
        (let ((token (generate-csrf-token)))
          (hunchentoot:set-cookie *csrf-cookie-name*
                                  :value token
                                  :path "/"
                                  :http-only t
                                  :secure (hunchentoot:ssl-p)
                                  :same-site :strict)
          token))))

(defun csrf-token-field ()
  "Return HTML for a hidden CSRF token field. Call this inside forms."
  (format nil "<input type=\"hidden\" name=\"~A\" value=\"~A\">"
          *csrf-form-field* (get-csrf-token)))

(defun valid-csrf-token-p ()
  "Check if the CSRF token in the POST request matches the cookie.
   Returns T if valid, NIL otherwise."
  (let ((cookie-token (hunchentoot:cookie-in *csrf-cookie-name*))
        (form-token (hunchentoot:post-parameter *csrf-form-field*)))
    (and cookie-token
         form-token
         (> (length cookie-token) 0)
         (> (length form-token) 0)
         (string= cookie-token form-token))))

(defun require-valid-csrf ()
  "Require a valid CSRF token for POST requests.
   Returns 403 Forbidden and aborts if invalid."
  (unless (valid-csrf-token-p)
    (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
    (hunchentoot:abort-request-handler)))
