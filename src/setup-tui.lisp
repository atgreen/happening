;;; setup-tui.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; TUI setup wizard for Happening analytics

(in-package #:happening)

;;; ----------------------------------------------------------------------------
;;; Setup wizard model
;;; ----------------------------------------------------------------------------

(defclass setup-model ()
  ((step :initform 0 :accessor setup-step)
   (username-input :accessor setup-username-input)
   (password-input :accessor setup-password-input)
   (confirm-input :accessor setup-confirm-input)
   (url-input :accessor setup-url-input)
   (error-message :initform nil :accessor setup-error-message)
   (complete :initform nil :accessor setup-complete))
  (:documentation "Model for the setup wizard."))

(defun get-public-ip ()
  "Fetch public IP using curl. Returns nil on failure."
  (handler-case
      (let ((output (uiop:run-program '("curl" "-s" "--connect-timeout" "3" "http://icanhazip.com")
                                      :output :string
                                      :ignore-error-status t)))
        (when (and output (> (length output) 0))
          (string-trim '(#\Space #\Newline #\Return #\Tab) output)))
    (error () nil)))

(defun make-setup-model ()
  "Create a new setup wizard model."
  (let ((model (make-instance 'setup-model))
        (public-ip (get-public-ip)))
    (setf (setup-username-input model)
          (tui.textinput:make-textinput
           :placeholder "admin"
           :prompt ""
           :width 50))
    (setf (setup-password-input model)
          (tui.textinput:make-textinput
           :placeholder "minimum 8 characters"
           :prompt ""
           :width 50
           :echo-mode :password))
    (setf (setup-confirm-input model)
          (tui.textinput:make-textinput
           :placeholder "repeat password"
           :prompt ""
           :width 50
           :echo-mode :password))
    (setf (setup-url-input model)
          (tui.textinput:make-textinput
           :value (if public-ip
                      (format nil "https://~A" public-ip)
                      "")
           :placeholder "https://your-server.com"
           :prompt ""
           :width 50))
    model))

(defun current-input (model)
  "Get the currently active input based on step."
  (case (setup-step model)
    (0 (setup-username-input model))
    (1 (setup-password-input model))
    (2 (setup-confirm-input model))
    (3 (setup-url-input model))
    (otherwise nil)))

(defun focus-current-input (model)
  "Focus the current input and blur others."
  (dolist (input (list (setup-username-input model)
                       (setup-password-input model)
                       (setup-confirm-input model)
                       (setup-url-input model)))
    (tui.textinput:textinput-blur input))
  (alexandria:when-let ((input (current-input model)))
    (tui.textinput:textinput-focus input)))

;;; ----------------------------------------------------------------------------
;;; TEA implementation
;;; ----------------------------------------------------------------------------

(defmethod tui:init ((model setup-model))
  (focus-current-input model)
  nil)

(defmethod tui:update-message ((model setup-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Quit on Ctrl+C or Escape
      ((or (and (tui:key-msg-ctrl msg) (eql key #\c))
           (eql key :escape))
       (values model (tui:quit-cmd)))

      ;; Any key to exit after completion
      ((setup-complete model)
       (values model (tui:quit-cmd)))

      ;; Enter to advance
      ((eql key :enter)
       (setf (setup-error-message model) nil)
       (case (setup-step model)
         ;; Validate username
         (0 (let ((username (tui.textinput:textinput-value (setup-username-input model))))
              (cond
                ((< (length username) 1)
                 (setf (setup-error-message model) "Username is required"))
                (t (incf (setup-step model))
                   (focus-current-input model)))))
         ;; Validate password
         (1 (let ((password (tui.textinput:textinput-value (setup-password-input model))))
              (cond
                ((< (length password) 8)
                 (setf (setup-error-message model) "Password must be at least 8 characters"))
                (t (incf (setup-step model))
                   (focus-current-input model)))))
         ;; Validate password confirmation
         (2 (let ((password (tui.textinput:textinput-value (setup-password-input model)))
                  (confirm (tui.textinput:textinput-value (setup-confirm-input model))))
              (cond
                ((string/= password confirm)
                 (setf (setup-error-message model) "Passwords do not match"))
                (t (incf (setup-step model))
                   (focus-current-input model)))))
         ;; Validate URL and complete setup
         (3 (let ((url (tui.textinput:textinput-value (setup-url-input model))))
              (cond
                ((< (length url) 1)
                 (setf (setup-error-message model) "URL is required"))
                (t
                 ;; Create user and set base URL
                 (let* ((username (tui.textinput:textinput-value (setup-username-input model)))
                        (password (tui.textinput:textinput-value (setup-password-input model)))
                        (user-id (create-user username password)))
                   (if user-id
                       (progn
                         ;; Persist base URL to database and global
                         (setf *base-url* url)
                         (set-config "base_url" url)
                         (setf (setup-complete model) t))
                       (setf (setup-error-message model) "Failed to create user"))))))))
       (values model nil))

      ;; Tab to advance to next field (without validation)
      ((and (eql key :tab) (< (setup-step model) 3))
       (incf (setup-step model))
       (focus-current-input model)
       (values model nil))

      ;; Shift+Tab to go back
      ((and (eql key :backtab) (> (setup-step model) 0))
       (decf (setup-step model))
       (focus-current-input model)
       (values model nil))

      ;; Pass key to current input
      (t
       (alexandria:when-let ((input (current-input model)))
         (tui.textinput:textinput-update input msg))
       (values model nil)))))

(defmethod tui:view ((model setup-model))
  (if (setup-complete model)
      ;; Show completion screen
      (tui:join-vertical
       tui:+left+
       ""
       (tui:bold (tui:colored "  Setup Complete!" :fg tui:*fg-green*))
       ""
       "  Admin account created successfully."
       ""
       "  Press any key to start the server...")

      ;; Show setup form
      (let* ((step (setup-step model))
             (title (tui:bold "  Happening Setup"))
             (subtitle "  Configure your analytics server.")
             (separator "")
             (username-label (if (= step 0)
                                 (tui:bold "  > Username:")
                                 "    Username:"))
             (username-field (format nil "    [~A]"
                                     (let ((val (tui.textinput:textinput-value (setup-username-input model))))
                                       (if (> (length val) 0) val "________________"))))
             (password-label (if (= step 1)
                                 (tui:bold "  > Password:")
                                 "    Password:"))
             (password-field (format nil "    [~A]"
                                     (let ((val (tui.textinput:textinput-value (setup-password-input model))))
                                       (if (> (length val) 0)
                                           (make-string (length val) :initial-element #\*)
                                           "________________"))))
             (confirm-label (if (= step 2)
                                (tui:bold "  > Confirm Password:")
                                "    Confirm Password:"))
             (confirm-field (format nil "    [~A]"
                                    (let ((val (tui.textinput:textinput-value (setup-confirm-input model))))
                                      (if (> (length val) 0)
                                          (make-string (length val) :initial-element #\*)
                                          "________________"))))
             (url-label (if (= step 3)
                            (tui:bold "  > Public URL:")
                            "    Public URL:"))
             (url-field (format nil "    [~A]"
                                (let ((val (tui.textinput:textinput-value (setup-url-input model))))
                                  (if (> (length val) 0) val "____________________________"))))
             (error-line (if (setup-error-message model)
                             (tui:colored (format nil "  Error: ~A" (setup-error-message model)) :fg tui:*fg-red*)
                             ""))
             (input-line (alexandria:when-let ((input (current-input model)))
                           (format nil "    ~A" (tui.textinput:textinput-view input))))
             (help-line "  [Enter] Continue  [Tab] Next  [Shift+Tab] Back  [Esc] Quit"))
        (tui:join-vertical
         tui:+left+
         ""
         title
         subtitle
         separator
         username-label
         (if (= step 0) input-line username-field)
         ""
         password-label
         (if (= step 1) input-line password-field)
         ""
         confirm-label
         (if (= step 2) input-line confirm-field)
         ""
         url-label
         (if (= step 3) input-line url-field)
         ""
         error-line
         ""
         help-line))))

;;; ----------------------------------------------------------------------------
;;; Entry point
;;; ----------------------------------------------------------------------------

(defun run-setup-wizard ()
  "Run the TUI setup wizard. Returns T if setup completed, NIL if cancelled."
  (let ((model (make-setup-model)))
    (tui:run (tui:make-program model :alt-screen t))
    (setup-complete model)))
