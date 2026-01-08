;;; setup.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; First-run setup wizard for Happening analytics

(in-package #:happening)

;;; ----------------------------------------------------------------------------
;;; Setup state checks
;;; ----------------------------------------------------------------------------

(defun needs-setup-p ()
  "Check if the system needs initial setup (no users exist)."
  (not (has-users-p)))

;;; ----------------------------------------------------------------------------
;;; Setup HTML pages
;;; ----------------------------------------------------------------------------

(defun render-setup-page (&optional error-message)
  "Render the initial setup page."
  (render-base-page
   "Setup - Happening"
   (cl-who:with-html-output-to-string (s)
     (:div :class "setup-container"
           (:div :class "setup-card"
                 (:h1 "Welcome to Happening")
                 (:p "Let's set up your analytics platform.")

                 (when error-message
                   (cl-who:htm
                    (:div :class "error-message"
                          (cl-who:str (html-escape error-message)))))

                 (:form :method "POST" :action "/setup"
                        (cl-who:fmt "~A" (csrf-token-field))
                        (:h2 "Create Admin Account")
                        (:div :class "form-group"
                              (:label :for "username" "Username")
                              (:input :type "text" :id "username" :name "username"
                                      :required t :autofocus t
                                      :placeholder "admin"))

                        (:div :class "form-group"
                              (:label :for "password" "Password")
                              (:input :type "password" :id "password" :name "password"
                                      :required t :minlength "8"
                                      :placeholder "At least 8 characters"))

                        (:div :class "form-group"
                              (:label :for "password_confirm" "Confirm Password")
                              (:input :type "password" :id "password_confirm" :name "password_confirm"
                                      :required t
                                      :placeholder "Repeat password"))

                        (:h2 "Create First Site")
                        (:div :class "form-group"
                              (:label :for "domain" "Website Domain")
                              (:input :type "text" :id "domain" :name "domain"
                                      :required t
                                      :placeholder "example.com"))

                        (:div :class "form-group"
                              (:label :for "site_name" "Site Name")
                              (:input :type "text" :id "site_name" :name "site_name"
                                      :required t
                                      :placeholder "My Website"))

                        (:button :type "submit" :class "btn-primary"
                                 "Complete Setup")))))))

(defun render-setup-complete-page (site-id tracking-snippet)
  "Render the setup complete page with tracking snippet."
  (declare (ignore site-id))
  (render-base-page
   "Setup Complete - Happening"
   (cl-who:with-html-output-to-string (s)
     (:div :class "setup-container"
           (:div :class "setup-card"
                 (:h1 "Setup Complete!")
                 (:p "Your Happening analytics platform is ready to use.")

                 (:h2 "Install the Tracking Script")
                 (:p "Add this code to your website, just before the closing </head> tag:")

                 (:div :class "code-block"
                       (:pre
                        (:code (cl-who:str (html-escape tracking-snippet)))))

                 (:h2 "What's Next?")
                 (:ul
                  (:li "Add the tracking script to your website")
                  (:li "Visit your site to generate some test data")
                  (:li "Check back here to see your analytics"))

                 (:a :href "/dashboard" :class "btn-primary"
                     "Go to Dashboard"))))))

;;; ----------------------------------------------------------------------------
;;; Setup form processing
;;; ----------------------------------------------------------------------------

(defun validate-setup-form (username password password-confirm domain site-name)
  "Validate the setup form. Returns an error message or NIL if valid."
  (cond
    ((or (null username) (string= username ""))
     "Username is required")
    ((< (length username) 3)
     "Username must be at least 3 characters")
    ((or (null password) (string= password ""))
     "Password is required")
    ((< (length password) 8)
     "Password must be at least 8 characters")
    ((not (string= password password-confirm))
     "Passwords do not match")
    ((or (null domain) (string= domain ""))
     "Website domain is required")
    ((or (null site-name) (string= site-name ""))
     "Site name is required")
    (t nil)))

(defun process-setup-form (username password password-confirm domain site-name)
  "Process the setup form. Returns (values success-p result).
   On success, result is the site-id.
   On failure, result is an error message."
  ;; Validate input
  (let ((error (validate-setup-form username password password-confirm domain site-name)))
    (when error
      (return-from process-setup-form (values nil error))))

  ;; Check if setup is still needed
  (unless (needs-setup-p)
    (return-from process-setup-form
      (values nil "Setup has already been completed")))

  ;; Create admin user
  (let ((user-id (create-user username password)))
    (unless user-id
      (return-from process-setup-form
        (values nil "Failed to create user (username may already exist)"))))

  ;; Create first site
  (let ((site-id (create-site domain site-name)))
    (unless site-id
      (return-from process-setup-form
        (values nil "Failed to create site (domain may already exist)")))
    (values t site-id)))

;;; ----------------------------------------------------------------------------
;;; Login page
;;; ----------------------------------------------------------------------------

(defun render-login-page (&optional error-message)
  "Render the login page."
  (render-base-page
   "Login - Happening"
   (cl-who:with-html-output-to-string (s)
     (:div :class "login-container"
           (:div :class "login-card"
                 (:h1 "Happening")
                 (:p "Sign in to your analytics dashboard")

                 (when error-message
                   (cl-who:htm
                    (:div :class "error-message"
                          (cl-who:str (html-escape error-message)))))

                 (:form :method "POST" :action "/login"
                        (cl-who:fmt "~A" (csrf-token-field))
                        (:div :class "form-group"
                              (:label :for "username" "Username")
                              (:input :type "text" :id "username" :name "username"
                                      :required t :autofocus t))

                        (:div :class "form-group"
                              (:label :for "password" "Password")
                              (:input :type "password" :id "password" :name "password"
                                      :required t))

                        (:button :type "submit" :class "btn-primary"
                                 "Sign In")))))))

;;; ----------------------------------------------------------------------------
;;; Sites list page
;;; ----------------------------------------------------------------------------

(defun render-sites-page ()
  "Render the sites list page."
  (let ((sites (list-sites)))
    (render-base-page
     "Sites - Happening"
     (cl-who:with-html-output-to-string (s)
       (:div :class "sites-container"
             (:header :class "page-header"
                      (:h1 "Your Sites")
                      (:a :href "/sites/new" :class "btn-primary" "Add Site"))

             (:div :class "sites-list"
                   (if sites
                       (loop for site in sites
                             do (cl-who:htm
                                 (:div :class "site-card"
                                       (:h3 (cl-who:str (html-escape (getf site :|name|))))
                                       (:p :class "site-domain"
                                           (cl-who:str (html-escape (getf site :|domain|))))
                                       (:div :class "site-actions"
                                             (:a :href (format nil "/dashboard/~A" (getf site :|id|))
                                                 :class "btn-secondary"
                                                 "View Dashboard")
                                             (:a :href (format nil "/sites/~A" (getf site :|id|))
                                                 :class "btn-secondary"
                                                 "Settings")))))
                       (cl-who:htm
                        (:p :class "empty-state"
                            "No sites yet. "
                            (:a :href "/sites/new" "Add your first site"))))))))))

(defun render-new-site-page (&optional error-message)
  "Render the new site form."
  (render-base-page
   "Add Site - Happening"
   (cl-who:with-html-output-to-string (s)
     (:div :class "setup-container"
           (:div :class "setup-card"
                 (:h1 "Add a New Site")

                 (when error-message
                   (cl-who:htm
                    (:div :class "error-message"
                          (cl-who:str (html-escape error-message)))))

                 (:form :method "POST" :action "/sites/new"
                        (cl-who:fmt "~A" (csrf-token-field))
                        (:div :class "form-group"
                              (:label :for "domain" "Website Domain")
                              (:input :type "text" :id "domain" :name "domain"
                                      :required t :autofocus t
                                      :placeholder "example.com"))

                        (:div :class "form-group"
                              (:label :for "site_name" "Site Name")
                              (:input :type "text" :id "site_name" :name "site_name"
                                      :required t
                                      :placeholder "My Website"))

                        (:div :class "form-actions"
                              (:a :href "/sites" :class "btn-secondary" "Cancel")
                              (:button :type "submit" :class "btn-primary"
                                       "Add Site"))))))))

(defun render-site-details-page (site)
  "Render the site details page with tracking snippet."
  (let* ((site-id (getf site :|id|))
         (site-name (getf site :|name|))
         (site-domain (getf site :|domain|))
         (snippet (get-tracking-snippet site-id)))
    (render-base-page
     (format nil "~A - Happening" site-name)
     (cl-who:with-html-output-to-string (s)
       (:div :class "setup-container"
             (:div :class "setup-card"
                   (:h1 (cl-who:str (html-escape site-name)))
                   (:p :class "site-domain"
                       (cl-who:str (html-escape site-domain)))

                   (:h2 "Tracking Script")
                   (:p "Add this code to your website, just before the closing </head> tag:")

                   (:div :class "code-block"
                         (:pre
                          (:code (cl-who:str (html-escape snippet)))))

                   (:div :class "form-actions"
                         (:a :href "/sites" :class "btn-secondary" "Back to Sites")
                         (:a :href (format nil "/dashboard/~A" site-id)
                             :class "btn-primary" "View Dashboard"))))))))
