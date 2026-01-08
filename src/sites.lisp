;;; sites.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Site management for Happening analytics

(in-package #:happening)

;;; ----------------------------------------------------------------------------
;;; Site ID generation
;;; ----------------------------------------------------------------------------

(defparameter *nanoid-alphabet*
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  "Alphabet for nanoid-style IDs.")

(defparameter *site-id-length* 12
  "Length of site IDs.")

(defun generate-site-id ()
  "Generate a nanoid-style site ID using CSPRNG."
  (let* ((alphabet *nanoid-alphabet*)
         (alphabet-length (length alphabet))
         (random-bytes (ironclad:random-data *site-id-length*))
         (id (make-string *site-id-length*)))
    (dotimes (i *site-id-length*)
      (setf (char id i)
            (char alphabet (mod (aref random-bytes i) alphabet-length))))
    id))

;;; ----------------------------------------------------------------------------
;;; Site CRUD operations
;;; ----------------------------------------------------------------------------

(defun create-site (domain name)
  "Create a new site. Returns the site ID or NIL if domain already exists."
  (when (fetch-one "SELECT id FROM sites WHERE domain = ?" domain)
    (return-from create-site nil))
  (let ((id (generate-site-id))
        (now (unix-timestamp)))
    (execute-sql
     "INSERT INTO sites (id, domain, name, created_at, settings) VALUES (?, ?, ?, ?, ?)"
     id domain name now "{}")
    id))

(defun get-site (id)
  "Get a site by ID."
  (fetch-one "SELECT * FROM sites WHERE id = ?" id))

(defun get-site-by-domain (domain)
  "Get a site by domain."
  (fetch-one "SELECT * FROM sites WHERE domain = ?" domain))

(defun list-sites ()
  "List all sites."
  (fetch-all "SELECT * FROM sites ORDER BY created_at DESC"))

(defun update-site (id &key name settings)
  "Update a site's name and/or settings."
  (let ((site (get-site id)))
    (unless site
      (return-from update-site nil))
    (when name
      (execute-sql "UPDATE sites SET name = ? WHERE id = ?" name id))
    (when settings
      (execute-sql "UPDATE sites SET settings = ? WHERE id = ?" settings id))
    t))

(defun delete-site (id)
  "Delete a site and all its events."
  ;; Delete events first (foreign key)
  (execute-sql "DELETE FROM events WHERE site_id = ?" id)
  (execute-sql "DELETE FROM daily_stats WHERE site_id = ?" id)
  ;; Then delete the site
  (execute-sql "DELETE FROM sites WHERE id = ?" id)
  t)

(defun site-count ()
  "Return the total number of sites."
  (let ((result (fetch-one "SELECT COUNT(*) as count FROM sites")))
    (getf result :|count|)))

(defun has-sites-p ()
  "Check if any sites exist in the system."
  (> (site-count) 0))

;;; ----------------------------------------------------------------------------
;;; Tracking snippet generation
;;; ----------------------------------------------------------------------------

(defvar *base-url* nil
  "Public base URL for tracking snippets. Set via --url option.")

(defun get-tracking-snippet (site-id)
  "Generate the tracking snippet HTML for a site."
  (let ((base-url (or *base-url* "")))
    (format nil
            "<script>
window.HAPPENING_SITE_ID='~A';
</script>
<script src=\"~A/js/tracker.js\" defer></script>"
            site-id base-url)))
