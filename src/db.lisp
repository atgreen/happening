;;; db.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Database layer for Happening analytics

(in-package #:happening)

;;; ----------------------------------------------------------------------------
;;; Database connection management
;;; ----------------------------------------------------------------------------

(defvar *db* nil
  "The database connection.")

(defvar *db-path* nil
  "Path to the SQLite database file.")

(defun db-path ()
  "Return the database path, defaulting to ./data/happening.db"
  (or *db-path*
      (let ((path (merge-pathnames "data/happening.db" (uiop:getcwd))))
        (ensure-directories-exist path)
        path)))

(defun init-database (&optional path)
  "Initialize the database connection with WAL mode."
  (when path
    (setf *db-path* path))
  (let ((db-file (db-path)))
    (llog:info (format nil "Initializing database at ~A" db-file))
    (ensure-directories-exist db-file)
    (setf *db* (dbi:connect :sqlite3 :database-name (namestring db-file)))
    ;; Enable WAL mode for better concurrent access
    (dbi:do-sql *db* "PRAGMA journal_mode=WAL")
    (dbi:do-sql *db* "PRAGMA foreign_keys=ON")
    (dbi:do-sql *db* "PRAGMA synchronous=NORMAL")
    (llog:info "Database initialized with WAL mode")
    (ensure-schema)
    *db*))

(defun close-database ()
  "Close the database connection."
  (when *db*
    (dbi:disconnect *db*)
    (setf *db* nil)))

(defun ensure-connection ()
  "Ensure we have an active database connection."
  (unless *db*
    (error "Database not initialized. Call init-database first.")))

;;; ----------------------------------------------------------------------------
;;; Schema management
;;; ----------------------------------------------------------------------------

(defparameter *schema-version* 1
  "Current schema version.")

(defun table-exists-p (table-name)
  "Check if a table exists in the database."
  (ensure-connection)
  (let ((result (dbi:fetch
                 (dbi:execute
                  (dbi:prepare *db*
                               "SELECT name FROM sqlite_master WHERE type='table' AND name=?")
                  (list table-name)))))
    (not (null result))))

(defun ensure-schema ()
  "Create database tables if they don't exist."
  (ensure-connection)
  (llog:info "Ensuring database schema...")

  ;; Schema version table
  (dbi:do-sql *db* "
    CREATE TABLE IF NOT EXISTS schema_version (
      version INTEGER PRIMARY KEY
    )")

  ;; Sites table
  (dbi:do-sql *db* "
    CREATE TABLE IF NOT EXISTS sites (
      id TEXT PRIMARY KEY,
      domain TEXT UNIQUE NOT NULL,
      name TEXT NOT NULL,
      created_at INTEGER NOT NULL,
      settings TEXT DEFAULT '{}'
    )")

  ;; Events table
  (dbi:do-sql *db* "
    CREATE TABLE IF NOT EXISTS events (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      site_id TEXT NOT NULL REFERENCES sites(id),
      timestamp INTEGER NOT NULL,
      visitor_hash TEXT NOT NULL,
      session_id TEXT NOT NULL,
      url TEXT NOT NULL,
      referrer TEXT,
      device_type TEXT,
      browser TEXT,
      os TEXT,
      country TEXT,
      viewport_width INTEGER
    )")

  ;; Events indexes
  (dbi:do-sql *db* "
    CREATE INDEX IF NOT EXISTS idx_events_site_time
    ON events(site_id, timestamp)")
  (dbi:do-sql *db* "
    CREATE INDEX IF NOT EXISTS idx_events_session
    ON events(site_id, session_id)")
  (dbi:do-sql *db* "
    CREATE INDEX IF NOT EXISTS idx_events_visitor
    ON events(site_id, visitor_hash, timestamp)")

  ;; Users table
  (dbi:do-sql *db* "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL,
      created_at INTEGER NOT NULL,
      last_login INTEGER
    )")

  ;; Sessions table (for auth sessions, not analytics sessions)
  (dbi:do-sql *db* "
    CREATE TABLE IF NOT EXISTS auth_sessions (
      token TEXT PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id),
      created_at INTEGER NOT NULL,
      expires_at INTEGER NOT NULL
    )")
  (dbi:do-sql *db* "
    CREATE INDEX IF NOT EXISTS idx_auth_sessions_expires
    ON auth_sessions(expires_at)")

  ;; Daily stats table (for pre-aggregation)
  (dbi:do-sql *db* "
    CREATE TABLE IF NOT EXISTS daily_stats (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      site_id TEXT NOT NULL REFERENCES sites(id),
      date TEXT NOT NULL,
      page_views INTEGER DEFAULT 0,
      unique_visitors INTEGER DEFAULT 0,
      sessions INTEGER DEFAULT 0,
      bounces INTEGER DEFAULT 0,
      total_duration INTEGER DEFAULT 0,
      page_stats TEXT DEFAULT '{}',
      UNIQUE(site_id, date)
    )")

  ;; Config table for persistent settings
  (dbi:do-sql *db* "
    CREATE TABLE IF NOT EXISTS config (
      key TEXT PRIMARY KEY,
      value TEXT NOT NULL
    )")

  (llog:info "Database schema ready"))

;;; ----------------------------------------------------------------------------
;;; Config helpers
;;; ----------------------------------------------------------------------------

(defun get-config (key &optional default)
  "Get a configuration value by key."
  (let ((result (fetch-one "SELECT value FROM config WHERE key = ?" key)))
    (if result
        (getf result :|value|)
        default)))

(defun set-config (key value)
  "Set a configuration value."
  (execute-sql
   "INSERT OR REPLACE INTO config (key, value) VALUES (?, ?)"
   key value))

;;; ----------------------------------------------------------------------------
;;; Query helpers
;;; ----------------------------------------------------------------------------

(defmacro with-query ((var query &rest params) &body body)
  "Execute a query and bind the result cursor to VAR."
  `(progn
     (ensure-connection)
     (let ((,var (dbi:execute
                  (dbi:prepare *db* ,query)
                  (list ,@params))))
       ,@body)))

(defun fetch-one (query &rest params)
  "Execute a query and return the first row as a plist."
  (ensure-connection)
  (dbi:fetch
   (dbi:execute
    (dbi:prepare *db* query)
    params)))

(defun fetch-all (query &rest params)
  "Execute a query and return all rows as a list of plists."
  (ensure-connection)
  (dbi:fetch-all
   (dbi:execute
    (dbi:prepare *db* query)
    params)))

(defun execute-sql (query &rest params)
  "Execute a SQL statement (INSERT, UPDATE, DELETE)."
  (ensure-connection)
  (dbi:execute
   (dbi:prepare *db* query)
   params))

(defun last-insert-id ()
  "Get the last inserted row ID."
  (ensure-connection)
  (getf (dbi:fetch
         (dbi:execute
          (dbi:prepare *db* "SELECT last_insert_rowid() as id")
          nil))
        :|id|))

;;; ----------------------------------------------------------------------------
;;; Utility functions
;;; ----------------------------------------------------------------------------

(defun unix-timestamp ()
  "Return the current Unix timestamp in seconds."
  (local-time:timestamp-to-unix (local-time:now)))

(defun unix-timestamp-ms ()
  "Return the current Unix timestamp in milliseconds."
  (* 1000 (unix-timestamp)))

(defun generate-id ()
  "Generate a unique ID string."
  (string-downcase (format nil "~A" (uuid:make-v4-uuid))))
