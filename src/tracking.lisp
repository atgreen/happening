;;; tracking.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Event tracking and ingestion for Happening analytics

(in-package #:happening)

;;; ----------------------------------------------------------------------------
;;; Configuration
;;; ----------------------------------------------------------------------------

(defparameter *trust-proxy* nil
  "When non-NIL, trust X-Forwarded-For header for client IP.
   Can be:
   - T to trust unconditionally (only use behind a reverse proxy you control)
   - A list of trusted proxy IP addresses (e.g., '(\"127.0.0.1\" \"10.0.0.1\"))
   Set via TRUST_PROXY environment variable (\"true\" or comma-separated IPs).")

(defparameter *timestamp-max-drift-ms* (* 5 60 1000)
  "Maximum allowed drift (in milliseconds) between client and server timestamps.
   Events with timestamps outside this window are clamped to server time.
   Default: 5 minutes.")

;;; ----------------------------------------------------------------------------
;;; Daily salt for visitor hashing (privacy protection)
;;; ----------------------------------------------------------------------------

(defvar *daily-salt* nil
  "Daily salt for visitor hash generation. Rotates each day.")

(defvar *daily-salt-date* nil
  "Date when the current daily salt was generated.")

(defun get-daily-salt ()
  "Get the daily salt, generating a new one if the day has changed."
  (let ((today (local-time:format-timestring nil (local-time:now)
                                              :format '(:year "-" (:month 2) "-" (:day 2)))))
    (when (or (null *daily-salt*)
              (not (string= today *daily-salt-date*)))
      (setf *daily-salt* (bytes-to-hex (generate-random-bytes 32)))
      (setf *daily-salt-date* today))
    *daily-salt*))

;;; ----------------------------------------------------------------------------
;;; Visitor and session identification
;;; ----------------------------------------------------------------------------

(defun generate-visitor-hash (ip user-agent)
  "Generate a privacy-preserving visitor hash.
   Uses SHA256 of IP + User-Agent + daily salt.
   The daily salt rotation ensures visitors can't be tracked across days."
  (let* ((data (format nil "~A|~A|~A" ip user-agent (get-daily-salt)))
         (digest (ironclad:digest-sequence
                  :sha256
                  (ironclad:ascii-string-to-byte-array data))))
    (bytes-to-hex digest)))

(defparameter *session-timeout* (* 30 60)
  "Session timeout in seconds (default: 30 minutes).")

(defun generate-session-id (visitor-hash timestamp)
  "Generate a session ID based on visitor hash and timestamp window.
   Sessions are grouped into 30-minute windows."
  (let* ((window (floor timestamp (* *session-timeout* 1000)))
         (data (format nil "~A|~A" visitor-hash window))
         (digest (ironclad:digest-sequence
                  :sha256
                  (ironclad:ascii-string-to-byte-array data))))
    (subseq (bytes-to-hex digest) 0 16)))

;;; ----------------------------------------------------------------------------
;;; User-Agent parsing
;;; ----------------------------------------------------------------------------

(defun parse-device-type (user-agent)
  "Parse device type from User-Agent string."
  (cond
    ((or (cl-ppcre:scan "(?i)mobile|android|iphone|ipod|blackberry|windows phone" user-agent)
         (and (cl-ppcre:scan "(?i)android" user-agent)
              (not (cl-ppcre:scan "(?i)tablet" user-agent))))
     "mobile")
    ((cl-ppcre:scan "(?i)tablet|ipad|kindle|playbook" user-agent)
     "tablet")
    (t "desktop")))

(defun parse-browser (user-agent)
  "Parse browser name from User-Agent string."
  (cond
    ((cl-ppcre:scan "(?i)edg/" user-agent) "Edge")
    ((cl-ppcre:scan "(?i)opr/|opera" user-agent) "Opera")
    ((cl-ppcre:scan "(?i)chrome/" user-agent) "Chrome")
    ((cl-ppcre:scan "(?i)firefox/" user-agent) "Firefox")
    ((cl-ppcre:scan "(?i)safari/" user-agent)
     (if (cl-ppcre:scan "(?i)chrome/" user-agent)
         "Chrome"
         "Safari"))
    ((cl-ppcre:scan "(?i)msie|trident/" user-agent) "IE")
    (t "Other")))

(defun parse-os (user-agent)
  "Parse operating system from User-Agent string."
  (cond
    ((cl-ppcre:scan "(?i)windows nt 10" user-agent) "Windows")
    ((cl-ppcre:scan "(?i)windows nt" user-agent) "Windows")
    ((cl-ppcre:scan "(?i)mac os x" user-agent) "macOS")
    ((cl-ppcre:scan "(?i)iphone|ipad|ipod" user-agent) "iOS")
    ((cl-ppcre:scan "(?i)android" user-agent) "Android")
    ((cl-ppcre:scan "(?i)linux" user-agent) "Linux")
    ((cl-ppcre:scan "(?i)cros" user-agent) "ChromeOS")
    (t "Other")))

(defun parse-user-agent (user-agent)
  "Parse User-Agent string and return a plist with device-type, browser, and os."
  (list :device-type (parse-device-type user-agent)
        :browser (parse-browser user-agent)
        :os (parse-os user-agent)))

;;; ----------------------------------------------------------------------------
;;; URL cleaning (strip tracking parameters)
;;; ----------------------------------------------------------------------------

(defparameter *tracking-params*
  '("fbclid"        ; Facebook
    "gclid"         ; Google Ads
    "gad_source"    ; Google Ads
    "gbraid"        ; Google Ads (iOS)
    "wbraid"        ; Google Ads (web-to-app)
    "dclid"         ; Google Display
    "msclkid"       ; Microsoft/Bing Ads
    "twclid"        ; Twitter
    "li_fat_id"     ; LinkedIn
    "mc_eid"        ; Mailchimp
    "mc_cid"        ; Mailchimp
    "_hsenc"        ; HubSpot
    "_hsmi"         ; HubSpot
    "hsa_acc"       ; HubSpot Ads
    "hsa_cam"       ; HubSpot Ads
    "hsa_grp"       ; HubSpot Ads
    "hsa_ad"        ; HubSpot Ads
    "hsa_src"       ; HubSpot Ads
    "hsa_tgt"       ; HubSpot Ads
    "hsa_kw"        ; HubSpot Ads
    "hsa_mt"        ; HubSpot Ads
    "hsa_net"       ; HubSpot Ads
    "hsa_ver"       ; HubSpot Ads
    "utm_source"    ; UTM tracking
    "utm_medium"    ; UTM tracking
    "utm_campaign"  ; UTM tracking
    "utm_term"      ; UTM tracking
    "utm_content"   ; UTM tracking
    "utm_id"        ; UTM tracking
    "utm_source_platform"  ; UTM tracking
    "utm_creative_format"  ; UTM tracking
    "utm_marketing_tactic" ; UTM tracking
    "oly_enc_id"    ; Omeda
    "oly_anon_id"   ; Omeda
    "vero_id"       ; Vero
    "vero_conv"     ; Vero
    "_ga"           ; Google Analytics
    "_gl"           ; Google Linker
    "spm"           ; Alibaba
    "scm"           ; Alibaba
    "pvid"          ; Alibaba
    "algo"          ; Alibaba
    "aff"           ; Affiliate tracking
    "ref"           ; Referral tracking
    "s_kwcid"       ; Adobe Analytics
    "ef_id"         ; Adobe Advertising Cloud
    "epik"          ; Pinterest
    "igshid"        ; Instagram
    "si"            ; Spotify
    )
  "List of tracking query parameters to strip from URLs.")

(defun strip-tracking-params (url)
  "Strip known tracking parameters from URL while preserving meaningful ones.
   Returns the cleaned URL."
  (when (null url)
    (return-from strip-tracking-params nil))
  (let ((query-start (position #\? url)))
    (if (null query-start)
        ;; No query string, return as-is
        url
        (let* ((base-url (subseq url 0 query-start))
               (query-string (subseq url (1+ query-start)))
               ;; Handle fragment (hash) if present
               (fragment-start (position #\# query-string))
               (fragment (when fragment-start (subseq query-string fragment-start)))
               (query-only (if fragment-start
                              (subseq query-string 0 fragment-start)
                              query-string))
               ;; Parse and filter parameters
               (params (cl-ppcre:split "&" query-only))
               (filtered-params
                 (remove-if
                  (lambda (param)
                    (let ((eq-pos (position #\= param)))
                      (when eq-pos
                        (let ((name (subseq param 0 eq-pos)))
                          (member name *tracking-params* :test #'string-equal)))))
                  params)))
          (if filtered-params
              ;; Reconstruct URL with remaining params
              (concatenate 'string
                           base-url "?"
                           (format nil "~{~A~^&~}" filtered-params)
                           (or fragment ""))
              ;; No params left, return base URL (with fragment if any)
              (concatenate 'string base-url (or fragment "")))))))

;;; ----------------------------------------------------------------------------
;;; Event recording
;;; ----------------------------------------------------------------------------

(defun record-event (site-id url referrer viewport-width ip user-agent timestamp)
  "Record a page view event.
   IP is hashed immediately and never stored.
   Returns T on success, NIL if site doesn't exist."
  ;; Validate site exists
  (unless (get-site site-id)
    (return-from record-event nil))

  (let* ((visitor-hash (generate-visitor-hash ip user-agent))
         (session-id (generate-session-id visitor-hash timestamp))
         (ua-info (parse-user-agent user-agent))
         ;; Look up country from IP (before discarding IP)
         (country (lookup-country ip))
         ;; Strip tracking parameters from URLs
         (clean-url (strip-tracking-params url))
         (clean-referrer (strip-tracking-params referrer)))
    (execute-sql
     "INSERT INTO events (site_id, timestamp, visitor_hash, session_id, url, referrer,
                          device_type, browser, os, country, viewport_width)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     site-id
     timestamp
     visitor-hash
     session-id
     clean-url
     (if (and clean-referrer (string/= clean-referrer "")) clean-referrer nil)
     (getf ua-info :device-type)
     (getf ua-info :browser)
     (getf ua-info :os)
     country
     viewport-width)
    t))

;;; ----------------------------------------------------------------------------
;;; Event API helpers
;;; ----------------------------------------------------------------------------

(defun parse-event-json (json-string)
  "Parse event JSON from the tracking script.
   Returns a plist with :site-id, :url, :referrer, :viewport-width, :timestamp."
  (let ((data (cl-json:decode-json-from-string json-string)))
    ;; cl-json converts site_id to :SITE--ID (underscore becomes hyphen, then camelCase handling)
    (list :site-id (or (cdr (assoc :site--id data))
                       (cdr (assoc :site-id data))
                       (cdr (assoc :|site_id| data)))
          :url (cdr (assoc :url data))
          :referrer (cdr (assoc :referrer data))
          :viewport-width (let ((viewport (cdr (assoc :viewport data))))
                            (when viewport
                              (cdr (assoc :width viewport))))
          :timestamp (or (cdr (assoc :timestamp data))
                         (unix-timestamp-ms)))))

(defun trusted-proxy-p (remote-addr)
  "Check if REMOTE-ADDR is a trusted proxy based on *trust-proxy* setting."
  (cond
    ((null *trust-proxy*) nil)
    ((eq *trust-proxy* t) t)
    ((listp *trust-proxy*) (member remote-addr *trust-proxy* :test #'string=))
    (t nil)))

(defun get-client-ip ()
  "Get the client IP address from the request.
   Only trusts X-Forwarded-For when *trust-proxy* is configured appropriately."
  (let ((remote-addr (hunchentoot:remote-addr*)))
    (if (trusted-proxy-p remote-addr)
        ;; Trust X-Forwarded-For from this proxy
        (let ((xff (hunchentoot:header-in* :x-forwarded-for)))
          (if xff
              (string-trim " " (first (cl-ppcre:split "," xff)))
              remote-addr))
        ;; Don't trust X-Forwarded-For, use direct connection IP
        remote-addr)))

(defun clamp-timestamp (client-timestamp)
  "Clamp CLIENT-TIMESTAMP to be within acceptable drift of server time.
   Returns the clamped timestamp in milliseconds."
  (let* ((server-time (unix-timestamp-ms))
         (min-allowed (- server-time *timestamp-max-drift-ms*))
         (max-allowed (+ server-time *timestamp-max-drift-ms*)))
    (cond
      ((null client-timestamp) server-time)
      ((< client-timestamp min-allowed) server-time)
      ((> client-timestamp max-allowed) server-time)
      (t client-timestamp))))

(defun process-tracking-event (body)
  "Process a tracking event from the request body.
   Returns :ok on success, :invalid-site if site doesn't exist,
   or :invalid-data if the request is malformed."
  (handler-case
      (let* ((event (parse-event-json body))
             (site-id (getf event :site-id))
             (url (getf event :url))
             (referrer (getf event :referrer))
             (viewport-width (getf event :viewport-width))
             ;; Clamp timestamp to prevent backdating/future-dating
             (timestamp (clamp-timestamp (getf event :timestamp)))
             (ip (get-client-ip))
             (user-agent (or (hunchentoot:header-in* :user-agent) "")))
        (cond
          ((or (null site-id) (null url))
           :invalid-data)
          ((record-event site-id url referrer viewport-width ip user-agent timestamp)
           :ok)
          (t
           :invalid-site)))
    (error ()
      :invalid-data)))
