;;; dashboard.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Dashboard queries and views for Happening analytics

(in-package #:happening)

;;; ----------------------------------------------------------------------------
;;; Date/time helpers
;;; ----------------------------------------------------------------------------

(defun date-to-timestamp (date-string)
  "Convert a date string (YYYY-MM-DD) to Unix timestamp (start of day UTC)."
  (let ((parsed (local-time:parse-timestring
                 (format nil "~AT00:00:00Z" date-string))))
    (* 1000 (local-time:timestamp-to-unix parsed))))

(defun timestamp-to-date (timestamp)
  "Convert a Unix timestamp (ms) to date string (YYYY-MM-DD)."
  (local-time:format-timestring
   nil
   (local-time:unix-to-timestamp (floor timestamp 1000))
   :format '(:year "-" (:month 2) "-" (:day 2))))

(defun days-ago (n)
  "Return the date string for N days ago (in UTC)."
  (local-time:format-timestring
   nil
   (local-time:timestamp- (local-time:now) n :day)
   :format '(:year "-" (:month 2) "-" (:day 2))
   :timezone local-time:+utc-zone+))

(defun today ()
  "Return today's date string."
  (days-ago 0))

;;; ----------------------------------------------------------------------------
;;; Dashboard queries
;;; ----------------------------------------------------------------------------

(defun get-pageviews (site-id start-date end-date)
  "Get total page views for a site in the date range."
  (let* ((start-ts (date-to-timestamp start-date))
         (end-ts (+ (date-to-timestamp end-date) (* 24 60 60 1000)))
         (result (fetch-one
                  "SELECT COUNT(*) as count FROM events
                   WHERE site_id = ? AND timestamp >= ? AND timestamp < ?"
                  site-id start-ts end-ts)))
    (or (getf result :|count|) 0)))

(defun get-unique-visitors (site-id start-date end-date)
  "Get unique visitors for a site in the date range."
  (let* ((start-ts (date-to-timestamp start-date))
         (end-ts (+ (date-to-timestamp end-date) (* 24 60 60 1000)))
         (result (fetch-one
                  "SELECT COUNT(DISTINCT visitor_hash) as count FROM events
                   WHERE site_id = ? AND timestamp >= ? AND timestamp < ?"
                  site-id start-ts end-ts)))
    (or (getf result :|count|) 0)))

(defun get-sessions (site-id start-date end-date)
  "Get total sessions for a site in the date range."
  (let* ((start-ts (date-to-timestamp start-date))
         (end-ts (+ (date-to-timestamp end-date) (* 24 60 60 1000)))
         (result (fetch-one
                  "SELECT COUNT(DISTINCT session_id) as count FROM events
                   WHERE site_id = ? AND timestamp >= ? AND timestamp < ?"
                  site-id start-ts end-ts)))
    (or (getf result :|count|) 0)))

(defun get-bounce-rate (site-id start-date end-date)
  "Get bounce rate for a site in the date range.
   Bounce = session with only one page view."
  (let* ((start-ts (date-to-timestamp start-date))
         (end-ts (+ (date-to-timestamp end-date) (* 24 60 60 1000)))
         (result (fetch-one
                  "SELECT
                     COUNT(DISTINCT CASE WHEN cnt = 1 THEN session_id END) as bounces,
                     COUNT(DISTINCT session_id) as total
                   FROM (
                     SELECT session_id, COUNT(*) as cnt FROM events
                     WHERE site_id = ? AND timestamp >= ? AND timestamp < ?
                     GROUP BY session_id
                   )"
                  site-id start-ts end-ts))
         (bounces (or (getf result :|bounces|) 0))
         (total (or (getf result :|total|) 1)))
    (if (zerop total)
        0.0
        (/ (float bounces) total))))

(defun get-top-pages (site-id start-date end-date &optional (limit 10))
  "Get top pages for a site in the date range."
  (let* ((start-ts (date-to-timestamp start-date))
         (end-ts (+ (date-to-timestamp end-date) (* 24 60 60 1000))))
    (fetch-all
     "SELECT url, COUNT(*) as views FROM events
      WHERE site_id = ? AND timestamp >= ? AND timestamp < ?
      GROUP BY url ORDER BY views DESC LIMIT ?"
     site-id start-ts end-ts limit)))

(defun get-top-referrers (site-id start-date end-date &optional (limit 10))
  "Get top referrers for a site in the date range."
  (let* ((start-ts (date-to-timestamp start-date))
         (end-ts (+ (date-to-timestamp end-date) (* 24 60 60 1000))))
    (fetch-all
     "SELECT COALESCE(referrer, '(direct)') as referrer, COUNT(*) as views FROM events
      WHERE site_id = ? AND timestamp >= ? AND timestamp < ?
      GROUP BY referrer ORDER BY views DESC LIMIT ?"
     site-id start-ts end-ts limit)))

(defun get-device-breakdown (site-id start-date end-date)
  "Get device type breakdown for a site in the date range."
  (let* ((start-ts (date-to-timestamp start-date))
         (end-ts (+ (date-to-timestamp end-date) (* 24 60 60 1000))))
    (fetch-all
     "SELECT device_type, COUNT(*) as count FROM events
      WHERE site_id = ? AND timestamp >= ? AND timestamp < ?
      GROUP BY device_type ORDER BY count DESC"
     site-id start-ts end-ts)))

(defun get-browser-breakdown (site-id start-date end-date)
  "Get browser breakdown for a site in the date range."
  (let* ((start-ts (date-to-timestamp start-date))
         (end-ts (+ (date-to-timestamp end-date) (* 24 60 60 1000))))
    (fetch-all
     "SELECT browser, COUNT(*) as count FROM events
      WHERE site_id = ? AND timestamp >= ? AND timestamp < ?
      GROUP BY browser ORDER BY count DESC"
     site-id start-ts end-ts)))

(defun get-os-breakdown (site-id start-date end-date)
  "Get OS breakdown for a site in the date range."
  (let* ((start-ts (date-to-timestamp start-date))
         (end-ts (+ (date-to-timestamp end-date) (* 24 60 60 1000))))
    (fetch-all
     "SELECT os, COUNT(*) as count FROM events
      WHERE site_id = ? AND timestamp >= ? AND timestamp < ?
      GROUP BY os ORDER BY count DESC"
     site-id start-ts end-ts)))

(defun get-country-breakdown (site-id start-date end-date &optional (limit 10))
  "Get country breakdown for a site in the date range."
  (let* ((start-ts (date-to-timestamp start-date))
         (end-ts (+ (date-to-timestamp end-date) (* 24 60 60 1000))))
    (fetch-all
     "SELECT COALESCE(country, 'Unknown') as country, COUNT(*) as count FROM events
      WHERE site_id = ? AND timestamp >= ? AND timestamp < ?
      GROUP BY country ORDER BY count DESC LIMIT ?"
     site-id start-ts end-ts limit)))

(defun get-daily-stats (site-id start-date end-date)
  "Get daily statistics for a site in the date range."
  (let* ((start-ts (date-to-timestamp start-date))
         (end-ts (+ (date-to-timestamp end-date) (* 24 60 60 1000))))
    (fetch-all
     "SELECT
        date(timestamp/1000, 'unixepoch') as date,
        COUNT(*) as pageviews,
        COUNT(DISTINCT visitor_hash) as visitors,
        COUNT(DISTINCT session_id) as sessions
      FROM events
      WHERE site_id = ? AND timestamp >= ? AND timestamp < ?
      GROUP BY date(timestamp/1000, 'unixepoch')
      ORDER BY date"
     site-id start-ts end-ts)))

(defun get-realtime-visitors (site-id &optional (minutes 5))
  "Get the number of visitors in the last N minutes."
  (let* ((cutoff (- (unix-timestamp-ms) (* minutes 60 1000)))
         (result (fetch-one
                  "SELECT COUNT(DISTINCT visitor_hash) as count FROM events
                   WHERE site_id = ? AND timestamp >= ?"
                  site-id cutoff)))
    (or (getf result :|count|) 0)))

;;; ----------------------------------------------------------------------------
;;; Dashboard data aggregation
;;; ----------------------------------------------------------------------------

(defun get-dashboard-stats (site-id start-date end-date)
  "Get all dashboard statistics for a site."
  (list :pageviews (get-pageviews site-id start-date end-date)
        :visitors (get-unique-visitors site-id start-date end-date)
        :sessions (get-sessions site-id start-date end-date)
        :bounce-rate (get-bounce-rate site-id start-date end-date)
        :top-pages (get-top-pages site-id start-date end-date)
        :top-referrers (get-top-referrers site-id start-date end-date)
        :devices (get-device-breakdown site-id start-date end-date)
        :browsers (get-browser-breakdown site-id start-date end-date)
        :countries (get-country-breakdown site-id start-date end-date)
        :daily (get-daily-stats site-id start-date end-date)
        :realtime (get-realtime-visitors site-id)))

;;; ----------------------------------------------------------------------------
;;; HTML rendering helpers
;;; ----------------------------------------------------------------------------

(defun html-escape (string)
  "Escape HTML special characters in a string."
  (if (null string)
      ""
      (with-output-to-string (out)
        (loop for char across (princ-to-string string)
              do (case char
                   (#\< (write-string "&lt;" out))
                   (#\> (write-string "&gt;" out))
                   (#\& (write-string "&amp;" out))
                   (#\" (write-string "&quot;" out))
                   (#\' (write-string "&#39;" out))
                   (otherwise (write-char char out)))))))

(defun format-number (n)
  "Format a number with commas for thousands."
  (if (null n)
      "0"
      (format nil "~:D" (floor n))))

(defun format-percentage (n)
  "Format a number as a percentage."
  (format nil "~,1F%" (* 100 (or n 0))))

(defun format-url-path (url)
  "Extract and format the path from a URL."
  (handler-case
      (let ((parsed (quri:uri url)))
        (or (quri:uri-path parsed) "/"))
    (error () url)))

;;; ----------------------------------------------------------------------------
;;; HTML page templates
;;; ----------------------------------------------------------------------------

(defun render-base-page (title body-content &key (include-charts nil))
  "Render a base HTML page with the given title and body content."
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (:title (cl-who:str title))
      (:link :rel "stylesheet" :href "/css/style.css")
      (when include-charts
        (cl-who:htm
         (:script :src "/js/chart.min.js"))))
     (:body
      (cl-who:str body-content)
      (:script :src "/js/app.js")))))

(defun render-stat-card (label value &optional change)
  "Render a stat card component."
  (cl-who:with-html-output-to-string (s)
    (:div :class "stat-card"
          (:div :class "stat-value" (cl-who:str value))
          (:div :class "stat-label" (cl-who:str label))
          (when change
            (cl-who:htm
             (:div :class (if (>= change 0) "stat-change positive" "stat-change negative")
                   (cl-who:fmt "~@D%" (round change))))))))

(defun render-table (headers rows &key (class "data-table"))
  "Render an HTML table."
  (cl-who:with-html-output-to-string (s)
    (:table :class class
            (:thead
             (:tr
              (loop for header in headers
                    do (cl-who:htm (:th (cl-who:str header))))))
            (:tbody
             (loop for row in rows
                   do (cl-who:htm
                       (:tr
                        (loop for cell in row
                              do (cl-who:htm (:td (cl-who:str (html-escape cell))))))))))))

(defun render-dashboard (site stats start-date end-date)
  "Render the main dashboard page for a site."
  (let* ((site-name (getf site :|name|))
         (site-domain (getf site :|domain|))
         (pageviews (getf stats :pageviews))
         (visitors (getf stats :visitors))
         (bounce-rate (getf stats :bounce-rate))
         (realtime (getf stats :realtime)))
    (render-base-page
     (format nil "~A - Happening" site-name)
     (cl-who:with-html-output-to-string (s)
       (:div :class "dashboard"
             ;; Header
             (:header :class "dashboard-header"
                      (:h1 "Happening")
                      (:div :class "site-selector"
                            (:span (cl-who:str site-domain))
                            (:a :href "/sites" "Switch Site"))
                      (:nav
                       (:a :href "/logout" "Logout")))

             ;; Date range
             (:div :class "date-range"
                   (:span (cl-who:fmt "~A to ~A" start-date end-date))
                   (:div :class "date-presets"
                         (:a :href "?range=7d" "7 days")
                         (:a :href "?range=30d" "30 days")
                         (:a :href "?range=90d" "90 days")))

             ;; Realtime badge
             (:div :class "realtime-badge"
                   (:span :class "pulse")
                   (:span (cl-who:fmt "~A active now" realtime)))

             ;; Stat cards
             (:div :class "stat-cards"
                   (cl-who:str (render-stat-card "Page Views" (format-number pageviews)))
                   (cl-who:str (render-stat-card "Unique Visitors" (format-number visitors)))
                   (cl-who:str (render-stat-card "Bounce Rate" (format-percentage bounce-rate))))

             ;; Charts placeholder
             (:div :class "chart-container"
                   (:canvas :id "traffic-chart"))

             ;; Two-column layout for tables
             (:div :class "dashboard-grid"
                   ;; Top Pages
                   (:div :class "dashboard-panel"
                         (:h3 "Top Pages")
                         (cl-who:str
                          (render-table
                           '("Page" "Views")
                           (loop for page in (getf stats :top-pages)
                                 collect (list (getf page :|url|)
                                               (format-number (getf page :|views|)))))))

                   ;; Top Referrers
                   (:div :class "dashboard-panel"
                         (:h3 "Top Referrers")
                         (cl-who:str
                          (render-table
                           '("Source" "Views")
                           (loop for ref in (getf stats :top-referrers)
                                 collect (list (getf ref :|referrer|)
                                               (format-number (getf ref :|views|)))))))

                   ;; Devices
                   (:div :class "dashboard-panel"
                         (:h3 "Devices")
                         (cl-who:str
                          (render-table
                           '("Device" "Count")
                           (loop for device in (getf stats :devices)
                                 collect (list (getf device :|device_type|)
                                               (format-number (getf device :|count|)))))))

                   ;; Browsers
                   (:div :class "dashboard-panel"
                         (:h3 "Browsers")
                         (cl-who:str
                          (render-table
                           '("Browser" "Count")
                           (loop for browser in (getf stats :browsers)
                                 collect (list (getf browser :|browser|)
                                               (format-number (getf browser :|count|)))))))

                   ;; Countries
                   (:div :class "dashboard-panel"
                         (:h3 "Countries")
                         (cl-who:str
                          (render-table
                           '("Country" "Count")
                           (loop for country in (getf stats :countries)
                                 collect (list (getf country :|country|)
                                               (format-number (getf country :|count|)))))))

                   ;; Chart data for JS
                   (:script
                    (cl-who:fmt "window.CHART_DATA = ~A;"
                                (cl-json:encode-json-to-string
                                 (loop for day in (getf stats :daily)
                                       collect (list (cons "date" (getf day :|date|))
                                                     (cons "pageviews" (getf day :|pageviews|))
                                                     (cons "visitors" (getf day :|visitors|))))))))
             ;; Footer
             (:footer :class "dashboard-footer"
                      (:p "Powered by Happening Analytics"))))
     :include-charts t)))
