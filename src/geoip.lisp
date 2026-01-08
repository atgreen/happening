;;; geoip.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; GeoIP lookup using ipverse/country-ip-blocks data
;;; Memory-efficient implementation using packed arrays

(in-package #:happening)

;;; ----------------------------------------------------------------------------
;;; Memory-efficient IP range storage
;;; ----------------------------------------------------------------------------
;;; We use parallel arrays for compactness:
;;;
;;; IPv4 (~9 bytes per range):
;;; - *ipv4-starts*: (simple-array (unsigned-byte 32)) of range start IPs
;;; - *ipv4-ends*: (simple-array (unsigned-byte 32)) of range end IPs
;;; - *ipv4-countries*: (simple-array (unsigned-byte 8)) of country indices
;;;
;;; IPv6 (~33 bytes per range):
;;; - *ipv6-starts-hi/lo*: pairs of (unsigned-byte 64) for 128-bit start
;;; - *ipv6-ends-hi/lo*: pairs of (unsigned-byte 64) for 128-bit end
;;; - *ipv6-countries*: (simple-array (unsigned-byte 8)) of country indices
;;;
;;; Country codes shared between IPv4 and IPv6.

(defvar *ipv4-starts* nil)
(defvar *ipv4-ends* nil)
(defvar *ipv4-countries* nil)

(defvar *ipv6-starts-hi* nil)
(defvar *ipv6-starts-lo* nil)
(defvar *ipv6-ends-hi* nil)
(defvar *ipv6-ends-lo* nil)
(defvar *ipv6-countries* nil)

(defvar *country-codes* nil
  "Vector mapping index -> 2-letter country code.")

(defvar *geoip-stats* nil
  "Plist of loading statistics.")

;;; ----------------------------------------------------------------------------
;;; IPv4 parsing
;;; ----------------------------------------------------------------------------

(defun parse-ipv4 (ip-string)
  "Parse an IPv4 address string to a 32-bit integer."
  (handler-case
      (let ((octets (mapcar #'parse-integer
                            (cl-ppcre:split "\\." ip-string))))
        (when (and (= (length octets) 4)
                   (every (lambda (o) (<= 0 o 255)) octets))
          (+ (ash (first octets) 24)
             (ash (second octets) 16)
             (ash (third octets) 8)
             (fourth octets))))
    (error () nil)))

(defun parse-ipv4-cidr (cidr-string)
  "Parse IPv4 CIDR notation. Returns (values start end) or NIL."
  (let ((parts (cl-ppcre:split "/" cidr-string)))
    (when (= (length parts) 2)
      (let ((base-ip (parse-ipv4 (first parts)))
            (prefix-len (parse-integer (second parts) :junk-allowed t)))
        (when (and base-ip prefix-len (<= 0 prefix-len 32))
          (let* ((mask (if (= prefix-len 0) 0
                           (ash (1- (ash 1 prefix-len)) (- 32 prefix-len))))
                 (start (logand base-ip mask))
                 (end (logior start (logand (lognot mask) #xFFFFFFFF))))
            (values start end)))))))

;;; ----------------------------------------------------------------------------
;;; IPv6 parsing
;;; ----------------------------------------------------------------------------

(defun parse-ipv6 (ip-string)
  "Parse an IPv6 address string to (values high64 low64).
   Handles full form, :: compression, and mixed IPv4 notation."
  (handler-case
      (let* ((str (string-trim " " ip-string))
             ;; Handle :: expansion
             (double-colon-pos (search "::" str))
             (groups nil))
        (cond
          ;; Has :: compression
          (double-colon-pos
           (let* ((before (subseq str 0 double-colon-pos))
                  (after (subseq str (+ double-colon-pos 2)))
                  (before-groups (if (string= before "") nil
                                     (cl-ppcre:split ":" before)))
                  (after-groups (if (string= after "") nil
                                    (cl-ppcre:split ":" after)))
                  (total-explicit (+ (length before-groups) (length after-groups)))
                  (zeros-needed (- 8 total-explicit)))
             (setf groups (append before-groups
                                  (make-list zeros-needed :initial-element "0")
                                  after-groups))))
          ;; No compression
          (t (setf groups (cl-ppcre:split ":" str))))

        (when (= (length groups) 8)
          (let ((values (mapcar (lambda (g) (parse-integer g :radix 16)) groups)))
            (when (every (lambda (v) (<= 0 v #xFFFF)) values)
              (let ((high (+ (ash (nth 0 values) 48)
                             (ash (nth 1 values) 32)
                             (ash (nth 2 values) 16)
                             (nth 3 values)))
                    (low (+ (ash (nth 4 values) 48)
                            (ash (nth 5 values) 32)
                            (ash (nth 6 values) 16)
                            (nth 7 values))))
                (values high low))))))
    (error () nil)))

(defun parse-ipv6-cidr (cidr-string)
  "Parse IPv6 CIDR notation. Returns (values start-hi start-lo end-hi end-lo) or NIL."
  (let ((parts (cl-ppcre:split "/" cidr-string)))
    (when (= (length parts) 2)
      (multiple-value-bind (base-hi base-lo) (parse-ipv6 (first parts))
        (let ((prefix-len (parse-integer (second parts) :junk-allowed t)))
          (when (and base-hi base-lo prefix-len (<= 0 prefix-len 128))
            ;; Calculate mask and apply
            (multiple-value-bind (start-hi start-lo end-hi end-lo)
                (apply-ipv6-mask base-hi base-lo prefix-len)
              (values start-hi start-lo end-hi end-lo))))))))

(defun apply-ipv6-mask (base-hi base-lo prefix-len)
  "Apply prefix mask to IPv6 address. Returns (values start-hi start-lo end-hi end-lo)."
  (cond
    ;; Prefix in high 64 bits
    ((<= prefix-len 64)
     (let* ((mask-hi (if (= prefix-len 0) 0
                         (ash (1- (ash 1 prefix-len)) (- 64 prefix-len))))
            (start-hi (logand base-hi mask-hi))
            (start-lo 0)
            (end-hi (logior start-hi (logand (lognot mask-hi) #xFFFFFFFFFFFFFFFF)))
            (end-lo #xFFFFFFFFFFFFFFFF))
       (values start-hi start-lo end-hi end-lo)))
    ;; Prefix spans both
    (t
     (let* ((lo-prefix (- prefix-len 64))
            (mask-lo (ash (1- (ash 1 lo-prefix)) (- 64 lo-prefix)))
            (start-hi base-hi)
            (start-lo (logand base-lo mask-lo))
            (end-hi base-hi)
            (end-lo (logior start-lo (logand (lognot mask-lo) #xFFFFFFFFFFFFFFFF))))
       (values start-hi start-lo end-hi end-lo)))))

;;; ----------------------------------------------------------------------------
;;; Loading country IP blocks
;;; ----------------------------------------------------------------------------

(defun default-country-ip-blocks-path ()
  "Return the default path for the country-ip-blocks repository."
  (merge-pathnames "data/country-ip-blocks/" (uiop:getcwd)))

(defun load-ipv4-file (filepath)
  "Load IPv4 ranges from ipv4-aggregated.txt. Returns list of (start . end)."
  (with-open-file (stream filepath :direction :input :if-does-not-exist nil)
    (when stream
      (loop for line = (read-line stream nil nil)
            while line
            for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
            when (and (> (length trimmed) 0)
                      (not (char= (char trimmed 0) #\#)))
              collect (multiple-value-bind (start end) (parse-ipv4-cidr trimmed)
                        (when (and start end) (cons start end)))
              into ranges
            finally (return (remove nil ranges))))))

(defun load-ipv6-file (filepath)
  "Load IPv6 ranges from ipv6-aggregated.txt. Returns list of (start-hi start-lo end-hi end-lo)."
  (with-open-file (stream filepath :direction :input :if-does-not-exist nil)
    (when stream
      (loop for line = (read-line stream nil nil)
            while line
            for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
            when (and (> (length trimmed) 0)
                      (not (char= (char trimmed 0) #\#)))
              collect (multiple-value-bind (shi slo ehi elo) (parse-ipv6-cidr trimmed)
                        (when shi (list shi slo ehi elo)))
              into ranges
            finally (return (remove nil ranges))))))

(defun load-external-geoip-data (repo-path)
  "Load GeoIP data from external ipverse repository."
  (handler-case
      (let ((ipv4-ranges nil)
            (ipv6-ranges nil)
            (country-list nil)
            (country-idx-map (make-hash-table :test 'equal))
            (country-dir (merge-pathnames "country/" repo-path)))

        ;; Iterate through country subdirectories
        (dolist (entry (uiop:subdirectories country-dir))
          (let* ((country-code (string-upcase
                                (car (last (pathname-directory entry)))))
                 (idx (or (gethash country-code country-idx-map)
                          (let ((new-idx (length country-list)))
                            (push country-code country-list)
                            (setf (gethash country-code country-idx-map) new-idx)
                            new-idx))))
            ;; Load IPv4
            (let ((v4-file (merge-pathnames "ipv4-aggregated.txt" entry)))
              (dolist (range (load-ipv4-file v4-file))
                (push (list (car range) (cdr range) idx) ipv4-ranges)))
            ;; Load IPv6
            (let ((v6-file (merge-pathnames "ipv6-aggregated.txt" entry)))
              (dolist (range (load-ipv6-file v6-file))
                (push (append range (list idx)) ipv6-ranges)))))

        ;; Build IPv4 arrays
        (setf ipv4-ranges (sort ipv4-ranges #'< :key #'first))
        (let ((n (length ipv4-ranges)))
          (setf *ipv4-starts* (make-array n :element-type '(unsigned-byte 32)))
          (setf *ipv4-ends* (make-array n :element-type '(unsigned-byte 32)))
          (setf *ipv4-countries* (make-array n :element-type '(unsigned-byte 8)))
          (loop for (start end idx) in ipv4-ranges
                for i from 0
                do (setf (aref *ipv4-starts* i) start
                         (aref *ipv4-ends* i) end
                         (aref *ipv4-countries* i) idx)))

        ;; Build IPv6 arrays (sort by high, then low)
        (setf ipv6-ranges (sort ipv6-ranges
                                (lambda (a b)
                                  (or (< (first a) (first b))
                                      (and (= (first a) (first b))
                                           (< (second a) (second b)))))))
        (let ((n (length ipv6-ranges)))
          (setf *ipv6-starts-hi* (make-array n :element-type '(unsigned-byte 64)))
          (setf *ipv6-starts-lo* (make-array n :element-type '(unsigned-byte 64)))
          (setf *ipv6-ends-hi* (make-array n :element-type '(unsigned-byte 64)))
          (setf *ipv6-ends-lo* (make-array n :element-type '(unsigned-byte 64)))
          (setf *ipv6-countries* (make-array n :element-type '(unsigned-byte 8)))
          (loop for (shi slo ehi elo idx) in ipv6-ranges
                for i from 0
                do (setf (aref *ipv6-starts-hi* i) shi
                         (aref *ipv6-starts-lo* i) slo
                         (aref *ipv6-ends-hi* i) ehi
                         (aref *ipv6-ends-lo* i) elo
                         (aref *ipv6-countries* i) idx)))

        (setf *country-codes* (coerce (nreverse country-list) 'vector))
        t)
    (error (e)
      (llog:error (format nil "Failed to load external GeoIP data: ~A" e))
      nil)))

(defun load-embedded-geoip-data ()
  "Load GeoIP data from embedded arrays."
  (when (and *embedded-ipv4-starts* (> (length *embedded-ipv4-starts*) 0))
    ;; Convert simple vectors to typed arrays for IPv4
    (let ((n (length *embedded-ipv4-starts*)))
      (setf *ipv4-starts* (make-array n :element-type '(unsigned-byte 32)))
      (setf *ipv4-ends* (make-array n :element-type '(unsigned-byte 32)))
      (setf *ipv4-countries* (make-array n :element-type '(unsigned-byte 8)))
      (loop for i from 0 below n
            do (setf (aref *ipv4-starts* i) (aref *embedded-ipv4-starts* i)
                     (aref *ipv4-ends* i) (aref *embedded-ipv4-ends* i)
                     (aref *ipv4-countries* i) (aref *embedded-ipv4-countries* i))))

    ;; Convert simple vectors to typed arrays for IPv6
    (when (and *embedded-ipv6-starts-hi* (> (length *embedded-ipv6-starts-hi*) 0))
      (let ((n (length *embedded-ipv6-starts-hi*)))
        (setf *ipv6-starts-hi* (make-array n :element-type '(unsigned-byte 64)))
        (setf *ipv6-starts-lo* (make-array n :element-type '(unsigned-byte 64)))
        (setf *ipv6-ends-hi* (make-array n :element-type '(unsigned-byte 64)))
        (setf *ipv6-ends-lo* (make-array n :element-type '(unsigned-byte 64)))
        (setf *ipv6-countries* (make-array n :element-type '(unsigned-byte 8)))
        (loop for i from 0 below n
              do (setf (aref *ipv6-starts-hi* i) (aref *embedded-ipv6-starts-hi* i)
                       (aref *ipv6-starts-lo* i) (aref *embedded-ipv6-starts-lo* i)
                       (aref *ipv6-ends-hi* i) (aref *embedded-ipv6-ends-hi* i)
                       (aref *ipv6-ends-lo* i) (aref *embedded-ipv6-ends-lo* i)
                       (aref *ipv6-countries* i) (aref *embedded-ipv6-countries* i)))))

    (setf *country-codes* (coerce *embedded-country-codes* 'vector))
    t))

(defun finalize-geoip-stats (source)
  "Calculate and log GeoIP statistics after loading."
  (let* ((v4-count (if *ipv4-starts* (length *ipv4-starts*) 0))
         (v6-count (if *ipv6-starts-hi* (length *ipv6-starts-hi*) 0))
         (v4-bytes (+ (* v4-count 4) (* v4-count 4) v4-count))
         (v6-bytes (+ (* v6-count 8 4) v6-count))
         (total-mb (/ (+ v4-bytes v6-bytes) 1048576.0)))
    (setf *geoip-stats* (list :ipv4-ranges v4-count
                              :ipv6-ranges v6-count
                              :countries (if *country-codes* (length *country-codes*) 0)
                              :memory-mb total-mb
                              :source source))
    (llog:info (format nil "Loaded ~A IPv4 + ~A IPv6 ranges for ~A countries (~,1F MB) [~A]"
                       v4-count v6-count
                       (if *country-codes* (length *country-codes*) 0)
                       total-mb source))))

(defun load-geoip-database (&optional path)
  "Load IP-to-country data. Prefers external ipverse repository, falls back to embedded data."
  (let ((repo-path (or path (default-country-ip-blocks-path))))
    (cond
      ;; Try external data first (more up-to-date)
      ((probe-file (merge-pathnames "country/" repo-path))
       (if (load-external-geoip-data repo-path)
           (finalize-geoip-stats "external")
           ;; External failed, try embedded as fallback
           (when (load-embedded-geoip-data)
             (finalize-geoip-stats "embedded"))))

      ;; No external data, try embedded
      ((and *embedded-ipv4-starts* (> (length *embedded-ipv4-starts*) 0))
       (if (load-embedded-geoip-data)
           (finalize-geoip-stats "embedded")
           (llog:info "Failed to load embedded GeoIP data")))

      ;; No data available
      (t
       (llog:info "GeoIP data not available (country lookup disabled)")
       (llog:info "To enable with latest data: git clone https://github.com/ipverse/country-ip-blocks data/country-ip-blocks")
       nil))))

(defun geoip-available-p ()
  "Check if GeoIP lookup is available."
  (or (and *ipv4-starts* (> (length *ipv4-starts*) 0))
      (and *ipv6-starts-hi* (> (length *ipv6-starts-hi*) 0))))

;;; ----------------------------------------------------------------------------
;;; IP to country lookup
;;; ----------------------------------------------------------------------------

(defun lookup-country (ip-address)
  "Look up the country code for an IP address (IPv4 or IPv6).
   Returns the ISO 3166-1 alpha-2 country code or NIL."
  (when (and ip-address (stringp ip-address))
    (if (find #\: ip-address)
        (lookup-country-ipv6 ip-address)
        (lookup-country-ipv4 ip-address))))

(defun lookup-country-ipv4 (ip-string)
  "Look up country for IPv4 address."
  (when *ipv4-starts*
    (let ((ip-int (parse-ipv4 ip-string)))
      (when ip-int
        (let ((idx (binary-search-ipv4 ip-int)))
          (when idx (aref *country-codes* idx)))))))

(defun lookup-country-ipv6 (ip-string)
  "Look up country for IPv6 address."
  (when *ipv6-starts-hi*
    (multiple-value-bind (hi lo) (parse-ipv6 ip-string)
      (when (and hi lo)
        (let ((idx (binary-search-ipv6 hi lo)))
          (when idx (aref *country-codes* idx)))))))

(defun binary-search-ipv4 (ip-int)
  "Binary search for country index containing IPv4 address."
  (declare (type (unsigned-byte 32) ip-int)
           (optimize (speed 3) (safety 0)))
  (let ((low 0)
        (high (1- (length *ipv4-starts*))))
    (declare (type fixnum low high))
    (loop while (<= low high)
          for mid fixnum = (ash (+ low high) -1)
          for start = (aref *ipv4-starts* mid)
          for end = (aref *ipv4-ends* mid)
          do (cond
               ((< ip-int start) (setf high (1- mid)))
               ((> ip-int end) (setf low (1+ mid)))
               (t (return (aref *ipv4-countries* mid)))))))

(defun binary-search-ipv6 (hi lo)
  "Binary search for country index containing IPv6 address."
  (declare (type (unsigned-byte 64) hi lo)
           (optimize (speed 3) (safety 0)))
  (let ((low 0)
        (high (1- (length *ipv6-starts-hi*))))
    (declare (type fixnum low high))
    (loop while (<= low high)
          for mid fixnum = (ash (+ low high) -1)
          for s-hi = (aref *ipv6-starts-hi* mid)
          for s-lo = (aref *ipv6-starts-lo* mid)
          for e-hi = (aref *ipv6-ends-hi* mid)
          for e-lo = (aref *ipv6-ends-lo* mid)
          do (cond
               ;; Before range
               ((or (< hi s-hi) (and (= hi s-hi) (< lo s-lo)))
                (setf high (1- mid)))
               ;; After range
               ((or (> hi e-hi) (and (= hi e-hi) (> lo e-lo)))
                (setf low (1+ mid)))
               ;; In range
               (t (return (aref *ipv6-countries* mid)))))))
