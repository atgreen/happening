;;; generate-geoip-data.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Build-time script to generate embedded GeoIP data from ipverse repository.
;;; Run with: sbcl --load scripts/generate-geoip-data.lisp --quit
;;;
;;; Expects data/country-ip-blocks/ to contain the cloned ipverse repository.

(require :asdf)
(require :uiop)

;;; ----------------------------------------------------------------------------
;;; IPv4 parsing (standalone, no dependencies)
;;; ----------------------------------------------------------------------------

(defun parse-ipv4 (ip-string)
  "Parse an IPv4 address string to a 32-bit integer."
  (let* ((parts (uiop:split-string ip-string :separator "."))
         (octets (mapcar #'parse-integer parts)))
    (when (and (= (length octets) 4)
               (every (lambda (o) (<= 0 o 255)) octets))
      (+ (ash (first octets) 24)
         (ash (second octets) 16)
         (ash (third octets) 8)
         (fourth octets)))))

(defun parse-ipv4-cidr (cidr-string)
  "Parse IPv4 CIDR notation. Returns (values start end) or NIL."
  (let ((slash-pos (position #\/ cidr-string)))
    (when slash-pos
      (let ((base-ip (parse-ipv4 (subseq cidr-string 0 slash-pos)))
            (prefix-len (parse-integer (subseq cidr-string (1+ slash-pos)) :junk-allowed t)))
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
  "Parse an IPv6 address string to (values high64 low64)."
  (let* ((str (string-trim " " ip-string))
         (double-colon-pos (search "::" str))
         (groups nil))
    (cond
      ;; Has :: compression
      (double-colon-pos
       (let* ((before (subseq str 0 double-colon-pos))
              (after (subseq str (+ double-colon-pos 2)))
              (before-groups (if (string= before "") nil
                                 (uiop:split-string before :separator ":")))
              (after-groups (if (string= after "") nil
                                (uiop:split-string after :separator ":")))
              (total-explicit (+ (length before-groups) (length after-groups)))
              (zeros-needed (- 8 total-explicit)))
         (setf groups (append before-groups
                              (make-list zeros-needed :initial-element "0")
                              after-groups))))
      ;; No compression
      (t (setf groups (uiop:split-string str :separator ":"))))

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
            (values high low)))))))

(defun apply-ipv6-mask (base-hi base-lo prefix-len)
  "Apply prefix mask to IPv6 address. Returns (values start-hi start-lo end-hi end-lo)."
  (cond
    ((<= prefix-len 64)
     (let* ((mask-hi (if (= prefix-len 0) 0
                         (ash (1- (ash 1 prefix-len)) (- 64 prefix-len))))
            (start-hi (logand base-hi mask-hi))
            (start-lo 0)
            (end-hi (logior start-hi (logand (lognot mask-hi) #xFFFFFFFFFFFFFFFF)))
            (end-lo #xFFFFFFFFFFFFFFFF))
       (values start-hi start-lo end-hi end-lo)))
    (t
     (let* ((lo-prefix (- prefix-len 64))
            (mask-lo (ash (1- (ash 1 lo-prefix)) (- 64 lo-prefix)))
            (start-hi base-hi)
            (start-lo (logand base-lo mask-lo))
            (end-hi base-hi)
            (end-lo (logior start-lo (logand (lognot mask-lo) #xFFFFFFFFFFFFFFFF))))
       (values start-hi start-lo end-hi end-lo)))))

(defun parse-ipv6-cidr (cidr-string)
  "Parse IPv6 CIDR notation. Returns (values start-hi start-lo end-hi end-lo) or NIL."
  (let ((slash-pos (position #\/ cidr-string)))
    (when slash-pos
      (multiple-value-bind (base-hi base-lo) (parse-ipv6 (subseq cidr-string 0 slash-pos))
        (let ((prefix-len (parse-integer (subseq cidr-string (1+ slash-pos)) :junk-allowed t)))
          (when (and base-hi base-lo prefix-len (<= 0 prefix-len 128))
            (apply-ipv6-mask base-hi base-lo prefix-len)))))))

;;; ----------------------------------------------------------------------------
;;; File loading
;;; ----------------------------------------------------------------------------

(defun load-ipv4-file (filepath)
  "Load IPv4 ranges from ipv4-aggregated.txt."
  (when (probe-file filepath)
    (with-open-file (stream filepath :direction :input)
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
  "Load IPv6 ranges from ipv6-aggregated.txt."
  (when (probe-file filepath)
    (with-open-file (stream filepath :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
            when (and (> (length trimmed) 0)
                      (not (char= (char trimmed 0) #\#)))
              collect (multiple-value-bind (shi slo ehi elo) (parse-ipv6-cidr trimmed)
                        (when shi (list shi slo ehi elo)))
              into ranges
            finally (return (remove nil ranges))))))

;;; ----------------------------------------------------------------------------
;;; Main generation
;;; ----------------------------------------------------------------------------

(defun generate-geoip-data (repo-path output-path)
  "Generate embedded GeoIP data file from ipverse repository."
  (let ((country-dir (merge-pathnames "country/" repo-path))
        (ipv4-ranges nil)
        (ipv6-ranges nil)
        (country-list nil)
        (country-idx-map (make-hash-table :test 'equal)))

    (unless (probe-file country-dir)
      (format *error-output* "Error: country directory not found at ~A~%" country-dir)
      (return-from generate-geoip-data nil))

    (format t "Reading country data from ~A~%" country-dir)

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

    ;; Sort ranges
    (setf ipv4-ranges (sort ipv4-ranges #'< :key #'first))
    (setf ipv6-ranges (sort ipv6-ranges
                            (lambda (a b)
                              (or (< (first a) (first b))
                                  (and (= (first a) (first b))
                                       (< (second a) (second b)))))))
    (setf country-list (nreverse country-list))

    (format t "Loaded ~A IPv4 ranges, ~A IPv6 ranges, ~A countries~%"
            (length ipv4-ranges) (length ipv6-ranges) (length country-list))

    ;; Write output file
    (with-open-file (out output-path :direction :output :if-exists :supersede)
      (format out ";;; geoip-embedded.lisp~%")
      (format out ";;;~%")
      (format out ";;; SPDX-License-Identifier: MIT~%")
      (format out ";;;~%")
      (format out ";;; Auto-generated embedded GeoIP data from ipverse/country-ip-blocks~%")
      (format out ";;; Generated: ~A~%" (multiple-value-bind (s m h d mo y) (get-decoded-time)
                                          (format nil "~4D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" y mo d h m s)))
      (format out ";;; DO NOT EDIT - regenerate with: make generate-geoip~%")
      (format out "~%")
      (format out "(in-package #:happening)~%~%")

      ;; Country codes
      (format out "(defparameter *embedded-country-codes*~%")
      (format out "  #(~{~S~^ ~}))~%~%" country-list)

      ;; IPv4 data
      (format out "(defparameter *embedded-ipv4-starts*~%")
      (format out "  #(~{~D~^ ~}))~%~%" (mapcar #'first ipv4-ranges))

      (format out "(defparameter *embedded-ipv4-ends*~%")
      (format out "  #(~{~D~^ ~}))~%~%" (mapcar #'second ipv4-ranges))

      (format out "(defparameter *embedded-ipv4-countries*~%")
      (format out "  #(~{~D~^ ~}))~%~%" (mapcar #'third ipv4-ranges))

      ;; IPv6 data
      (format out "(defparameter *embedded-ipv6-starts-hi*~%")
      (format out "  #(~{~D~^ ~}))~%~%" (mapcar #'first ipv6-ranges))

      (format out "(defparameter *embedded-ipv6-starts-lo*~%")
      (format out "  #(~{~D~^ ~}))~%~%" (mapcar #'second ipv6-ranges))

      (format out "(defparameter *embedded-ipv6-ends-hi*~%")
      (format out "  #(~{~D~^ ~}))~%~%" (mapcar #'third ipv6-ranges))

      (format out "(defparameter *embedded-ipv6-ends-lo*~%")
      (format out "  #(~{~D~^ ~}))~%~%" (mapcar #'fourth ipv6-ranges))

      (format out "(defparameter *embedded-ipv6-countries*~%")
      (format out "  #(~{~D~^ ~}))~%~%" (mapcar #'fifth ipv6-ranges)))

    (format t "Generated ~A~%" output-path)
    t))

;;; Run if executed as script
(let* ((script-dir (uiop:pathname-directory-pathname *load-truename*))
       (project-dir (uiop:pathname-parent-directory-pathname script-dir))
       (repo-path (merge-pathnames "data/country-ip-blocks/" project-dir))
       (output-path (merge-pathnames "src/geoip-embedded.lisp" project-dir)))
  (if (generate-geoip-data repo-path output-path)
      (format t "Success!~%")
      (progn
        (format *error-output* "Failed to generate GeoIP data~%")
        (uiop:quit 1))))
