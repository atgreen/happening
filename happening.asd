;;; happening.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green <green@moxielogic.org>

;;; Use pure-tls instead of cl+ssl (OpenSSL FFI) for TLS on all platforms.
;;; Load pure-tls/cl+ssl-compat first (provides cl+ssl-compatible API), then
;;; register "cl+ssl" as immutable so ASDF never tries to load the real cl+ssl.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :pure-tls/cl+ssl-compat)
  (asdf:register-immutable-system "cl+ssl"))

(asdf:defsystem "happening"
  :description "A privacy-focused, self-hosted web analytics platform"
  :author      "Anthony Green <green@moxielogic.org>"
  :license     "MIT"
  :version     "0.3.0"
  :depends-on  (:version-string
                :clingon
                :hunchentoot
                :easy-routes
                :llog
                :cl-dotenv
                :slynk
                ;; Database
                :cl-dbi
                :sxql
                :dbd-sqlite3
                ;; Crypto & utilities
                :ironclad
                :cl-json
                :cl-base64
                :flexi-streams
                :local-time
                :uuid
                :cl-ppcre
                :cl-who
                :alexandria
                :quri
                :drakma
                ;; TLS with automatic certificates (Hunchentoot integration)
                :pure-tls/acme+hunchentoot
                :usocket
                ;; TUI
                :tuition
                ;; Self-update
                :cl-selfupdate/drakma)
  :serial      t
  :components  ((:file "src/package")
                (:file "src/db")
                (:file "src/geoip-embedded")
                (:file "src/geoip")
                (:file "src/auth")
                (:file "src/sites")
                (:file "src/tracking")
                (:file "src/dashboard")
                (:file "src/setup")
                (:file "src/setup-tui")
                (:file "src/tls")
                (:file "src/static-assets")
                (:file "src/server")
                (:file "src/main"))
  :build-operation "program-op"
  :build-pathname "happening"
  :entry-point "happening:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
