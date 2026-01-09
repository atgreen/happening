;;; happening.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green <green@moxielogic.org>

(asdf:defsystem "happening"
  :description "A privacy-focused, self-hosted web analytics platform"
  :author      "Anthony Green <green@moxielogic.org>"
  :license     "MIT"
  :version     "0.2.0"
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
                :tuition)
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
