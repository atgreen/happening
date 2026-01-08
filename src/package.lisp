;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Package definition for Happening analytics

(defpackage #:happening
  (:use #:cl)
  (:documentation "Happening - A privacy-focused, self-hosted web analytics platform")
  (:export
   ;; Main entry point
   #:main

   ;; Database
   #:init-database
   #:close-database
   #:get-config
   #:set-config

   ;; GeoIP
   #:load-geoip-database
   #:geoip-available-p
   #:lookup-country

   ;; Server
   #:start-server
   #:stop-server

   ;; Sites
   #:create-site
   #:get-site
   #:list-sites
   #:delete-site
   #:*base-url*

   ;; Users
   #:create-user
   #:authenticate-user

   ;; Dashboard
   #:get-dashboard-stats

   ;; TLS (automatic ACME certificate management)
   #:make-happening-acme-acceptor
   #:https-configured-p
   #:configured-domain
   #:*cert-store-path*
   #:*default-renewal-days*))
