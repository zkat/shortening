;;;; shortening.asd

(asdf:defsystem #:shortening
  :serial t
  :depends-on (#:hunchentoot #:alexandria #:py-configparser)
  :license "Public Domain"
  :components ((:file "shortening")))

