;;;; shortening.asd

(asdf:defsystem #:shortening
  :serial t
  :depends-on (#:hunchentoot #:alexandria #:py-configparser #:yaclml)
  :license "Public Domain"
  :components ((:file "shortening")))

