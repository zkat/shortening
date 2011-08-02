;;;; shortening.asd

(asdf:defsystem #:shortening
  :serial t
  :depends-on (#:hunchentoot #:alexandria #:py-configparser)
  :components ((:file "shortening")))

