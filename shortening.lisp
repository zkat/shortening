(cl:defpackage #:shortening
  (:use #:cl #:alexandria #:hunchentoot #:py-configparser #:yaclml)
  (:export :init))
(cl:in-package #:shortening)

;; Config
(defparameter *port* 8181)
(defparameter *max-db-size* 100)
(defparameter *url-length* 6)
(defvar *external-db* nil)

(defparameter *config-locations* (list (merge-pathnames ".shortening.conf" (user-homedir-pathname))
                                       #P"/etc/shortening.conf"))
(defun load-config ()
  (when-let* ((config-path (find-if #'probe-file *config-locations*))
              (config (handler-case (read-files (make-config) (list config-path))
                        (configparser-error ()
                          (format *error-output*
                                  "~&Error while loading config file ~A. Using defaults.~%"
                                  config-path)))))
    (flet ((conf (id &optional (value-processor #'parse-integer)
                     &aux (symbol (intern (format nil "*~:@(~A~)*" id)
                                          (find-package :shortening)))
                    (name (string-upcase (string id))))
             (setf (symbol-value symbol)
                   (or (ignore-errors (funcall value-processor (get-option config "shortening" name)))
                       (when (boundp symbol) (symbol-value symbol))))))
      (mapcar #'conf '(port max-db-size url-length))
      (conf 'external-db (conjoin (compose #'not #'emptyp) #'pathname)))
    t))

;; Util
(defparameter *random-alphabet* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
(defun random-string (&optional (length 6))
  (map-into (make-array length :element-type 'character)
            (curry #'random-elt *random-alphabet*)))

;; DB
(defvar *url-db* nil)
(defstruct (url (:constructor make-url (expansion &optional origin))
                (:type list))
  expansion origin)

(defun init-db ()
  (when (probe-file *external-db*)
    (setf *url-db* (with-open-file (fd *external-db*
                                       :if-does-not-exist nil)
                     (when fd
                       (let (*read-eval*) (read fd nil)))))))

(defun find-url (short-url)
  (when-let (url (cdr (assoc short-url *url-db* :test #'string=)))
    (url-expansion url)))

(defun truncate-db ()
  (when (> (length *url-db*) *max-db-size*)
    (setf *url-db* (subseq *url-db* 0 (1- (length *url-db*))))))

(defun unique-url ()
  (let ((url (concatenate 'string "/" (random-string *url-length*))))
    (if (or (find-url url) (string= url "/api"))
        (unique-url)
        url)))

(defun add-url (long-url &optional origin)
  (let ((short (unique-url)))
    (push (cons short (make-url long-url origin))
          *url-db*)
    (truncate-db)
    (when *external-db*
      (with-open-file (fd (ensure-directories-exist *external-db*)
                          :direction :output
                          :if-exists :supersede)
        (prin1 *url-db* fd)))
    short))

;; server
(defun 404-handler ()
  (setf (return-code*) +http-not-found+)
  "Page not found")

(define-easy-handler (home :uri "/") ((plainp :parameter-type 'boolean))
  (if plainp
      (prin1 *url-db*)
      (with-yaclml-output-to-string
        (<:html :prologue "<!DOCTYPE html>"
                (<:body
                 (<:h1 "Submit a URL")
                 (<:form :method :post :action "/api"
                         (<:label :for "url" "URL: ")
                         (<:input :id "url" :name "url")
                         (<:input :type "hidden" :name "origin" :value "web-form")
                         (<:submit :value "shorten!"))
                 (<:h1 "Links")
                 (<:ul
                  (loop for (short . url) in *url-db*
                     do (<:li (<:p (<:ah short) " - " (<:href (url-expansion url)
                                                              (<:ah (url-expansion url)))
                                   (when-let (origin (url-origin url))
                                     (<:ah " (origin: " origin ")")))))))))))

(define-easy-handler (api :uri "/api") (url origin)
  (setf (content-type*) "text/plain")
  (when url (add-url url origin)))

(defmacro exit-on-error (&body body)
  `(#+sbcl progn #+sbcl (setf sb-ext:*invoke-debugger-hook*
                              (lambda (c prev)
                                (declare (ignore prev))
                                (format *error-output* "~&~A~%" c)
                                (sb-ext:quit)))
    #+clisp ext:exit-on-error
    #-(or sbcl clisp) progn
    ,@body))

(defun init ()
  (exit-on-error
   (load-config)
   (init-db)
   (push (lambda (*request*)
           (unless (string= (script-name*) "/api")
             (when-let (target (find-url (script-name*)))
               (redirect target))))
         *dispatch-table*)
   (setf *default-handler* '404-handler)
   (pushnew +http-not-found+ *approved-return-codes*)
   (start (make-instance 'acceptor :port *port*
                         :taskmaster (make-instance 'single-threaded-taskmaster)))))
