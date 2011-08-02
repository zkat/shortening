(cl:defpackage #:shortening
  (:use #:cl #:alexandria #:hunchentoot #:py-configparser)
  (:export :init :*port* :*max-db-size*))
(cl:in-package #:shortening)

;; Config
(defparameter *port* 8181)
(defparameter *max-db-size* 50)

(defun load-config ()
  (when-let* ((config-path (probe-file (merge-pathnames ".shortening.conf" (user-homedir-pathname))))
              (config (handler-case (read-files (make-config) (list config-path))
                        (configparser-error () nil))))
    (when-let (port (ignore-errors
                      (parse-integer
                       (get-option config "conf" "port"))))
      (setf *port* port))
    (when-let (max-db-size (ignore-errors
                             (parse-integer
                              (get-option config "conf" "max-db-size"))))
      (setf *max-db-size* max-db-size))
    t))

(defun ensure-config ()
  (let ((path (merge-pathnames ".shortening.conf" (user-homedir-pathname))))
    (unless (probe-file path)
      (with-open-file (fd path :direction :output :if-exists :supersede)
        (with-input-from-string (s (format nil "[default]~%port = ~A~%max-db-size = ~A~%"
                                           *port* *max-db-size*))
          (copy-stream s fd)))
      t)))

;; Util
(defparameter *random-alphabet* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
(defun random-string (&optional (length 6))
  (map-into (make-array length :element-type 'character)
            (lambda ()
              (elt *random-alphabet* (random (length *random-alphabet*))))))

;; DB
(defvar *url-db* nil)
(defun find-url (short-url)
  (cdr (assoc short-url *url-db* :test #'string=)))

(defun truncate-db ()
  (when (> (length *url-db*) *max-db-size*)
    (setf *url-db* (subseq *url-db* 0 (1- (length *url-db*))))))

(defun unique-url ()
  (let ((url (concatenate 'string "/" (random-string))))
    (if (find-url url)
        (unique-url)
        url)))

(defun add-url (long-url)
  (let ((short (unique-url)))
    (push (cons short long-url)
          *url-db*)
    (truncate-db)
    short))

;; server
(defvar *acceptor* nil)
(defun init ()
  (ensure-config)
  (load-config)
  (when *acceptor*
    (stop *acceptor*)
    (setf *acceptor* nil))
  (setf *dispatch-table* (list (lambda (*request*)
                                 (unless (string= (script-name*) "/api")
                                   (when-let (target (find-url (script-name*)))
                                     (redirect target))))
                               'dispatch-easy-handlers
                               'default-dispatcher)
        *acceptor* (make-instance
                    'acceptor
                    :port *port*
                    :taskmaster (make-instance 'single-threaded-taskmaster)))
  (start *acceptor*))

(define-easy-handler (api :uri "/api") (url)
  (setf (content-type*) "text/plain")
  (when url (add-url url)))
