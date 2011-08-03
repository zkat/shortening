(cl:in-package :cl-user)
(ql:quickload 'shortening)
#+sbcl
(sb-ext:save-lisp-and-die "shortening" :executable t :toplevel #'shortening:init)
#+ccl
(ccl:save-application "shortening" :prepend-kernel t
                      :toplevel-function #'shortening:init
                      :error-handler :quit)
#+clisp
(ext:saveinitmem "shortening" :norc t :quiet t :executable t :init-function #'shortening:init)