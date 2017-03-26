#!/usr/bin/env sbcl --script
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
        (load quicklisp-init)))
(ql:quickload :plump :silent t)
(ql:quickload :dexador :silent t)

(print (plump:parse (dex:get "http://google.fr/")))
