#!/usr/bin/env sbcl --script
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
        (load quicklisp-init)))
(ql:quickload :plump :silent t)
(ql:quickload :dexador :silent t)
(ql:quickload :clss :silent t)

(defun parse-events (tl-doc)
  (print
   (loop
      for event across (clss:select ".ev-feed .ev-block" tl-doc)
      for classes = (plump:attribute event "class")
      for name = (plump:text (aref (clss:select "span[data-event-id]" event) 0))
      ; TODO filter by sc2
      for timer = (plump:text (aref (clss:select ".ev-timer" event) 0))
      for timer-text = (if (search "ev-live" classes) "LIVE" timer)
      collect (list  name timer-text)))
  )
(parse-events (plump:parse (dex:get "http://teamliquid.net/")))
