#!/usr/bin/env sbcl --script
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
        (load quicklisp-init)))
(ql:quickload :plump :silent t)
(ql:quickload :dexador :silent t)
(ql:quickload :clss :silent t)

(defconstant +sc2-css+ ".ev[style='background: url(/images/games/1.png) transparent no-repeat']" )
(defvar *sc2-streams* (make-hash-table :test 'equal))
(with-open-file (stream "streams")
  (loop
     for key = (read-line stream nil)
     for value = (read-line stream nil)
     until (or (null key) (null value))
     do (setf (gethash key *sc2-streams*) value)))

(defun parse-events (tl-doc)
  (loop
     for event across (clss:select ".ev-feed .ev-block" tl-doc)
     for classes = (plump:attribute event "class")
     for name = (plump:text (aref (clss:select "span[data-event-id]" event) 0))
     for timer = (plump:text (aref (clss:select ".ev-timer" event) 0))
     for timer-text = (if (search "ev-live" classes) "LIVE" timer)
     when (plusp (length (clss:select +sc2-css+ event)))
     collect (list name timer-text (gethash name *sc2-streams*))))
(parse-events (plump:parse (dex:get "http://teamliquid.net/")))
