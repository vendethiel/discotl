#!/usr/bin/env sbcl --script
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
        (load quicklisp-init)))
(ql:quickload :alexandria :silent t)
(ql:quickload :dexador :silent t)
(ql:quickload :plump :silent t)
(ql:quickload :clss :silent t)

(defconstant +sc2-css+ ".ev[style='background: url(/images/games/1.png) transparent no-repeat']" )
(defvar *sc2-streams* (make-hash-table :test 'equal))
(with-open-file (stream "streams")
  (loop
     for key = (read-line stream nil)
     for value = (read-line stream nil)
     until (or (null key) (null value))
     do (setf (gethash key *sc2-streams*) value)))

(defvar *sc2-ignores*
  (with-open-file (stream "ignore")
    (loop
       for line = (read-line stream nil)
       until (null line)
       collect line)))

(defun parse-events (tl-doc)
  (flet ((approved-event (name) (not (member name *sc2-ignores* :test 'equal)))
         (has-elements (seq) (plusp (length seq))))
    (loop
       for event across (clss:select ".ev-feed .ev-block" tl-doc)
       for classes = (plump:attribute event "class")
       for name = (plump:text (aref (clss:select "span[data-event-id]" event) 0))
       for timer = (plump:text (aref (clss:select ".ev-timer" event) 0))

       for is-live = (search "ev-live" classes)
       for stream-a = (clss:select ".ev-stream a" event) ; find TL stream link (XXX the english stream SHOULD always be first)
       for stream-tl = (when (and is-live (has-elements stream-a))
                         (format nil "http://teamliquid.net~a" (plump:attribute (aref stream-a 0) "href")))
       for stream = (or (gethash name *sc2-streams*) stream-tl)
       for result = (list name (if is-live nil timer) stream)

       when (and (approved-event name) (has-elements (clss:select +sc2-css+ event)))
        if is-live collect result into live
        else collect result into upcoming
       finally (return (list live upcoming)))))

(defun print-events (header events)
  (when events
    (format t " # ~:@(~a~)~%~%" header)
    (mapc
     (lambda (event)
       (destructuring-bind (name timer stream) event
         (unless (search "days" timer) ; remove event in the "far" future (2d+)
           (format t "~a~@[ - ~a~]~@[ - ~a~]~%" name timer stream))))
     events)
    (write-char #\Newline)
    ))

(destructuring-bind (live upcoming) (parse-events (plump:parse (dex:get "http://teamliquid.net/")))
  (print-events "live" live)
  (print-events "upcoming" upcoming))
