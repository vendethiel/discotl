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

(defconstant +sc2-css+ ".ev[style='background: url(/images/games/1.png) transparent no-repeat']")
(defconstant +default-lang+ "gb")
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
  (labels
      ((approved-event (name)
         (not (member name *sc2-ignores* :test 'equal)))
       (has-elements (seq)
         (plusp (length seq)))
       (attr-first (els attr)
         (plump:attribute (aref els 0) attr))
       (stream-name (a)
         (format nil "http://teamliquid.net~a" (attr-first a "href")))
       (stream-lang (imgs)
         (if (plusp (length imgs)) ; 0 or 1
             ;; extract flag
             (pathname-name (pathname (attr-first imgs "src")))
             +default-lang+))
       (get-streams (name stream-spans)
         (alexandria:if-let (stream (gethash name *sc2-streams*))
           (list (list +default-lang+ stream)) ; single result if listed
           (map 'list
                (lambda (span)
                  (list (stream-lang (clss:select "img" span))
                        (stream-name (clss:select "a" span))))
                stream-spans))))
    (loop
      for event across (clss:select ".ev-feed .ev-block" tl-doc)
      for classes = (plump:attribute event "class")
      for name = (plump:text (aref (clss:select "span[data-event-id]" event) 0))
      for timer = (plump:text (aref (clss:select ".ev-timer" event) 0))

      for is-live = (search "ev-live" classes)
      for stream-spans = (clss:select ".ev-stream > div > span:first-child" event)
      ;for stream-spans = (map 'list (lambda (div) (aref (clss:select "span" div) 0))
      ;                        (clss:select ".ev-stream" event))
      for result = (list name
                         (unless is-live timer)
                         (if is-live (get-streams name stream-spans)))

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
           (format t "~a~@[ - ~a~]~%~:{       :flag_~a: <~a>~%~}" name timer stream)

           )))
     events)
    (write-char #\Newline)))

(destructuring-bind (live upcoming) (parse-events (plump:parse (dex:get "http://teamliquid.net/")))
  (print-events "live" live)
  (print-events "upcoming" upcoming))
