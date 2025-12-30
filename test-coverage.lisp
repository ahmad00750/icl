#!/usr/bin/env sbcl --script
;;; test-coverage.lisp - Test sb-cover with ocicl
;;;
;;; Run with: sbcl --load test-coverage.lisp

(require 'sb-cover)

;; Enable coverage instrumentation
(declaim (optimize sb-cover:store-coverage-data))

;; Load ocicl with coverage
(format t "~&Loading ocicl with coverage...~%")
(let ((ocicl-asd (merge-pathnames "ocicl.asd" (user-homedir-pathname))))
  ;; Adjust path as needed
  (load "/home/green/git/ocicl/ocicl.asd"))

(asdf:load-system :ocicl :force t)

;; Disable coverage for subsequent loads
(declaim (optimize (sb-cover:store-coverage-data 0)))

(format t "~&Running (ocicl:main)...~%")
(handler-case
    (ocicl:main)
  (error (e)
    (format t "~&ocicl:main returned/errored: ~A~%" e)))

;; Generate HTML report
(format t "~&Generating coverage report...~%")
(sb-cover:report "/tmp/ocicl-coverage/")

(format t "~&Coverage report generated in /tmp/ocicl-coverage/~%")
(format t "~&Open /tmp/ocicl-coverage/cover-index.html in a browser.~%")
