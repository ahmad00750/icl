;;; tests/output-tests.lisp --- Tests for output formatting functions
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite output-tests
  :description "Tests for output formatting functions"
  :in icl-tests)

(in-suite output-tests)

;;; visible-string-length tests

(test visible-string-length-plain
  "Test visible length of plain strings"
  (is (= (icl::visible-string-length "hello") 5))
  (is (= (icl::visible-string-length "") 0))
  (is (= (icl::visible-string-length "hello world") 11)))

(test visible-string-length-with-ansi
  "Test visible length ignores ANSI escape sequences"
  ;; ESC[31m = red, ESC[0m = reset
  (let ((red-text (format nil "~C[31mhello~C[0m" #\Escape #\Escape)))
    (is (= (icl::visible-string-length red-text) 5)))
  ;; Multiple ANSI codes
  (let ((styled (format nil "~C[1m~C[34mtest~C[0m" #\Escape #\Escape #\Escape)))
    (is (= (icl::visible-string-length styled) 4))))

(test visible-string-length-256-color
  "Test visible length with 256-color codes"
  ;; ESC[38;5;196m = 256-color red
  (let ((colored (format nil "~C[38;5;196mtext~C[0m" #\Escape #\Escape)))
    (is (= (icl::visible-string-length colored) 4))))

;;; list-length-bounded tests
;;; Note: function returns 0-based count (iterations), not actual length

(test list-length-bounded-short
  "Test bounded length for short lists"
  (is (= (icl::list-length-bounded '(1 2 3) 10) 2))
  (is (= (icl::list-length-bounded nil 10) 0))
  (is (= (icl::list-length-bounded '(a) 10) 0)))

(test list-length-bounded-at-max
  "Test bounded length at exactly max"
  (is (= (icl::list-length-bounded '(1 2 3 4 5) 5) 4)))

(test list-length-bounded-exceeds-max
  "Test bounded length returns max+1 for long lists"
  (is (= (icl::list-length-bounded '(1 2 3 4 5 6) 5) 5))
  (is (= (icl::list-length-bounded '(1 2 3 4 5 6 7 8 9 10) 3) 4)))

;;; count-display-lines tests

(test count-display-lines-simple
  "Test line counting for simple text"
  (is (= (icl::count-display-lines "hello" 80) 1))
  (is (= (icl::count-display-lines "" 80) 0))
  (is (= (icl::count-display-lines "line1
line2
line3" 80) 3)))

(test count-display-lines-wrapping
  "Test line counting with wrapping"
  ;; 20 chars in 10-char width = 2 lines
  (is (= (icl::count-display-lines "12345678901234567890" 10) 2))
  ;; 25 chars in 10-char width = 3 lines
  (is (= (icl::count-display-lines "1234567890123456789012345" 10) 3)))

(test count-display-lines-with-ansi
  "Test line counting ignores ANSI codes"
  ;; Text is "hello" (5 chars) but with ANSI codes
  (let ((text (format nil "~C[31mhello~C[0m" #\Escape #\Escape)))
    (is (= (icl::count-display-lines text 80) 1))
    ;; Should not wrap despite ANSI code characters
    (is (= (icl::count-display-lines text 5) 1))))

(test count-display-lines-newlines-and-wrap
  "Test line counting with both newlines and wrapping"
  ;; First line wraps (15 chars in 10-width = 2 lines)
  ;; Second line is 5 chars = 1 line
  ;; Total = 3 lines
  (is (= (icl::count-display-lines "123456789012345
hello" 10) 3)))

;;; colorize tests

(test colorize-with-colors-disabled
  "Test colorize returns plain text when colors disabled"
  (let ((icl::*colors-enabled* nil))
    (is (string= (icl::colorize "test" icl::*color-red*) "test"))))

;;; no-color-p tests

(test no-color-env-not-set
  "Test no-color-p returns NIL when NO_COLOR not set"
  ;; Save and clear NO_COLOR
  (let ((saved (uiop:getenv "NO_COLOR")))
    (unwind-protect
         (progn
           (setf (uiop:getenv "NO_COLOR") nil)
           ;; NO_COLOR not set should return NIL
           (is-false (icl::no-color-p)))
      (when saved
        (setf (uiop:getenv "NO_COLOR") saved)))))
