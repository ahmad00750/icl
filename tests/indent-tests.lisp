;;; tests/indent-tests.lisp --- Tests for indentation functions
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite indent-tests
  :description "Tests for indentation functions"
  :in icl-tests)

(in-suite indent-tests)

;;; skip-whitespace tests

(test skip-whitespace-spaces
  "Test skipping spaces"
  (is (= (icl::skip-whitespace "   hello" 0) 3)))

(test skip-whitespace-tabs
  "Test skipping tabs"
  (is (= (icl::skip-whitespace "		hello" 0) 2)))

(test skip-whitespace-mixed
  "Test skipping mixed whitespace"
  (is (= (icl::skip-whitespace " 	 hello" 0) 3)))

(test skip-whitespace-no-ws
  "Test when no whitespace to skip"
  (is (= (icl::skip-whitespace "hello" 0) 0)))

(test skip-whitespace-from-middle
  "Test skipping from middle of string"
  (is (= (icl::skip-whitespace "foo   bar" 3) 6)))

(test skip-whitespace-all-ws
  "Test string that is all whitespace"
  (is (= (icl::skip-whitespace "   " 0) 3)))

;;; make-indent-string tests

(test make-indent-string-zero
  "Test creating zero-length indent"
  (is (string= (icl::make-indent-string 0) "")))

(test make-indent-string-small
  "Test creating small indent"
  (is (string= (icl::make-indent-string 2) "  "))
  (is (string= (icl::make-indent-string 4) "    ")))

(test make-indent-string-large
  "Test creating larger indent"
  (let ((indent (icl::make-indent-string 10)))
    (is (= (length indent) 10))
    (is (every (lambda (c) (char= c #\Space)) indent))))

;;; calculate-indent tests

(test calculate-indent-empty
  "Test indent after empty text"
  (is (= (icl::calculate-indent "") 0)))

(test calculate-indent-open-paren
  "Test indent after open paren"
  (is (= (icl::calculate-indent "(") 2)))

(test calculate-indent-after-defun
  "Test indent after defun header"
  (let ((indent (icl::calculate-indent "(defun foo (x)")))
    ;; Body of defun should be indented by 2
    (is (= indent 2))))

(test calculate-indent-let-body
  "Test indent for let body"
  (let ((indent (icl::calculate-indent "(let ((x 1))")))
    ;; Body of let should be indented by 2
    (is (= indent 2))))

(test calculate-indent-nested
  "Test indent for nested forms"
  (let ((indent (icl::calculate-indent "(defun foo (x)
  (let ((y 1))")))
    ;; Nested let body should align
    (is (>= indent 4))))

(test calculate-indent-after-close
  "Test indent after all parens closed"
  (is (= (icl::calculate-indent "(defun foo () nil)") 0)))

(test calculate-indent-cond
  "Test indent for cond body"
  ;; cond has body-start of 0, so all clauses get standard indent
  (let ((indent (icl::calculate-indent "(cond")))
    (is (= indent 2))))

(test calculate-indent-function-call
  "Test indent for function call args"
  ;; Regular function calls align to first arg position
  ;; "(format t" - first-arg-col is 0 (format starts right after paren)
  ;; so indent = column + 1 + first-arg-col = 0 + 1 + 0 = 1
  (let ((indent (icl::calculate-indent "(format t")))
    (is (= indent 1))))

;;; reindent-string tests

(test reindent-string-single-line
  "Test reindenting single line (unchanged)"
  (let ((input "(defun foo () nil)"))
    (is (string= (icl::reindent-string input) input))))

(test reindent-string-multiline
  "Test reindenting multi-line form"
  (let* ((input "(defun foo ()
nil)")
         (result (icl::reindent-string input)))
    ;; Second line should be indented
    (is (search "  nil" result))))

(test reindent-string-preserves-first-line
  "Test that first line indentation is preserved"
  (let* ((input "  (defun foo ()
nil)")
         (result (icl::reindent-string input)))
    ;; First line should start with spaces
    (is (char= (char result 0) #\Space))))
