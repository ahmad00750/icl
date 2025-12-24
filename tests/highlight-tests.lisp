;;; tests/highlight-tests.lisp --- Tests for syntax highlighting functions
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite highlight-tests
  :description "Tests for syntax highlighting functions"
  :in icl-tests)

(in-suite highlight-tests)

;;; whitespace-char-p tests

(test whitespace-char-space
  "Test space is whitespace"
  (is-true (icl::whitespace-char-p #\Space)))

(test whitespace-char-tab
  "Test tab is whitespace"
  (is-true (icl::whitespace-char-p #\Tab)))

(test whitespace-char-newline
  "Test newline is whitespace"
  (is-true (icl::whitespace-char-p #\Newline)))

(test whitespace-char-return
  "Test carriage return is whitespace"
  (is-true (icl::whitespace-char-p #\Return)))

(test whitespace-char-non-whitespace
  "Test non-whitespace characters"
  (is-false (icl::whitespace-char-p #\a))
  (is-false (icl::whitespace-char-p #\0))
  (is-false (icl::whitespace-char-p #\-)))

;;; constituent-char-p tests

(test constituent-char-alphanumeric
  "Test alphanumeric chars are constituent"
  (is-true (icl::constituent-char-p #\a))
  (is-true (icl::constituent-char-p #\Z))
  (is-true (icl::constituent-char-p #\5)))

(test constituent-char-symbol-chars
  "Test symbol chars are constituent"
  (is-true (icl::constituent-char-p #\-))
  (is-true (icl::constituent-char-p #\*))
  (is-true (icl::constituent-char-p #\+)))

(test constituent-char-non-constituent
  "Test non-constituent characters"
  (is-false (icl::constituent-char-p #\Space))
  (is-false (icl::constituent-char-p #\())
  (is-false (icl::constituent-char-p #\)))
  (is-false (icl::constituent-char-p #\"))
  (is-false (icl::constituent-char-p #\;))
  (is-false (icl::constituent-char-p nil)))

;;; tokenize-lisp tests

(test tokenize-lisp-empty
  "Test tokenizing empty string"
  (is (null (icl::tokenize-lisp ""))))

(test tokenize-lisp-simple-symbol
  "Test tokenizing simple symbol"
  (let ((tokens (icl::tokenize-lisp "defun")))
    (is (= (length tokens) 1))
    (is (eql (first (first tokens)) :special))))

(test tokenize-lisp-string
  "Test tokenizing string literal"
  (let ((tokens (icl::tokenize-lisp "\"hello\"")))
    (is (= (length tokens) 1))
    (is (eql (first (first tokens)) :string))))

(test tokenize-lisp-comment
  "Test tokenizing comment"
  (let ((tokens (icl::tokenize-lisp "; comment")))
    (is (= (length tokens) 1))
    (is (eql (first (first tokens)) :comment))))

(test tokenize-lisp-keyword
  "Test tokenizing keyword"
  (let ((tokens (icl::tokenize-lisp ":test")))
    (is (= (length tokens) 1))
    (is (eql (first (first tokens)) :keyword))))

(test tokenize-lisp-number
  "Test tokenizing number"
  (let ((tokens (icl::tokenize-lisp "42")))
    (is (= (length tokens) 1))
    (is (eql (first (first tokens)) :number))))

(test tokenize-lisp-parens
  "Test tokenizing parentheses"
  (let ((tokens (icl::tokenize-lisp "()")))
    (is (= (length tokens) 2))
    (is (eql (first (first tokens)) :paren))
    (is (eql (first (second tokens)) :paren))))

(test tokenize-lisp-complex-form
  "Test tokenizing complex Lisp form"
  (let ((tokens (icl::tokenize-lisp "(defun foo (x) x)")))
    ;; Should have: ( defun foo ( x ) x )
    (is (>= (length tokens) 7))
    ;; First token should be paren
    (is (eql (first (first tokens)) :paren))
    ;; Second should be special form
    (is (eql (first (second tokens)) :special))))

(test tokenize-lisp-builtin
  "Test tokenizing builtin function"
  (let ((tokens (icl::tokenize-lisp "mapcar")))
    (is (= (length tokens) 1))
    (is (eql (first (first tokens)) :builtin))))

(test tokenize-lisp-quote
  "Test tokenizing quote character"
  (let ((tokens (icl::tokenize-lisp "'foo")))
    (is (= (length tokens) 2))
    (is (eql (first (first tokens)) :quote))))

;;; find-matching-paren tests

(test find-matching-paren-simple
  "Test finding matching paren in simple form"
  (let* ((str "()")
         (tokens (icl::tokenize-lisp str)))
    (is (= (icl::find-matching-paren str 0 tokens) 1))))

(test find-matching-paren-nested
  "Test finding matching paren in nested form"
  (let* ((str "((inner))")
         (tokens (icl::tokenize-lisp str)))
    (is (= (icl::find-matching-paren str 0 tokens) 8))))

(test find-matching-paren-backward
  "Test finding matching paren searching backward"
  (let* ((str "(test)")
         (tokens (icl::tokenize-lisp str)))
    (is (= (icl::find-matching-paren str 5 tokens) 0))))

(test find-matching-paren-unmatched
  "Test unmatched paren returns NIL"
  (let* ((str "(unclosed")
         (tokens (icl::tokenize-lisp str)))
    (is-false (icl::find-matching-paren str 0 tokens))))

;;; position-in-string-or-comment-p tests

(test position-in-string
  "Test detecting position inside string"
  (let ((tokens (icl::tokenize-lisp "\"hello\"")))
    ;; Position 3 is inside the string (between h and e)
    (is-true (icl::position-in-string-or-comment-p 3 tokens))))

(test position-in-comment
  "Test detecting position inside comment"
  (let ((tokens (icl::tokenize-lisp "; comment")))
    ;; Position 3 is inside the comment
    (is-true (icl::position-in-string-or-comment-p 3 tokens))))

(test position-outside-string-comment
  "Test detecting position outside string/comment"
  (let ((tokens (icl::tokenize-lisp "defun")))
    (is-false (icl::position-in-string-or-comment-p 2 tokens))))

;;; display-width tests

(test display-width-plain
  "Test display width of plain text"
  (is (= (icl::display-width "hello") 5))
  (is (= (icl::display-width "") 0)))

(test display-width-with-ansi
  "Test display width ignores ANSI codes"
  (let ((colored (format nil "~C[31mhello~C[0m" #\Escape #\Escape)))
    (is (= (icl::display-width colored) 5))))

(test display-width-multiple-ansi
  "Test display width with multiple ANSI codes"
  (let ((styled (format nil "~C[1m~C[34mtest~C[0m" #\Escape #\Escape #\Escape)))
    (is (= (icl::display-width styled) 4))))
