;;; tests/theme-tests.lisp --- Tests for theme functions
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite theme-tests
  :description "Tests for theme functions"
  :in icl-tests)

(in-suite theme-tests)

;; Ensure themes are initialized before running tests
(icl::initialize-themes)

;;; Theme lookup tests (themes use keywords, not strings)

(test find-terminal-theme-exists
  "Test finding existing terminal themes"
  (is-true (icl::find-terminal-theme :dracula))
  (is-true (icl::find-terminal-theme :monokai))
  (is-true (icl::find-terminal-theme :solarized-dark)))

(test find-terminal-theme-not-exists
  "Test finding non-existent terminal theme returns NIL"
  (is-false (icl::find-terminal-theme :nonexistent-theme-xyz)))

(test find-browser-theme-exists
  "Test finding existing browser themes"
  (is-true (icl::find-browser-theme :dracula))
  (is-true (icl::find-browser-theme :monokai)))

(test find-browser-theme-not-exists
  "Test finding non-existent browser theme returns NIL"
  (is-false (icl::find-browser-theme :nonexistent-theme-xyz)))

;;; Theme listing tests

(test list-terminal-themes-not-empty
  "Test that terminal theme list is not empty"
  (let ((themes (icl::list-terminal-themes)))
    (is-true (listp themes))
    (is-true (> (length themes) 0))))

(test list-browser-themes-not-empty
  "Test that browser theme list is not empty"
  (let ((themes (icl::list-browser-themes)))
    (is-true (listp themes))
    (is-true (> (length themes) 0))))

;;; Theme structure tests

(test terminal-theme-has-required-fields
  "Test that terminal themes have required fields"
  (let ((theme (icl::find-terminal-theme :dracula)))
    (is-true (icl::terminal-theme-p theme))
    (is-true (keywordp (icl::terminal-theme-name theme)))
    (is-true (stringp (icl::terminal-theme-display-name theme)))))

(test browser-theme-has-required-fields
  "Test that browser themes have required fields"
  (let ((theme (icl::find-browser-theme :dracula)))
    (is-true (icl::browser-theme-p theme))
    (is-true (keywordp (icl::browser-theme-name theme)))
    (is-true (stringp (icl::browser-theme-display-name theme)))))

;;; alist-to-hash tests

(test alist-to-hash-empty
  "Test converting empty alist"
  (let ((ht (icl::alist-to-hash nil)))
    (is (= (hash-table-count ht) 0))))

(test alist-to-hash-simple
  "Test converting simple alist"
  (let ((ht (icl::alist-to-hash '(("a" . 1) ("b" . 2)))))
    (is (= (hash-table-count ht) 2))
    (is (= (gethash "a" ht) 1))
    (is (= (gethash "b" ht) 2))))
