;;; tests/package.lisp --- Test package for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(defpackage #:icl-tests
  (:use #:cl #:fiveam)
  (:export #:run-tests
           #:icl-tests))

(in-package #:icl-tests)

(def-suite icl-tests
  :description "ICL test suite")

(in-suite icl-tests)

(defun run-tests ()
  "Run all ICL tests."
  (run! 'icl-tests))
