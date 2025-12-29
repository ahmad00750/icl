;;; tests/command-tests.lisp --- Tests for command functions
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite command-tests
  :description "Tests for command functions"
  :in icl-tests)

(in-suite command-tests)

;;; Package context tests for symbol reading

(test read-symbol-in-current-package
  "Test that symbols are read in the current *icl-package* context"
  ;; Create a test package with a test symbol
  (let ((test-pkg (or (find-package :icl-test-pkg)
                      (make-package :icl-test-pkg))))
    (unwind-protect
        (progn
          ;; Intern a symbol in our test package
          (intern "TEST-SYMBOL" test-pkg)
          ;; Set ICL's package context to our test package
          (let ((icl::*icl-package* test-pkg))
            ;; Read a symbol name - should intern in test package
            (let ((sym (let ((*package* (or icl::*icl-package* *package*)))
                         (read-from-string "TEST-SYMBOL"))))
              (is (eq (symbol-package sym) test-pkg)
                  "Symbol should be read in *icl-package* context"))))
      ;; Cleanup
      (delete-package test-pkg))))

(test read-symbol-defaults-to-current-package
  "Test that symbol reading defaults to *package* when *icl-package* is nil"
  (let ((icl::*icl-package* nil))
    (let ((sym (let ((*package* (or icl::*icl-package* *package*)))
                 (read-from-string "SOME-SYMBOL"))))
      ;; Should be read in the current package (icl-tests)
      (is (eq (symbol-package sym) (find-package :icl-tests))
          "Symbol should default to current *package* when *icl-package* is nil"))))

(test read-qualified-symbol-ignores-context
  "Test that fully qualified symbols ignore package context"
  (let ((test-pkg (or (find-package :icl-test-pkg2)
                      (make-package :icl-test-pkg2))))
    (unwind-protect
        (let ((icl::*icl-package* test-pkg))
          ;; Reading CL:CAR should always return the CL symbol
          (let ((sym (let ((*package* (or icl::*icl-package* *package*)))
                       (read-from-string "CL:CAR"))))
            (is (eq sym 'cl:car)
                "Qualified symbols should resolve to their specified package")))
      (delete-package test-pkg))))

;;; Disassemble command symbol resolution tests

(test dis-resolves-symbol-in-icl-package
  "Test that ,dis resolves symbols using *icl-package* context"
  ;; This tests the exact logic used in slynk-disassemble
  (let ((test-pkg (or (find-package :icl-dis-test-pkg)
                      (make-package :icl-dis-test-pkg))))
    (unwind-protect
        (progn
          ;; Intern a function symbol in test package
          (intern "MY-FUNCTION" test-pkg)
          ;; Simulate being in test package via *icl-package*
          (let ((icl::*icl-package* test-pkg))
            ;; This is the exact pattern from slynk-disassemble
            (let* ((name "my-function")
                   (sym (let ((*package* (or icl::*icl-package* *package*)))
                          (read-from-string (string-upcase name))))
                   (full-name (if (symbol-package sym)
                                  (format nil "~A::~A"
                                          (package-name (symbol-package sym))
                                          (symbol-name sym))
                                  (symbol-name sym))))
              ;; Should produce fully qualified name in test package
              (is (string= full-name "ICL-DIS-TEST-PKG::MY-FUNCTION")
                  "Symbol should be qualified with *icl-package*"))))
      (delete-package test-pkg))))

(test dis-resolves-cl-symbol-correctly
  "Test that ,dis resolves CL symbols regardless of *icl-package*"
  ;; Use CL-USER which inherits from CL
  (let ((icl::*icl-package* (find-package :cl-user)))
    ;; CAR should resolve to CL:CAR
    (let* ((name "car")
           (sym (let ((*package* (or icl::*icl-package* *package*)))
                  (read-from-string (string-upcase name))))
           (full-name (if (symbol-package sym)
                          (format nil "~A::~A"
                                  (package-name (symbol-package sym))
                                  (symbol-name sym))
                          (symbol-name sym))))
      ;; CAR is inherited from CL, so should be CL::CAR
      (is (string= full-name "COMMON-LISP::CAR")
          "CL symbols should resolve to COMMON-LISP package"))))

(test dis-handles-explicit-package-prefix
  "Test that ,dis handles explicitly qualified symbols"
  (let ((icl::*icl-package* (find-package :cl-user)))
    ;; Explicitly qualified symbol should use that package
    (let* ((name "cl:length")
           (sym (let ((*package* (or icl::*icl-package* *package*)))
                  (read-from-string (string-upcase name))))
           (full-name (if (symbol-package sym)
                          (format nil "~A::~A"
                                  (package-name (symbol-package sym))
                                  (symbol-name sym))
                          (symbol-name sym))))
      (is (string= full-name "COMMON-LISP::LENGTH")
          "Explicitly qualified symbols should resolve correctly"))))

;;; Command parsing tests

(test split-command-line-simple
  "Test splitting simple command arguments"
  (let ((parts (icl::split-command-line "arg1 arg2 arg3")))
    (is (equal parts '("arg1" "arg2" "arg3")))))

(test split-command-line-quoted
  "Test splitting command with quoted arguments (quotes preserved)"
  (let ((parts (icl::split-command-line "arg1 \"arg with spaces\" arg3")))
    (is (equal parts '("arg1" "\"arg with spaces\"" "arg3")))))

(test split-command-line-empty
  "Test splitting empty command line"
  (let ((parts (icl::split-command-line "")))
    (is (null parts))))
