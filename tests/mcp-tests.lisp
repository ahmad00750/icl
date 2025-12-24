;;; tests/mcp-tests.lisp --- Tests for MCP server functions
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite mcp-tests
  :description "Tests for MCP server functions"
  :in icl-tests)

(in-suite mcp-tests)

;;; Path safety tests

(test safe-project-path-valid
  "Test valid project paths"
  (is (icl::safe-project-path-p "file.lisp"))
  (is (icl::safe-project-path-p "src/file.lisp"))
  (is (icl::safe-project-path-p "a/b/c/file.txt")))

(test safe-project-path-rejects-traversal
  "Test that .. paths are rejected"
  (is (not (icl::safe-project-path-p "../file.lisp")))
  (is (not (icl::safe-project-path-p "src/../../../etc/passwd")))
  (is (not (icl::safe-project-path-p "foo/bar/../../../baz"))))

(test safe-project-path-rejects-absolute
  "Test that absolute paths are rejected"
  (is (not (icl::safe-project-path-p "/etc/passwd")))
  (is (not (icl::safe-project-path-p "/home/user/file.lisp"))))

(test safe-glob-pattern-valid
  "Test valid glob patterns"
  (is (icl::safe-glob-pattern-p "*.lisp"))
  (is (icl::safe-glob-pattern-p "src/**/*.lisp"))
  (is (icl::safe-glob-pattern-p "file?.txt")))

(test safe-glob-pattern-rejects-traversal
  "Test that .. in glob patterns is rejected"
  (is (not (icl::safe-glob-pattern-p "../**/*.lisp")))
  (is (not (icl::safe-glob-pattern-p "src/../../*.lisp"))))

(test safe-glob-pattern-rejects-absolute
  "Test that absolute glob patterns are rejected"
  (is (not (icl::safe-glob-pattern-p "/*.lisp")))
  (is (not (icl::safe-glob-pattern-p "/etc/**/*"))))

;;; path-under-root-p tests

(test path-under-root-valid
  "Test paths correctly identified as under root"
  ;; Use actual existing paths - current directory and its parent
  (let* ((cwd (truename (uiop:getcwd)))
         (src-dir (truename (merge-pathnames "src/" cwd))))
    (is-true (icl::path-under-root-p src-dir cwd))))

(test path-under-root-invalid
  "Test paths correctly identified as NOT under root"
  ;; /home is not under the current working directory
  (let* ((cwd (truename (uiop:getcwd)))
         (root-dir (truename "/")))
    ;; Root directory is NOT under cwd
    (is-false (icl::path-under-root-p root-dir cwd))))

;;; Session token tests

(test session-token-length
  "Test that session tokens are 32 hex characters"
  (let ((token (icl::generate-session-token)))
    (is (= (length token) 32))
    (is (every (lambda (c) (or (digit-char-p c) (find c "abcdefABCDEF"))) token))))

(test session-token-uniqueness
  "Test that consecutive tokens are different"
  (let ((token1 (icl::generate-session-token))
        (token2 (icl::generate-session-token)))
    (is (not (string= token1 token2)))))
