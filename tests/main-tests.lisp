;;; tests/main-tests.lisp --- Tests for CLI functions
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite main-tests
  :description "Tests for CLI functions"
  :in icl-tests)

(in-suite main-tests)

;;; parse-connect-string tests

(test parse-connect-string-with-port
  "Test parsing host:port format"
  (multiple-value-bind (host port)
      (icl::parse-connect-string "localhost:4005")
    (is (string= host "localhost"))
    (is (= port 4005))))

(test parse-connect-string-host-only
  "Test parsing host-only format (uses default port)"
  (multiple-value-bind (host port)
      (icl::parse-connect-string "myhost")
    (is (string= host "myhost"))
    (is (= port icl::*slynk-port*))))

(test parse-connect-string-ipv4
  "Test parsing IPv4:port format"
  (multiple-value-bind (host port)
      (icl::parse-connect-string "192.168.1.1:5000")
    (is (string= host "192.168.1.1"))
    (is (= port 5000))))

(test parse-connect-string-invalid-port
  "Test that invalid port strings signal an error"
  (signals error
    (icl::parse-connect-string "localhost:abc")))

(test parse-connect-string-port-out-of-range
  "Test that port > 65535 signals an error"
  (signals error
    (icl::parse-connect-string "localhost:70000")))

(test parse-connect-string-negative-port
  "Test that port 0 signals an error"
  (signals error
    (icl::parse-connect-string "localhost:0")))
