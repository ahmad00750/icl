;;; regexp.lisp --- Regex visualization using Regulex railroad diagrams
;;;
;;; Usage: In ICL browser mode, load this file and visualize:
;;;   (load "examples/regexp.lisp")
;;;   ,viz (make-regexp "^[a-z]+@[a-z]+\\.com$")
;;;   ,viz (make-regexp "(foo|bar)+")
;;;   ,viz (make-regexp "https?://[^\\s]+")
;;;
;;; Uses the Regulex JavaScript library for railroad diagram visualization.
;;;
;;; ---------------------------------------------------------------------------
;;; Regex Wrapper Class
;;; ---------------------------------------------------------------------------

(defpackage :regexp-viz
  (:use :cl)
  (:export :make-regexp :regexp :regexp-pattern))

(in-package :regexp-viz)

(defclass regexp ()
  ((pattern :initarg :pattern :accessor regexp-pattern
            :documentation "The original regex pattern string."))
  (:documentation "A wrapper for regex patterns."))

(defun make-regexp (pattern)
  "Create a regexp object from a pattern string."
  (make-instance 'regexp :pattern pattern))

;;; ---------------------------------------------------------------------------
;;; ICL Visualization Integration
;;; ---------------------------------------------------------------------------

(defun register-icl-viz ()
  "Register regexp visualization with ICL."
  (defmethod icl-runtime:visualize ((obj regexp))
    (list :regexp (regexp-pattern obj))))

;;; ---------------------------------------------------------------------------
;;; Example Patterns
;;; ---------------------------------------------------------------------------

(defvar *email-pattern* (make-regexp "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")
  "A simple email validation pattern.")

(defvar *phone-pattern* (make-regexp "\\d{3}[-.\\s]?\\d{3}[-.\\s]?\\d{4}")
  "US phone number pattern.")

(defvar *url-pattern* (make-regexp "https?://[^\\s]+")
  "Simple URL pattern.")

(defvar *alternation-pattern* (make-regexp "(foo|bar|baz)+")
  "Alternation with repetition.")

;;; To test, run in ICL browser mode:
;;;   ,viz *email-pattern*       ; Email regex structure
;;;   ,viz *phone-pattern*       ; Phone number regex
;;;   ,viz *url-pattern*         ; URL regex
;;;   ,viz *alternation-pattern* ; Shows branching paths
;;;   ,viz (make-regexp "(a|b)*c")  ; Custom pattern
