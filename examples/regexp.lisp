;;; regexp.lisp --- Regex visualization using CL-PPCRE parse trees
;;;
;;; Usage: In ICL browser mode, load this file and visualize:
;;;   (load "examples/regexp.lisp")
;;;   ,viz (make-regexp "^[a-z]+@[a-z]+\\.com$")
;;;   ,viz (make-regexp "(foo|bar)+")
;;;   ,viz (make-regexp "\\d{3}-\\d{4}")
;;;
;;; ---------------------------------------------------------------------------
;;; Regex Wrapper Class
;;; ---------------------------------------------------------------------------

(defpackage :regexp-viz
  (:use :cl)
  (:export :make-regexp :regexp :regexp-pattern :regexp-tree))

(in-package :regexp-viz)

(defclass regexp ()
  ((pattern :initarg :pattern :accessor regexp-pattern
            :documentation "The original regex pattern string.")
   (tree :initarg :tree :accessor regexp-tree
         :documentation "The parsed CL-PPCRE parse tree."))
  (:documentation "A wrapper for regex patterns with their parse trees."))

(defun make-regexp (pattern)
  "Create a regexp object from a pattern string."
  (make-instance 'regexp
                 :pattern pattern
                 :tree (cl-ppcre:parse-string pattern)))

;;; ---------------------------------------------------------------------------
;;; Parse Tree to Mermaid Conversion
;;; ---------------------------------------------------------------------------

(defvar *node-counter* 0
  "Counter for generating unique node IDs.")

(defun reset-counter ()
  (setf *node-counter* 0))

(defun next-node-id ()
  (format nil "n~D" (incf *node-counter*)))

(defun escape-mermaid (str)
  "Escape special characters for Mermaid labels."
  (with-output-to-string (out)
    (loop for char across (princ-to-string str)
          do (case char
               (#\" (write-string "#quot;" out))
               (#\< (write-string "#lt;" out))
               (#\> (write-string "#gt;" out))
               (#\& (write-string "#amp;" out))
               (#\[ (write-string "#91;" out))
               (#\] (write-string "#93;" out))
               (#\{ (write-string "#123;" out))
               (#\} (write-string "#125;" out))
               (#\| (write-string "#124;" out))
               (otherwise (write-char char out))))))

(defun format-char-class (spec)
  "Format a character class specification for display."
  (cond
    ((characterp spec) (escape-mermaid (string spec)))
    ((and (consp spec) (eq (car spec) :range))
     (format nil "~A-~A" (escape-mermaid (string (second spec)))
             (escape-mermaid (string (third spec)))))
    ((eq spec :digit-class) "\\d")
    ((eq spec :non-digit-class) "\\D")
    ((eq spec :word-char-class) "\\w")
    ((eq spec :non-word-char-class) "\\W")
    ((eq spec :whitespace-char-class) "\\s")
    ((eq spec :non-whitespace-char-class) "\\S")
    (t (escape-mermaid (princ-to-string spec)))))

(defun tree-to-mermaid (tree)
  "Convert a CL-PPCRE parse tree to Mermaid flowchart format."
  (reset-counter)
  (with-output-to-string (s)
    (format s "flowchart LR~%")
    (let ((start-id (next-node-id))
          (end-id (next-node-id)))
      (format s "    ~A((start))~%" start-id)
      (format s "    ~A((end))~%" end-id)
      (let ((last-nodes (emit-node s tree (list start-id))))
        (dolist (node last-nodes)
          (format s "    ~A --> ~A~%" node end-id))))))

(defun emit-node (stream tree prev-nodes)
  "Emit Mermaid nodes for TREE, connecting from PREV-NODES.
   Returns list of nodes that need to connect to the next element."
  (cond
    ;; Simple string literal
    ((stringp tree)
     (let ((id (next-node-id)))
       (format stream "    ~A[\"~A\"]~%" id (escape-mermaid tree))
       (dolist (prev prev-nodes)
         (format stream "    ~A --> ~A~%" prev id))
       (list id)))

    ;; Single character
    ((characterp tree)
     (let ((id (next-node-id)))
       (format stream "    ~A[\"~A\"]~%" id (escape-mermaid (string tree)))
       (dolist (prev prev-nodes)
         (format stream "    ~A --> ~A~%" prev id))
       (list id)))

    ;; Compound expressions
    ((consp tree)
     (case (car tree)
       ;; Sequence of elements
       (:sequence
        (let ((current-nodes prev-nodes))
          (dolist (elem (cdr tree))
            (setf current-nodes (emit-node stream elem current-nodes)))
          current-nodes))

       ;; Alternation (choice)
       (:alternation
        (let ((branch-id (next-node-id))
              (merge-id (next-node-id))
              (all-ends nil))
          (format stream "    ~A{OR}~%" branch-id)
          (dolist (prev prev-nodes)
            (format stream "    ~A --> ~A~%" prev branch-id))
          (dolist (alt (cdr tree))
            (let ((ends (emit-node stream alt (list branch-id))))
              (setf all-ends (append ends all-ends))))
          ;; All branches merge
          (format stream "    ~A(( ))~%" merge-id)
          (dolist (end all-ends)
            (format stream "    ~A --> ~A~%" end merge-id))
          (list merge-id)))

       ;; Groups (capturing and non-capturing)
       ((:register :group)
        (let* ((group-type (if (eq (car tree) :register) "group" "(?:)"))
               (start-id (next-node-id))
               (end-id (next-node-id)))
          (format stream "    ~A([\"~A start\"])~%" start-id group-type)
          (dolist (prev prev-nodes)
            (format stream "    ~A --> ~A~%" prev start-id))
          (let ((inner-ends (emit-node stream (second tree) (list start-id))))
            (format stream "    ~A([\"~A end\"])~%" end-id group-type)
            (dolist (end inner-ends)
              (format stream "    ~A --> ~A~%" end end-id))
            (list end-id))))

       ;; Quantifiers
       ((:greedy-repetition :non-greedy-repetition)
        (let* ((min (second tree))
               (max (third tree))
               (inner (fourth tree))
               (greedy (eq (car tree) :greedy-repetition))
               (quant (cond
                        ((and (= min 0) (null max)) (if greedy "*" "*?"))
                        ((and (= min 1) (null max)) (if greedy "+" "+?"))
                        ((and (= min 0) (eql max 1)) (if greedy "?" "??"))
                        ((eql min max) (format nil "{~D}" min))
                        ((null max) (format nil "{~D,}" min))
                        (t (format nil "{~D,~D}" min max))))
               (quant-id (next-node-id))
               (loop-id (next-node-id)))
          ;; Emit the inner pattern
          (let ((inner-ends (emit-node stream inner prev-nodes)))
            ;; Add quantifier annotation
            (format stream "    ~A[/\"~A\"/]~%" quant-id quant)
            (dolist (end inner-ends)
              (format stream "    ~A --> ~A~%" end quant-id))
            ;; Loop back for repetition (if unbounded or max > 1)
            (when (or (null max) (> max 1))
              (format stream "    ~A -.-> ~A~%" quant-id (car prev-nodes)))
            (list quant-id))))

       ;; Character class
       (:char-class
        (let ((id (next-node-id))
              (chars (mapcar #'format-char-class (cdr tree))))
          (format stream "    ~A[\"#91;~{~A~}#93;\"]~%" id chars)
          (dolist (prev prev-nodes)
            (format stream "    ~A --> ~A~%" prev id))
          (list id)))

       ;; Inverted character class
       (:inverted-char-class
        (let ((id (next-node-id))
              (chars (mapcar #'format-char-class (cdr tree))))
          (format stream "    ~A[\"#91;^~{~A~}#93;\"]~%" id chars)
          (dolist (prev prev-nodes)
            (format stream "    ~A --> ~A~%" prev id))
          (list id)))

       ;; Anchors
       (:start-anchor
        (let ((id (next-node-id)))
          (format stream "    ~A{{\"^\"}}~%" id)
          (dolist (prev prev-nodes)
            (format stream "    ~A --> ~A~%" prev id))
          (list id)))

       (:end-anchor
        (let ((id (next-node-id)))
          (format stream "    ~A{{\"$\"}}~%" id)
          (dolist (prev prev-nodes)
            (format stream "    ~A --> ~A~%" prev id))
          (list id)))

       (:word-boundary
        (let ((id (next-node-id)))
          (format stream "    ~A{{\"\\b\"}}~%" id)
          (dolist (prev prev-nodes)
            (format stream "    ~A --> ~A~%" prev id))
          (list id)))

       (:non-word-boundary
        (let ((id (next-node-id)))
          (format stream "    ~A{{\"\\B\"}}~%" id)
          (dolist (prev prev-nodes)
            (format stream "    ~A --> ~A~%" prev id))
          (list id)))

       ;; Everything-matcher (dot)
       (:everything
        (let ((id (next-node-id)))
          (format stream "    ~A[\".#40;any#41;\"]~%" id)
          (dolist (prev prev-nodes)
            (format stream "    ~A --> ~A~%" prev id))
          (list id)))

       ;; Lookahead/lookbehind
       ((:positive-lookahead :negative-lookahead
         :positive-lookbehind :negative-lookbehind)
        (let* ((type-name (case (car tree)
                           (:positive-lookahead "(?=)")
                           (:negative-lookahead "(?!)")
                           (:positive-lookbehind "(?<=)")
                           (:negative-lookbehind "(?<!)")))
               (id (next-node-id)))
          (format stream "    ~A{{\"~A\"}}~%" id type-name)
          (dolist (prev prev-nodes)
            (format stream "    ~A --> ~A~%" prev id))
          ;; Lookarounds don't consume input, so they pass through
          (list id)))

       ;; Back-reference
       (:back-reference
        (let ((id (next-node-id))
              (num (second tree)))
          (format stream "    ~A[\"\\~D\"]~%" id num)
          (dolist (prev prev-nodes)
            (format stream "    ~A --> ~A~%" prev id))
          (list id)))

       ;; Special character classes
       ((:digit-class :non-digit-class :word-char-class
         :non-word-char-class :whitespace-char-class :non-whitespace-char-class)
        (let ((id (next-node-id))
              (label (case (car tree)
                       (:digit-class "\\d")
                       (:non-digit-class "\\D")
                       (:word-char-class "\\w")
                       (:non-word-char-class "\\W")
                       (:whitespace-char-class "\\s")
                       (:non-whitespace-char-class "\\S"))))
          (format stream "    ~A[\"~A\"]~%" id label)
          (dolist (prev prev-nodes)
            (format stream "    ~A --> ~A~%" prev id))
          (list id)))

       ;; Flags/modifiers - just pass through
       ((:flags)
        prev-nodes)

       ;; Unknown - show as-is
       (otherwise
        (let ((id (next-node-id)))
          (format stream "    ~A[\"~A\"]~%" id (escape-mermaid (princ-to-string tree)))
          (dolist (prev prev-nodes)
            (format stream "    ~A --> ~A~%" prev id))
          (list id)))))

    ;; Symbols (character class shortcuts at top level)
    ((symbolp tree)
     (let ((id (next-node-id))
           (label (case tree
                    (:digit-class "\\d")
                    (:non-digit-class "\\D")
                    (:word-char-class "\\w")
                    (:non-word-char-class "\\W")
                    (:whitespace-char-class "\\s")
                    (:non-whitespace-char-class "\\S")
                    (:everything ".")
                    (otherwise (escape-mermaid (symbol-name tree))))))
       (format stream "    ~A[\"~A\"]~%" id label)
       (dolist (prev prev-nodes)
         (format stream "    ~A --> ~A~%" prev id))
       (list id)))

    ;; Fallback
    (t
     (let ((id (next-node-id)))
       (format stream "    ~A[\"~A\"]~%" id (escape-mermaid (princ-to-string tree)))
       (dolist (prev prev-nodes)
         (format stream "    ~A --> ~A~%" prev id))
       (list id)))))

;;; ---------------------------------------------------------------------------
;;; ICL Visualization Integration
;;; ---------------------------------------------------------------------------

(defun register-icl-viz ()
  "Register regexp visualization with ICL."
  (defmethod icl-runtime:visualize ((obj regexp))
    (list :mermaid (tree-to-mermaid (regexp-tree obj)))))

;;; ---------------------------------------------------------------------------
;;; Example Patterns
;;; ---------------------------------------------------------------------------

(defvar *email-pattern* (make-regexp "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")
  "A simple email validation pattern.")

(defvar *phone-pattern* (make-regexp "\\d{3}[-.\\s]?\\d{3}[-.\\s]?\\d{4}")
  "US phone number pattern.")

(defvar *url-pattern* (make-regexp "https?://[^\\s]+")
  "Simple URL pattern.")

;;; To test, run in ICL browser mode:
;;;   ,viz *email-pattern*    ; Email regex structure
;;;   ,viz *phone-pattern*    ; Phone number regex
;;;   ,viz *url-pattern*      ; URL regex
;;;   ,viz (make-regexp "(foo|bar)+")  ; Custom pattern
