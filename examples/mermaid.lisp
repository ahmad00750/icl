;;; mermaid.lisp --- Mermaid diagram examples for ICL
;;;
;;; Usage: In ICL browser mode, load this file and visualize:
;;;   (load "examples/mermaid.lisp")
;;;   ,viz *flowchart*
;;;   ,viz *sequence-diagram*
;;;   ,viz *my-state-machine*
;;;
;;; ---------------------------------------------------------------------------
;;; Simple Mermaid Diagram Examples
;;; ---------------------------------------------------------------------------

;; A simple flowchart
(defvar *flowchart*
  "graph TD
    A[Start] --> B{Is it working?}
    B -->|Yes| C[Great!]
    B -->|No| D[Debug]
    D --> B
    C --> E[End]"
  "A simple flowchart diagram.")

;; A sequence diagram
(defvar *sequence-diagram*
  "sequenceDiagram
    participant User
    participant ICL
    participant Lisp
    User->>ICL: Enter expression
    ICL->>Lisp: Evaluate
    Lisp-->>ICL: Return value
    ICL-->>User: Display result"
  "A sequence diagram showing ICL evaluation flow.")

;; A class diagram
(defvar *class-diagram*
  "classDiagram
    class CLOS-Object {
      +slots
      +methods()
    }
    class Standard-Object {
      +class-of()
      +slot-value()
    }
    class Your-Class {
      +custom-slots
      +your-methods()
    }
    CLOS-Object <|-- Standard-Object
    Standard-Object <|-- Your-Class"
  "A class diagram showing CLOS inheritance.")

;;; ---------------------------------------------------------------------------
;;; Custom Visualization Example
;;; ---------------------------------------------------------------------------

(defclass state-machine ()
  ((name :initarg :name :accessor sm-name)
   (states :initarg :states :accessor sm-states)
   (transitions :initarg :transitions :accessor sm-transitions)
   (initial :initarg :initial :accessor sm-initial))
  (:documentation "A simple state machine definition."))

(defun make-state-machine (name states transitions initial)
  "Create a state machine.
   STATES is a list of state names.
   TRANSITIONS is a list of (from to label) tuples.
   INITIAL is the initial state name."
  (make-instance 'state-machine
                 :name name
                 :states states
                 :transitions transitions
                 :initial initial))

(defun state-machine-to-mermaid (sm)
  "Convert state-machine to a Mermaid state diagram string."
  (with-output-to-string (s)
    (format s "stateDiagram-v2~%")
    ;; Mark initial state
    (format s "    [*] --> ~A~%" (sm-initial sm))
    ;; Add transitions
    (loop for (from to label) in (sm-transitions sm)
          do (format s "    ~A --> ~A: ~A~%" from to label))))

;; Register custom visualization with ICL
;; This function is called automatically by ICL when ,viz is invoked
(defun register-icl-viz ()
  "Register state-machine visualization with ICL."
  (eval `(defmethod ,(intern "VISUALIZE" :icl-runtime) ((obj state-machine))
           (list :mermaid (state-machine-to-mermaid obj)))))

;; Example state machine instance
(defvar *my-state-machine*
  (make-state-machine
   "HTTP Request"
   '(idle connecting connected error closed)
   '((idle connecting "connect()")
     (connecting connected "success")
     (connecting error "timeout")
     (connected idle "disconnect()")
     (connected error "network failure")
     (error closed "cleanup")
     (closed idle "reset"))
   'idle)
  "Sample state machine for HTTP connection lifecycle.")

;;; To test, run in ICL browser mode:
;;;   ,viz *flowchart*           ; Simple flowchart
;;;   ,viz *sequence-diagram*    ; Sequence diagram
;;;   ,viz *class-diagram*       ; Class diagram
;;;   ,viz *my-state-machine*    ; Custom object visualization
