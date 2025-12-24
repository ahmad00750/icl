;;; vega.lisp --- Vega-Lite visualization examples for ICL
;;;
;;; Usage: In ICL browser mode, load this file and visualize:
;;;   (load "examples/vega.lisp")
;;;   ,viz *my-sales*
;;;
;;; ─────────────────────────────────────────────────────────────────────────────
;;; Custom Visualization Example
;;; ─────────────────────────────────────────────────────────────────────────────

(defclass sales-data ()
  ((quarters :initarg :quarters :accessor sales-quarters)
   (revenue :initarg :revenue :accessor sales-revenue)
   (expenses :initarg :expenses :accessor sales-expenses))
  (:documentation "Quarterly sales data with revenue and expenses."))

(defun make-sales-data (quarters revenue expenses)
  "Create a sales-data object."
  (make-instance 'sales-data
                 :quarters quarters
                 :revenue revenue
                 :expenses expenses))

(defun sales-data-to-vega-lite (obj)
  "Convert sales-data to a Vega-Lite spec string."
  (let* ((quarters (sales-quarters obj))
         (revenue (sales-revenue obj))
         (expenses (sales-expenses obj))
         (values (loop for q in quarters
                       for r in revenue
                       for e in expenses
                       collect (format nil "{\"quarter\":\"~A\",\"type\":\"Revenue\",\"amount\":~A}" q r)
                       collect (format nil "{\"quarter\":\"~A\",\"type\":\"Expenses\",\"amount\":~A}" q e))))
    (format nil "{\"$schema\":\"https://vega.github.io/schema/vega-lite/v5.json\",
      \"description\":\"Quarterly Sales Data\",
      \"data\":{\"values\":[~{~A~^,~}]},
      \"mark\":\"bar\",
      \"encoding\":{
        \"x\":{\"field\":\"quarter\",\"type\":\"nominal\",\"title\":\"Quarter\"},
        \"y\":{\"field\":\"amount\",\"type\":\"quantitative\",\"title\":\"Amount ($)\"},
        \"color\":{\"field\":\"type\",\"type\":\"nominal\",\"title\":\"Type\"},
        \"xOffset\":{\"field\":\"type\"}
      }}" values)))

;; Register custom visualization when running in ICL
(when (find-package :icl-runtime)
  (eval `(defmethod ,(intern "VISUALIZE" :icl-runtime) ((obj sales-data))
           (list :vega-lite (sales-data-to-vega-lite obj)))))

;; Example sales data instance
(defvar *my-sales*
  (make-sales-data '("Q1" "Q2" "Q3" "Q4")
                   '(120000 145000 160000 180000)
                   '(80000 95000 90000 110000))
  "Sample sales data for visualization.")

;;; To test, run in ICL browser mode:
;;;   ,viz *my-sales*       ; Custom object visualization
