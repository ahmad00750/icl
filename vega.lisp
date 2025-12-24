;;; vega.lisp --- Test Vega-Lite visualization in ICL
;;;
;;; Load this file and run the examples to test Vega-Lite charts.
;;; Requires ICL browser mode (-b flag).

(defvar *bar-chart*
  "{\"$schema\":\"https://vega.github.io/schema/vega-lite/v5.json\",
    \"description\":\"A simple bar chart\",
    \"data\":{\"values\":[
      {\"category\":\"A\",\"value\":28},
      {\"category\":\"B\",\"value\":55},
      {\"category\":\"C\",\"value\":43},
      {\"category\":\"D\",\"value\":91},
      {\"category\":\"E\",\"value\":81}
    ]},
    \"mark\":\"bar\",
    \"encoding\":{
      \"x\":{\"field\":\"category\",\"type\":\"nominal\",\"title\":\"Category\"},
      \"y\":{\"field\":\"value\",\"type\":\"quantitative\",\"title\":\"Value\"}
    }}"
  "Simple bar chart spec.")

(defvar *line-chart*
  "{\"$schema\":\"https://vega.github.io/schema/vega-lite/v5.json\",
    \"description\":\"A line chart with points\",
    \"data\":{\"values\":[
      {\"x\":0,\"y\":0},
      {\"x\":1,\"y\":2},
      {\"x\":2,\"y\":1},
      {\"x\":3,\"y\":4},
      {\"x\":4,\"y\":3},
      {\"x\":5,\"y\":5}
    ]},
    \"mark\":{\"type\":\"line\",\"point\":true},
    \"encoding\":{
      \"x\":{\"field\":\"x\",\"type\":\"quantitative\"},
      \"y\":{\"field\":\"y\",\"type\":\"quantitative\"}
    }}"
  "Line chart with points.")

(defvar *pie-chart*
  "{\"$schema\":\"https://vega.github.io/schema/vega-lite/v5.json\",
    \"description\":\"A pie chart\",
    \"data\":{\"values\":[
      {\"category\":\"Lisp\",\"value\":45},
      {\"category\":\"Python\",\"value\":25},
      {\"category\":\"Rust\",\"value\":20},
      {\"category\":\"Other\",\"value\":10}
    ]},
    \"mark\":\"arc\",
    \"encoding\":{
      \"theta\":{\"field\":\"value\",\"type\":\"quantitative\"},
      \"color\":{\"field\":\"category\",\"type\":\"nominal\"}
    }}"
  "Pie chart showing language preferences.")

(defvar *scatter-plot*
  "{\"$schema\":\"https://vega.github.io/schema/vega-lite/v5.json\",
    \"description\":\"A scatter plot\",
    \"data\":{\"values\":[
      {\"x\":1,\"y\":2,\"size\":10},
      {\"x\":2,\"y\":4,\"size\":20},
      {\"x\":3,\"y\":3,\"size\":15},
      {\"x\":4,\"y\":6,\"size\":25},
      {\"x\":5,\"y\":5,\"size\":30}
    ]},
    \"mark\":\"point\",
    \"encoding\":{
      \"x\":{\"field\":\"x\",\"type\":\"quantitative\"},
      \"y\":{\"field\":\"y\",\"type\":\"quantitative\"},
      \"size\":{\"field\":\"size\",\"type\":\"quantitative\"}
    }}"
  "Scatter plot with variable point sizes.")

;;; To test, run in ICL browser mode:
;;;   ,viz *bar-chart*
;;;   ,viz *line-chart*
;;;   ,viz *pie-chart*
;;;   ,viz *scatter-plot*
