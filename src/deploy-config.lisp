;;; deploy-config.lisp --- Deploy library configuration
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;; Tell deploy to skip compression library (built into SBCL)
(deploy:define-library deploy::compression-lib :dont-deploy t)
