;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; filename: package.lisp
;;;
;;; Purpose: definition of the cologne-phonetics package
;;;
;;; Author: Ruben Philipp <ruben.philipp@folkwang-uni.de>
;;; Created: 2023-02-01, Essen
;;; $$ Last modified:  20:08:20 Wed Feb  1 2023 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage :cologne-phonetics
  (:nicknames :col-ph)
  (:use :common-lisp))
;;  (:import-from :cl-user )

(ql:quickload "cl-ppcre")
(ql:quickload "split-sequence")

(in-package :cologne-phonetics)
