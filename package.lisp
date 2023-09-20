;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; package.lisp
;;;
;;; PURPOSE
;;; Definition of the cologne-phonetics package.
;;;
;;; VERSION
;;; 1.0.1
;;;
;;; AUTHOR
;;; Ruben Philipp <ruben.philipp@folkwang-uni.de>
;;;
;;; CREATED
;;; 2023-02-01, Essen
;;;
;;; $$ Last modified:  11:03:55 Tue Mar 28 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage :cologne-phonetics
  (:nicknames :col-ph)
  (:use :common-lisp :cl-ppcre :split-sequence)
  (:export #:ENCODE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF package.lisp
