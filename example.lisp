;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; example.lisp
;;;
;;; PURPOSE
;;; Example for using the cologne-phonetics package.
;;;
;;; AUTHOR
;;; Ruben Philipp
;;;
;;; CREATED
;;; 2023-02-01
;;;
;;; $$ Last modified:  11:03:04 Tue Mar 28 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(load "package.lisp")
(load "cologne-phonetics.lisp")


(cologne-phonetics::encode "Philipp Philip Filip Filipp Fillipp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF example.lisp
