;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename: example.lisp
;;;
;;; Purpose: examples usage
;;;
;;; Author: Ruben Philipp
;;; Created: 2023-02-01
;;; $$ Last modified:  15:10:32 Thu Feb  2 2023 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(load "package.lisp")
(load "cologne-phonetics.lisp")


(cologne-phonetics::encode "Philipp Philip Filip Filipp Fillipp")
