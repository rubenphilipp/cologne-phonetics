;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename: example.lisp
;;;
;;; Purpose: examples usage
;;;
;;; Author: Ruben Philipp
;;; Created: 2023-02-01
;;; $$ Last modified:  21:56:45 Wed Feb  1 2023 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(load "package.lisp")
(load "cologne-phonetics.lisp")


(cologne-phonetics::encode "Philipp Philip Filip Filipp Fillipp")
