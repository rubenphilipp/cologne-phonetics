;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; cologne-phonetics.lisp
;;;
;;; PURPOSE
;;; Common Lisp implementation of the Cologne Phonetics algorithm
;;;
;;; Credits: This package is based on the Python implementation
;;;          by Janek Nouvertné
;;;          (https://github.com/provinzkraut/cologne_phonetics)
;;;
;;; AUTHOR
;;; Ruben Philipp <ruben.philipp@folkwang-uni.de>
;;;
;;; CREATED
;;; 2023-02-01
;;;
;;; VERSION
;;; 1.0.1
;;;
;;; $$ Last modified:  11:02:00 Tue Mar 28 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cologne-phonetics)

(defparameter *col-ph-replacements*
  '(("ä" "ae")
    ("ö" "oe")
    ("ü" "ue")
    ("ß" "s")
    ("é" "e")
    ("è" "e")
    ("á" "a")
    ("à" "a")
    ("ç" "c")))

(defparameter *char-replacements*
  '(;; replace umlauts with a vowel
    ;; NB: As all vowels are later encoded with a 0,
    ;; this turns every umlaut into an "a"
    ;; RP  Mon Mar 27 22:12:30 2023
    ("ß" "s")
    ("äöü" "a")
    ;; ignore additional special characters
    ;; RP  Mon Mar 27 22:13:20 2023
    ("[^a-z ]" "")
    ;; [d,t replacements]
    ;; not before c,s,z
    ("[dt](?![csz])" "2")
    ;; before c,s,z
    ("[dt](?=[csz])" "8")
    ;; [x replacements]
    ;; not after c,k,q
    ("(?<![ckq])x" "48")
    ;; after c,k,q. insert new x for later comparison. will be removed later
    ;; at the start before a,h,k,l,o,q,r,u,x
    ;; | not after s,z before a,h,k,o,q,u,x
    ("(?<=[ckq])x" "x8")
    ;; [c replacements]
    ;; not before a,h,k,o,q,u,x
    ;; | not before s,z
    ;; | at the start, not before a,h,k,l,o,q,r,u,x
    ("^c(?=[ahkloqrux])|(?<![sz])c(?=[ahkoqux])" "4")
    ("c(?![ahkoqux])|(?<=[sz])c|^c(?![ahkloqrux])" "8")
    ("p(?!h)|b" "1") ;;; p not before h
    ("p(?=h)|[fvw]" "3") ;;; p before h and f,v,w
    ("[hx]" "")
    ("[aeijouy]" "0")
    ("[gkq]" "4")
    ("l" "5")
    ("[mn]" "6")
    ("r" "7")
    ("[sz]" "8")
    ("(\\d)(?=\\1)" "")    ;; repeating digits
    ("\\B0" "")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; apply replacements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apply-replacements (data &optional (replacements *col-ph-replacements*))
  (reduce (lambda (acc x) (cl-ppcre:regex-replace-all (first x) acc (second x)))
          replacements
          :initial-value data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; encode
;;;
;;; encode a string to Cologne Phonetics Code
;;; A whitespace is interpreted as a word break
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun encode (data &optional (replacements *char-replacements*))
  (mapcar (lambda (str) (apply-replacements str replacements))
   (split-sequence #\space (apply-replacements (string-downcase data)))))

#|
(cologne-phonetics:encode "Philipp Philip Phillip Filip Filipp")
;; => ("351" "351" "351" "351" "351")
|#
