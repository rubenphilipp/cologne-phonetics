;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cologne-phonetics
;;;
;;; Purpose: Common Lisp implementation of the Cologne Phonetics algorithm
;;;
;;; Credits: This package is based on the Python implementation
;;;          by Janek Nouvertné
;;;          (https://github.com/provinzkraut/cologne_phonetics)
;;;
;;; Author: Ruben Philipp <ruben.philipp@folkwang-uni.de>
;;;
;;; Creation date: 2023-02-01
;;;
;;; $$ Last modified:  22:57:30 Thu Feb  2 2023 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cologne-phonetics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; define replacement patterns for special characters (e.g. umlauts)
;;; type: association list
#|
(let* ((list1 (list (cons #\ä "ae")
                    (cons #\ö "oe"))))
(cdr (assoc #\ä list1)))
|#

(defparameter +col-ph-replacements+
  (list (cons "ä" "ae")
        (cons "ö" "oe")
        (cons "ü" "ue")
        (cons "ß" "s")
        (cons "é" "e")
        (cons "è" "e")
        (cons "á" "a")
        (cons "à" "a")
        (cons "ç" "c")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; apply replacements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apply-replacements (data &key (replacements +col-ph-replacements+))
  (loop for replacement in replacements
        do
           (setf data (cl-ppcre:regex-replace-all (car replacement)
                                                  data
                                                  (cdr replacement)
                                                  :element-type 'character)))
  data)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; encode
;;;
;;; encode a string to Cologne Phonetics Code
;;; A whitespace is interpreted as a word break
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(cologne-phonetics::encode "Philipp Philip Phillip Filip Filipp")
;; => ("351" "351" "351" "351" "351")
|#


(defun encode (data)
  (let* ((data-downcase (string-downcase data))
         ;; apply replacements
         (replaced-data (apply-replacements data-downcase))
         (words (split-sequence:split-sequence #\space
                                               replaced-data)))
    ;; word by word
    (loop for word in words
          collect
          (let* (;; ignore additional special characters
                 (data (cl-ppcre:regex-replace-all "[^a-z ]"
                                                   word
                                                   "")))
            ;; [d,t replacements]
            ;; not before c,s,z
            (setf data
                  (cl-ppcre:regex-replace-all "[dt](?![csz])"
                                              data
                                              "2"))
            ;; before c,s,z
            (setf data
                  (cl-ppcre:regex-replace-all "[dt](?=[csz])"
                                              data
                                              "8"))

            ;; [x replacements]
            ;; not after c,k,q
            (setf data
                  (cl-ppcre:regex-replace-all "(?<![ckq])x"
                                              data
                                              "48"))
            ;; after c,k,q. insert new x for later comparison. will be removed later
            (setf data
                  (cl-ppcre:regex-replace-all "(?<=[ckq])x"
                                              data
                                              "x8"))

            ;; [c replacements]
            ;; at the start before a,h,k,l,o,q,r,u,x
            ;; | not after s,z before a,h,k,o,q,u,x
            (setf data
                  (cl-ppcre:regex-replace-all "^c(?=[ahkloqrux])|(?<![sz])c(?=[ahkoqux])"
                                              data
                                              "4"))
            ;; not before a,h,k,o,q,u,x
            ;; | not before s,z
            ;; | at the start, not before a,h,k,l,o,q,r,u,x
            (setf data
                  (cl-ppcre:regex-replace-all "c(?![ahkoqux])|(?<=[sz])c|^c(?![ahkloqrux])"
                                              data
                                              "8"))

            ;; p not before h
            (setf data
                  (cl-ppcre:regex-replace-all "p(?!h)|b"
                                              data
                                              "1"))
            ;; p before h and f,v,w
            (setf data
                  (cl-ppcre:regex-replace-all "p(?=h)|[fvw]"
                                              data
                                              "3"))

            (setf data
                  (cl-ppcre:regex-replace-all "[hx]"
                                              data
                                              ""))
            (setf data
                  (cl-ppcre:regex-replace-all "[aeijouy]"
                                              data
                                              "0"))
            (setf data
                  (cl-ppcre:regex-replace-all "[gkq]"
                                              data
                                              "4"))
            (setf data
                  (cl-ppcre:regex-replace-all "l"
                                              data
                                              "5"))
            (setf data
                  (cl-ppcre:regex-replace-all "[mn]"
                                              data
                                              "6"))
            (setf data
                  (cl-ppcre:regex-replace-all "r"
                                              data
                                              "7"))
            (setf data
                  (cl-ppcre:regex-replace-all "[sz]"
                                              data
                                              "8"))
            ;; repeating digits
            (setf data
                  (cl-ppcre:regex-replace-all "(\\d)(?=\\1)"
                                              data
                                              ""))
            (setf data
                  (cl-ppcre:regex-replace-all "\\B0"
                                              data
                                              ""))))))
            
            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF cologne-phonetics.lisp
