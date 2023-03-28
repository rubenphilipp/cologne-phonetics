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
          (let* (;; lowercase everything
                 ;; RP  Mon Mar 27 22:09:39 2023
                 (data (string-downcase word)))
            ;; replace ß with s (results in code 8)
            ;; RP  Mon Mar 27 22:11:14 2023
            (setf data
                  (cl-ppcre:regex-replace-all "ß"
                                              data
                                              "s"))
            ;; replace umlauts with a vowel
            ;; NB: As all vowels are later encoded with a 0,
            ;; this turns every umlaut into an "a"
            ;; RP  Mon Mar 27 22:12:30 2023
            (setf data
                  (cl-ppcre:regex-replace-all "äöü"
                                              data
                                              "a"))
            ;; ignore additional special characters
            ;; RP  Mon Mar 27 22:13:20 2023
            (setf data
                  (cl-ppcre:regex-replace-all "[^a-z ]"
                                              data
                                              ""))
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
