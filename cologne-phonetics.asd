
(asdf:defsystem #:cologne-phonetics
  :description "An implementation of the Cologne Phonetics algorithm"
  :author "Ruben Philipp <ruben.philipp@folkwang-uni.de>"
  :license  ""
  :version "1.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:split-sequence)
  :components ((:file "package")
               (:file "cologne-phonetics")))
