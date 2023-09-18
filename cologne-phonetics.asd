
(asdf:defsystem #:cologne-phonetics
  :description "cm with all additions loaded"
  :author "Ruben Philipp <ruben.philipp@folkwang-uni.de>"
  :license  ""
  :version "1.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:split-sequence)
  :components ((:file "package")
               (:file "cologne-phonetics")))
