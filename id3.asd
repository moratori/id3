

(defpackage id3-asd
	(:use :cl :asdf))
(in-package id3-asd)


(defsystem id3
  :depends-on (:namespace :cl-ppcre)
  :serial t
  :components 
    ((:module "src"
      :serial t
      :components 
        ((:file "main")))))
