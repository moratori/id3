

(defpackage id3-asd
	(:use :cl :asdf))
(in-package id3-asd)


(defsystem id3
  :depends-on (:namespace :cl-ppcre :iterate)
  :serial t
  :components 
    ((:module "src"
      :serial t
      :components 
        ((:file "struct")
		 (:file "main")
         (:file "out")))))
