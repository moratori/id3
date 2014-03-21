
(ns:defns id3.struct
	(:use :cl)
	(:export
	  :node-p
	  :node-id
	  :node-edge-name
	  :node-label
	  :node-type
	  :node-child-list))

@export
(defvar +LEAF+ 'leaf)
@export
(defvar +BRANCH+ 'branch)

@export
(defstruct (node (:constructor node (edge-name label type child-list)))
  (id (subseq (symbol-name (gensym "ID")) 2) :type string)
  (edge-name "" :type t)
  (label "" :type t)
  (type 'branch :type symbol)
  (child-list nil :type list))



