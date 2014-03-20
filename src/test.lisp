
(ql:quickload :id3)


(import '(id3.core:loadfile
		  id3.core:make-tree))

(multiple-value-bind (property-list ents) (loadfile "test.csv")
  (print 
	(make-tree property-list ents))
  )
