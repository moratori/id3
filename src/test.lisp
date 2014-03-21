
(ql:quickload :id3)


(import '(id3.core:loadfile
		  id3.core:make-tree))

(multiple-value-bind (property-list ents) (loadfile "data/test2.csv")
  (let ((tree (make-tree property-list ents)))
	(print tree)
	(print 
	  (id3.out::write-tree "data/test2.dot" tree)
	  
	  )
	)
  
  )


