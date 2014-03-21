
(ns:defns id3.out
	(:use :cl
		  :id3.struct))


(defun node-definition (tree)
  (assert (node-p tree))
  (labels 
	((main (tree def)
		(let ((id (node-id tree))
			  (label (node-label tree))
			  (type (node-type tree))
			  (children (node-child-list tree)))
		  (cond 
			((eq type +LEAF+)
			 (cons (list id label "circle")  def))
			((eq type +BRANCH+)
			 (cons (list id label "diamond")
			 (loop for child in children 
				   append (main child nil))))))))
	(main tree nil)))


(defun init-file (stream graph-name def)
  (format stream "digraph ~A {~%~{~2tN~A~%~}~%~%" 
		  graph-name
		  (mapcar 
			(lambda (each)
			  (destructuring-bind (id label shape) each
				(concatenate 
				  'string
				  id 
				  "[label = \"" label "\" , shape = " shape "];"
				  ))) def)))


(defun %write-tree (stream tree)
  (let ((id (node-id tree))
		(edge-name (node-edge-name tree))
		(label (node-label tree))
		(type (node-type tree))
		(child-list (node-child-list tree)))
	(dolist (child child-list)
	  (format stream "~2tN~A -> N~A[label = \"~A\"];~%"
			  id (node-id child) (node-edge-name child)))
	(dolist (branch (remove-if-not 
					  (lambda (child) (eq +BRANCH+ (node-type child))) child-list))
	  (%write-tree stream branch))))



(defun write-tree (path tree)
  (with-open-file (out path :direction :output :if-exists :supersede)
	(let ((def (node-definition tree)))
	  (init-file out (pathname-name path) def)
	  (%write-tree out tree)
	  (format out "}"))))
