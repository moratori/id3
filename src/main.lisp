
(ns:defns id3.core
		  (:use :cl)
		  (:import-from :cl-ppcre
						:split))


(defvar *mode* #'string=)
(defvar +DELIMITER+ #\,)


(defun class-key (property-list)
  (car (last property-list)))

(defun loadfile (path)
  (with-open-file (in path :direction :input :if-does-not-exist :error)
	(let ((property-list (split +DELIMITER+ (read-line in))))
	  (values
		 property-list
		 (loop for line = (read-line in nil nil)
			   while line
			   collect 
			     (let ((table (make-hash-table :test #'equal)))
			       (loop for key in property-list
					     for val in (split +DELIMITER+ line)
					     do (setf (gethash key table) val)) table))))))

(defun entropy (probs)
  (- (loop for p in probs
		   sum (if (zerop p) 0 (* p (log p 2))))))

(defun property-domain (property ents)
  (let ((result nil))
	(loop for ent in ents
		  do (pushnew (gethash property ent) result :test *mode*))
	result))

(defun count-property (property property-val ents)
  (count-if 
	(lambda (ent) 
	  (funcall *mode* property-val (gethash property ent)))ents))

(defun initial-entropy (class-key ents)
  (let ((len (length ents))
		(domain (property-domain class-key ents)))
	(entropy 
	  (mapcar 
		(lambda (x)
		  (/ (count-property class-key x ents) len)) domain))))

(defun property-entropy (class-key property ents)
  (let  ((class-dom (property-domain class-key ents))
		 (prop-dom  (property-domain property ents)))
	(* (/ 1 (length ents))
	   (loop for prp-val in prop-dom
			 sum 
			 (let ((denom (count-property property prp-val ents)))
			   (* denom
				  (entropy	
					(loop for cls-val in class-dom
						  collect 
						  (/ (count-if 
							   (lambda (ent)
								 (and
							 	   (funcall *mode* (gethash property ent) prp-val)
							 	   (funcall *mode* (gethash class-key ent) cls-val))) ents) denom)))))))))

(defun sort-property (property-list ents)
  (let* ((key (class-key property-list))
		 (init-val (initial-entropy key ents)))
	(sort (remove key property-list :test *mode*)
		  (lambda (x y)
			(> (- init-val (property-entropy key x ents)) 
			   (- init-val (property-entropy key y ents)))))))


(multiple-value-bind (property-list ents) (loadfile "test.csv")
	(print (sort-property 
			 property-list
			 ents)))



