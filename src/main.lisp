
(ns:defns id3.core
		  (:use :cl)
		  (:import-from :cl-ppcre
						:split))

(ql:quickload :cl-ppcre)
(import '(cl-ppcre:split))


(defvar *mode* #'string=)
(defvar +DELIMITER+ #\,)


(defun class-key (property-list)
  (car (last property-list)))

;; 以下の２値を返してくれればloadfileでなくてもよい
;; その場合 *mode* を適切な等値比較述語にする必要あり
;; (property1 property2 ...)
;; (#S(HASH)1 #S(HASH)2 ...)
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



(defstruct (node (:constructor node (edge-name label type child-list)))
  (edge-name "" :type t)
  (label "" :type t)
  (type 'branch :type symbol)
  (child-list nil :type list
			 ;; every factor in child-list shuold be node or leaf
			  ))

(defun collect-ent (property val ents)
  (remove-if-not
	(lambda (ent)
	  (funcall *mode* (gethash property ent) val)
	  ) ents))

(defun every-same? (class-key ents)
  ;; ents mustn't nil
  (let ((val (gethash class-key (car ents))))
	(every 
	  (lambda (ent)
		(funcall *mode* (gethash class-key ent) val)) 
	  (cdr ents))))

;; ents が nil　だったらつまり 学習データが無いんだから...?
;; 
(defun make-tree (from class-key priority-property-list ents)
  (cond 
	((null ents)
	 (format t "ents is nil!!"))
	((null priority-property-list)
	 (format t "decide by majority!!"))
	((every-same? class-key ents)
	 (node 
	   from 
	   (gethash class-key (car ents))
	   'leaf
	   nil))
	(t 
	  (let ((tar (car priority-property-list)))
		;; tar == MANIFACTURE
		(node 
		  from
		  tar 
		  'branch
		  (mapcar 
			(lambda (x)
			 (make-tree x class-key (cdr priority-property-list)
						(collect-ent 
						  tar 
						  x
						  ents)))
			(property-domain tar ents)))))))



(multiple-value-bind (property-list ents) (loadfile "test.csv")
  (let ((plist (sort-property 
			 property-list
			 ents)))
	(print (make-tree "" (class-key property-list)
					  plist ents))))
