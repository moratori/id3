


(ns:defns id3.core
  (:use :cl)
  (:import-from :cl-ppcre
				:split))


(defun loadfile (path)
  (with-open-file (in path :direction :input :if-does-not-exist :error)
	(values
	  (split #\, (read-line in nil nil))
	  (loop for line = (read-line in nil nil)
			while line
			collect (split #\, line)))))


(defun entropy (probs)
  (- (loop for each in probs sum 
		   (* each (log each 2)))))

