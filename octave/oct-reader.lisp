(ql:quickload 'cl-ppcre)

(defpackage :oct-reader
  (:use :cl :cl-ppcre))

(in-package :oct-reader)

(set-dispatch-macro-character 
 #\# #\m
 (lambda (stream char1 char2)
   (let* ((mat-string (string-from-stream stream #\space))
	  (nums (mapcar #'read-from-string (ppcre::split ":" mat-string))))
     (let ((l (length nums)))
       (cond ((= l 0) `#())
	     ((= l 1) `#(,(first nums)))
	     ((= l 2) `(range ,(first nums) ,(second nums)))
	     ((= l 3) `(range ,(first nums) ,(third nums) ,(second nums))))))))

(defun range (start stop &optional (inc 1))
  (and (or (and (< start stop) (< inc 0))
	   (and (> start stop) (> inc 0)))
       (break (format nil "wrong range supplied: start= ~d, stop= ~d, inc= ~d" start stop inc)))
  (coerce (loop for i from start to stop by inc
	       collect i) 'vector))

(defvar *|;-reader|* (get-macro-character #\;))

(set-macro-character
 #\[
 (lambda (stream char1)
   (let* ((semi-pos)
	  (splits (string-from-stream stream #\]))
	  (splits (mapcan (lambda (s) (if (not (string= s ""))
					  (if (not (string= s ";"))
					      (list (concatenate 'string "#m" s))
					      (list ";"))))
			  (cl-ppcre::split "[ ]" splits)))
	  (splits (loop for s in splits 
		     with p = 0
		     when (if (string= s ";") 
			      (progn (push p semi-pos) nil)
			      (progn (incf p) t))
		     collect (read-from-string s t nil))))
     (setf semi-pos (reverse semi-pos))
     (read-char stream t nil t)
     `(vertcat ,@(loop for i from 0 to (length semi-pos)
		    with init-pos = 0
		    collect (prog1 
				`(horzcat ,@(subseq splits init-pos (nth i semi-pos)))
			      (setf init-pos (nth i semi-pos))))))))

;(set-macro-character #\] (get-macro-character #\) ))

(defun string-from-stream (stream delimit-char)
  (coerce
   (loop named reader 
      until (let ((char (peek-char nil stream nil nil t)))
	      (or (null char) (eq char delimit-char)))
      collect (read-char stream nil nil t))
   'string))

(defun horzcat (&rest vectors)
  (if (not (loop for v in vectors always (vectorp v))) (break "not a vector passed to horzcat"))
  (reduce (lambda (a b)
	    (concatenate 'vector a b)) vectors))

(defun vertcat (&rest arrays)
;  (format t "vertcat called with : ~D" arrays)
  (let ((list (reduce 'nconc (mapcar #'list (mapcar #'array-to-list arrays)))))
    (make-array (get-dims-from-list list) :initial-contents list)))

(defun get-dims-from-list (list &optional (dims nil))
  (cond ((not (atom list))
	 (setq dims (append dims `(,(length list))))
	 (funcall #'get-dims-from-list (first list) dims))
	(t dims)))

(defun setcons (lst val)
  (setq lst (cons val lst)) val)

(defun array-to-list (arr)
  (let ((genlist)(i 0))
    (labels ((snippet ()
	       (cond ((< i (length (array-dimensions arr)))
		      (incf i)
		      (setq genlist (cons (gensym) genlist))
		      `(let ((lst))
			 (do (( ,(car genlist) 
				 (- (array-dimension ,arr (- ,i 1)) 1) 
				 (- ,(car genlist) 1))) ((< ,(car genlist) 0) lst)
			   (setf lst (cons ,(snippet) lst)))))
		     (t `(aref ,arr ,@(reverse genlist))))))
      (eval (snippet)))))

(defun list= (&rest lists)
  (if (apply #'= (mapcar #'length lists))
      (reduce (lambda (l1 l2)
		(mapcan (lambda (a b)
			  (cond ((not (= a b)) (break (format nil "lists ~D and ~D are not equal." l1 l2))(return-from list= nil))))
			l1 l2)
		l2)
	      lists)
      nil))
				
(defun oct== (n1 n2)
  (if (= n1 n2) 1 0))
			
(defun oct-mat-op (operation &rest arrays)
  (if (apply #'list= (mapcar #'array-dimensions arrays))
      (let* ((dims (array-dimensions (first arrays)))
	     (aout (make-array dims :initial-element 0))
	     (indices (make-list (length dims) :initial-element 0)))
	(labels ((inc-index ()
		   (loop for i from (1- (length dims)) downto 0
		      do (cond ((< (nth i indices) (1- (nth i dims)))
				(incf (nth i indices))
				(return-from inc-index t))
			       (t (setf (nth i indices) 0))))
		   nil))
	  (loop
	       do (setf (apply #'aref aout indices)
			(apply operation (mapcar (lambda (arr) (apply #'aref arr indices)) arrays)))
	       until (not (inc-index))))
	aout)))

(defun oct-max (list)
  (apply #'mapcar #'max list))

(defun oct-min (list)
  (apply #'mapcar #'min list))

(defun oct-transpose (list)
  (apply #'mapcar #'list list))

(defun oct-sumcolumns (list)
  (apply #'mapcar #'+ list))

(defun oct-sumrows (list)
  (apply #'+ list))
