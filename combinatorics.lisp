(defpackage combinatorics
  (:use common-lisp)
  (:export choose choose-combination choose-exhaustive combinations permutations))

(in-package :combinatorics)

(defun matchcont (A B)
  (dolist (itr A t) (cond ((null (find itr B)) (return NIL)))))
    
(defun findcar (ITM LST)
  (dolist (itr LST NIL) (cond ((matchcont itr ITM) (return t)))))

(defun permute (n LST)
  "A faster version available later (exhaustive-choose)"
  (let ((NETLIST))
    (labels ((permutations (n LST &optional (ACC))
	       (cond ((zerop n) (setq NETLIST (cons ACC NETLIST)))
		     ('T (mapcar #'(lambda (E) (permutations (- n 1) (remove E LST) (cons E ACC)))
				 LST)))))
      (permutations n LST))
    NETLIST))

(defun combination (n LST)
  (let ((NETLIST))
    (labels ((combinations (n LST &optional (ACC))
	       (cond ((zerop n)
		      (cond ((not (findcar ACC NETLIST)) (setq NETLIST (cons (reverse ACC) NETLIST)))))		      
		     ('T (mapcar #'(lambda (E) (combinations (- n 1) (remove E LST) (cons E ACC)))
				 LST)))))
      (combinations n LST))
    (reverse NETLIST)))

(defun roll-dice (n)
  (let ((NETLIST) (LST '(1 2 3 4 5 6)))
    (labels ((permutations (n LST &optional (ACC))
	       (cond ((zerop n) (setq NETLIST (cons ACC NETLIST)))
		     ('T (mapcar #'(lambda (E) (permutations (- n 1) LST (cons E ACC)))
				 LST)))))
      (permutations n LST))
    NETLIST))


(defun combination2 (n LST)
  (let ((NETLIST))
    (labels ((permutations (n LST &optional (ACC))
	       (cond ((zerop n) (setq NETLIST (cons ACC NETLIST)))
		     ('T  (mapcar #'(lambda (E) (progn (permutations (- n 1) (remove E LST) (cons E ACC)) (setq LST (remove E LST))))
				  LST)))))
      (progn (permutations n LST) NETLIST))))

(defun aplist (&key ((:startat start) 0) ((:stopat stop) 0) ((:step step) 1))
  (cond ((not (> (* step start) (* step stop)))
	 (append `(,start) (aplist :startat (+ start step) :stopat stop :step step)))))

;; only if we could use this kind of code structure, life would 
;; be so much easier for combinatorics

'(block choice-block
  (let ((arg '(0 1 2 3 4 5)))
    (loop 
       (block exec-block
	 (let ((X (first arg)))
	   (cond ((null X) (return-from choice-block)))
	   (setq arg (rest arg))
	   (cond ((oddp X) (return-from exec-block))
		 ('T (print X))))))))

(defmacro choose-combination (val-list var-list &body code)
  (labels ((create-choose-list ()
	     (mapcar #'(lambda (var) `(,var ,val-list)) var-list)))
    (let ((sel-group (gensym "selected-groups")))
      `(let ((,sel-group))
	 (choose-exhaustive ,(create-choose-list)
	   (if (find (sort (list ,@var-list) #'<) ,sel-group :test #'equal)
	       (fail))
	   (setq ,sel-group (append ,sel-group (list (sort (list ,@var-list) #'<))))
	   (format nil "~& ~D" ,sel-group)
	   ,@code)))))
		    
(defmacro choose-let (var list &body code)
  "Execute code after binding var to any one item of list"
  (let ((argvar (gensym)))
  `(block choice-block
     (let ((,argvar ,list))
       (loop
	  (cond ((null ,argvar) (return-from choice-block)))
	  (block exec-block
	    (let ((,var (first ,argvar)))
	      (setf ,argvar (rest ,argvar))
	      ,@code)))))))

(defmacro fail ()  
  "Choose next combination"
  `(return-from exec-block))

(defmacro choose (var-list-pairs &body code)
  "Choose multiple vars from paired lists and execute code"
  (labels ((each-var-list (var-list-pairs)
	     (cond ((not (null var-list-pairs))
		    `(choose-let ,(caar var-list-pairs) ,(cadar var-list-pairs)
		       ,(each-var-list (rest var-list-pairs))))
		   ('T `(progn ,@code)))))
    (cond ((symbolp (car var-list-pairs))
	   `(choose (macroexpand-1 ',var-list-pairs) ,@code))
	  ('T (each-var-list var-list-pairs)))))

(defmacro choose-exhaustive (var-list-pairs &body code)
  "Choose multiple vars from paired lists and execute code"
  (labels ((each-var-list (var-lists)
	     (cond ((not (null var-lists))
		    `(choose-let ,(caar var-lists) ,(cadar var-lists)
		       ,(each-var-list (rest var-lists))))
		   ('T `(progn 
			  (let ((last))
			    (dolist (i (sort (list ,@(mapcar #'car var-list-pairs)) #'<))
			      (if (and last (= last i)) 
				  (fail)
				  (setq last i))))
			  ,@code)))))
    (cond ((symbolp (car var-list-pairs))
	   `(choose (macroexpand-1 ',var-list-pairs) ,@code))
	  ('T (each-var-list var-list-pairs)))))

(defmacro choose-logic (var-list &body code)
  "Choose vars from either 0 or 1 and execute code"
  (labels ((each-var-list (var-list-pairs)
	     (cond ((not (null var-list-pairs))
		    `(choose-let ,(car var-list-pairs) '(nil t)
		       ,(each-var-list (rest var-list-pairs))))
		   ('T `(progn ,@code)))))
    (each-var-list var-list)))

(defun ap-list (&key ((:startat start) 0) ((:stopat stop) 0) ((:step step) 1))
  (cond ((not (> (* start step) (* stop step)))
	 (append `(,start) (ap-list :startat (+ start step) :stopat stop :step step)))))

(defmacro nzero (arg) `(not (zerop ,arg)))
(defmacro nnull (arg) `(not (null ,arg)))

(defun symbol-val-list (list)
  (cond ((not (null list)) (append (list (symbol-value (car list))) (symbol-val-list (rest list))))))

(defmacro as-many-choose (list n)
  "Choose n items each time and collect without exhausting during choosing"
  (let ((capturesyms)(collectvar (gensym)))
    (labels ((choose-blocks (n)
	       (let ((capsym (gensym)))
		 (cond ((nzero n)
			(setq capturesyms (append `(,capsym) capturesyms))
			`(choose-let ,capsym ,list ,(choose-blocks (- n 1))))
		       ('T 
			`(setq ,collectvar (append (list (list ,@capturesyms)) ,collectvar) ))))))
      `(let ((,collectvar))
	 ,(choose-blocks n)
	 ,collectvar))))

(defmacro exhausting-choose (list n)
  "Choose n items each time and collect while exhausting during choosing"
  (let ((capturesyms)(collectvar (gensym)))
    (labels ((choose-blocks (n)
	       (let ((capsym (gensym)))
		 (cond ((nzero n)
			(setq capturesyms (append `(,capsym) capturesyms))
			`(choose-let ,capsym (remove-mult ,list ,@(rest capturesyms)) ,(choose-blocks (- n 1))))
		       ('T 
			`(setq ,collectvar (append (list (list ,@capturesyms)) ,collectvar)))))))
      `(let ((,collectvar))
	 ,(choose-blocks n)
	 ,collectvar))))

(defmacro remove-mult (list &body items)
  "Remove multiple items from a list"
  (cond ((not (null items))
	 `(remove ,(car items) (remove-mult ,list ,@(rest items))))
	('T list)))

;; this code for permuting over the length of the list (from net)

(defun p (l)
  (if (null l) '(())
  (mapcan #'(lambda (x)
    (mapcar #'(lambda (y) (cons x y))
      (p (remove x l :count 1)))) l)))

(defmacro xor (&body args)
  (labels ((eachargsnot (args)
	     (if (not (null args))
		 (append `((not ,(car args))) (eachargsnot (rest args)))))
	   (eachargs (args)
	     (if (not (null args))
		 (append `(,(car args)) (eachargs (rest args))))))
    `(and (or ,@(eachargs args))(or ,@(eachargsnot args)))))

(defun fxor2 (&rest args)
  (cond ((= (length args) 1) args)
	((= (length args) 2) (cond ((= (first args) (second args)) nil)
				   ('T t)))))

'(defmacro xor (&body args)
  (cond ((> (length args) 2) 
	 `(xor ,@(car args) (xor ,@(rest args)) ))
	('T
	 `(fxor2 ,@args))))
	 
(defun factorial (N) 
  (if (= N 1) 1
      (* N (factorial (1- N)))))

(defun combinations (n r)
  (/ (factorial n) (* (factorial r) (factorial (- n r)))))

(defun permuations (n r)
  (/ (factorial n) (factorial (- n r))))