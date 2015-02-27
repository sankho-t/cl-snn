(cl:in-package :snn)

(require :cl-ppcre)

;(
;  {
;  [ n1 #n0-1..10 -> syn1 ]
;  [ syn1 -> #n-2..5 -> syn2 ]
;  @n1 #< :in-s I :w 2 #>
;  @n0-1..10 #< :in-v 2 :w 0.5 :delay #v0..10 #>
;  @n0-1..5 n-2..5 #< :w 2 #>
;  ^p n1 n0-1..10 n-2..5
;  ^s n2
;  }snn1
;)

(defvar *current-form*)
(defvar *symbols*)
(defvar *psyms*)
(defvar *ssyms*)
(defvar *components*)
(defvar *extra-run*)

(defun set-snn-define-macros ()
  (set-macro-character 
   #\{ (lambda (us uc) 
	 (declare (ignore us uc))
	 (reset-reader)))
  (set-macro-character 
   #\} (lambda (us uc) 
	 (declare (ignore uc))
	 (if (eq (read us t nil t) 'do)
	     `(create-snn-closure ,(read us t nil t))
	     (let ((name (read us t nil t)))
	       `(defmacro ,(read-from-string (concatenate 'string "with-" (symbol-name name))) (&body code)
		  `(create-snn-closure ,@code))))))
  (set-macro-character
   #\[
   (lambda (stream uchar)
     (declare (ignore uchar))
     (let* (;; capture until there is an end square brace
	    (splits (string-from-stream stream #\]))
	    ;; split them according to layer
	    (splits (cl-ppcre::split "->" splits)))
       (eval `(feedforward-snn ,@(loop for s in splits
				    collect `',(let ((l-pos 0))
						    (loop until (or (= l-pos (length s)) (empty-string-p (subseq s l-pos)))
						       collect (multiple-value-bind (readch pos)
								   (read-from-string (subseq s l-pos))
								 (incf l-pos pos) readch)))))))
     nil))
  (set-macro-character #\] (lambda (us uc) (declare (ignore us uc)) ())))

;; function to expand a dotted-list of components
(let ((read-fn (lambda (stream char1 uchar2)
		 (declare (ignore uchar2))
		 ;; the definition lies in a single word, hence seek until a space
		 (let ((str (string-from-stream stream #\Space)))
		   ;; check if it truly is a dotted word (contains a dash)
		   (if (find #\- str)
		       ;; return the expanded list version
		       (dotted-word str (string char1)))))))
  (set-dispatch-macro-character #\# #\n read-fn)
  (set-dispatch-macro-character #\# #\s read-fn))
  
(set-macro-character #\^
 (lambda (stream uchar)
   (declare (ignore uchar))
   ;; remove whitespaces from stream
   (peek-char t stream t nil t)
   (let (;; read the symbol specifying the typ eof monitor being set
	 (comm (read stream t nil t))
	 ;; and the entire line
	 (inst (read-line stream t nil t)))
     ;; check for a valid monitor symbol
     (assert (or (eq comm 'P) (eq comm 'S)) (comm) 
	     (format nil "Unknown caret argument: ~D, for ~D.~&Provide any other character." comm inst))
     (let* (;; read all the component words
	    (insts (cl-ppcre::split " " inst))
	    ;; if a component is specified as a range, then expand it
	    (insts (reduce #'append (loop for c in insts
				       if (find #\- (coerce c 'list))
				       collect (dotted-word c)
				       else
				       collect (list (read-from-string c t nil))))))       
     `(set-syms ,comm ,@insts)))))

(set-macro-character #\@ 
 (lambda (stream uchar)
   (declare (ignore uchar))
   (let* ((comp (string-from-stream stream #\# #\<))
	  (comps (cl-ppcre::split " " comp))
	  (comps (reduce #'append (loop for c in comps
				     if (find #\- (coerce c 'list))
				     collect (dotted-word c)
				     else
				     collect (list (read-from-string c t nil))))))
     (setf comps (mapcan (lambda (c) 
			   (if (not (find c *components*)) (format t "~&Component ~D not found in structure. Skipping." c)
			       (list c))) comps))
     (set-dispatch-macro-character 
      #\# #\<
      (lambda (stream uchar1 uchar2)
	(declare (ignore uchar1 uchar2))
	(eval
	 `(set-comp-props 
	   :components ',comps
	   ,@(let ((const-args)(var-args)
		   (args (merge-brackets (cl-ppcre::split "[ ]+" (string-trim " " (string-from-stream stream #\# #\> ))))))
		  (loop 
		     for param in args by #'cddr
		     for value in (rest args) by #'cddr
		     with read-val = nil
		     with read-param = nil
		     do (setf read-val (read-from-string value t nil) read-param (read-from-string param t nil))
		     if (or (vectorp read-val))                ;; can also eval the argument before type check
		     do (push `(,read-param ,(coerce read-val 'list)) var-args)
		     else do (push `(,read-param ,read-val) const-args))
		  `(:fixed-args ',const-args :list-args ',var-args))))))
     (set-dispatch-macro-character #\# #\> (lambda (ustream uch1 uch2) (declare (ignore ustream uch1 uch2)) (set-snn-define-macros)))
     (read-char stream t nil t) ;; remove the # char. from stream
     (funcall (get-dispatch-macro-character #\# #\<) stream nil nil)
     (read-char stream t nil t)
     (funcall (get-dispatch-macro-character #\# #\>) stream nil nil)
     )))

(set-dispatch-macro-character #\# #\v
 (lambda (stream uch1 uch2)
   (declare (ignore uch1 uch2))
   (let ((args (string-from-stream stream #\Space nil t)))
     (coerce (dotted-word args "" nil) 'vector))))

(defmacro setn (synapse type neuron)
  (push `(memcall (sv ,synapse) ,(if (eq type :trainer) :set-trainer-sym-ind
				     (if (eq type :output) :set-output-sym
					 (break (format nil "Unknown type: ~D" type))))
	  (memcall ,neuron :spike-sym)
	  ;; if we are setting a trainer, also supply the index
	  ,@(if (eq type :trainer) `((memcall ,neuron :syn-index))))
	*extra-run*)
  nil)

(defmacro create-snn-closure (&body code)
  (labels ((type-of-comp (c)
	     (let ((name (symbol-name c)))
	       (if (string-equal (subseq name 0 1) "n") 'neuron
		   (if (string-equal (subseq name 0 2) "sp") 'spiker
		       (if (string-equal (subseq name 0 2) "sy") 'synapse
			   (if (string-equal (subseq  name 0 3) "ter") 'terminator
			       (let ((type)) 
				 (assert t (type) 
					 (format nil "Don't know component type of ~D. 
Please specify by symbol: neuron, spiker or synapse" c))
				 (type-of-comp type))))))))
	   (remove-f (arg1 arglist)
	     (loop for a in arglist by #'cddr
		  for b in (rest arglist) by #'cddr
		  if (not (eq a arg1))
		  appending `(,a ,b) into out
		  finally (return out)))
	   (create-build-forms ()
	     (append (prog1 
			 '((ter (terminator)))
		       '(break (format nil "Components: ~D" *components*)))
		     (loop for c in *components*
			for toc = (type-of-comp c)
			for ff = (gethash c *current-form*)
			if (or (eq toc 'neuron) (eq toc 'spiker) (eq toc 'terminator))
			;; modify here if neuron accepts multiple synapses
			collect `(,c (,toc ,(first (getf ff :next)) ,@(remove-f :next ff)))
			else
			if (eq toc 'synapse)
			collect `(,c (,toc (list ,@(getf ff :next)) ,@(remove-f :next ff))))))
	   (show-comp-props ()
	     (loop for c in *components*
		do (format t "~&~D : ~D" c (gethash c *current-form*)))))
    '(show-comp-props)
    `(with-syms (,@*symbols*)
       (let ((bforms ,*current-form*)
	     (psyms ',*psyms*)(ssyms ',*ssyms*)
	     (snn (build-snn ,(create-build-forms) :symbols ,*symbols* :extra-run ,*extra-run* :display-build nil)))
	 (declare (ignorable bforms psyms ssyms))
	 ,(pprint *symbols*)
	 (macrolet ((monitor-value (component type)
		      (let ((cf (gethash component *current-form*)))
			(if (not cf) (break (format nil "Unknown component: ~D!" component))
			    (if (or (and (eq type 's) (not (find :s cf)))
				    (and (eq type 'p) (not (find :p cf))))
				(break (format nil "No ~D monitor is set for ~D." type component))
				`(symbol-value ,(getf (gethash component *current-form*)
						      (switch type (('s :s)('p :p))
							      :default (break (format nil "Monitor type should be s or p, not: ~D" type)))))))))
		    (monitor-values (type &body components)
		      `(values ,@(mapcar (lambda (c) `(monitor-value ,c ,type)) components)))
		    (monitor-list (type &body components)
		      `(list ,@(mapcar (lambda (c) `(monitor-value ,c ,type)) components)))
		    (plot (&body components)
		      `(progn 
			 (set-to-octave)
			 (syms-plot 
			  (,@(mapcan (lambda (c) (let ((x (getf (gethash c *current-form*) :p)))
						   (if x (list x))))  components))
			  (,@(mapcan (lambda (c) (let ((x (getf (gethash c *current-form*) :s)))
						   (if x (list x))))  components))
			  t nil nil))))	      
	   (labels ((snn-run (ms &optional (step 0.1))
		      (monitor-exit (funcall snn ms step)))
		    (set-to-octave ()
		      ,@(mapcar (lambda (s) `(cl-octave::octave/sym-set ,s)) *symbols*))
		    (plot-all ()
		      ;(pprint `(syms-plot ,psyms ,ssyms ,@args))
		      (set-to-octave)
		      (syms-plot (,@*psyms*) (,@*ssyms*) t nil nil)))
	     (cl-octave::octave/run t t)
	     ,@code
	     ))))))

;; stubs (look inside create-snn-closure for actual functions
(defun monitor-value (component type)
  (declare (ignore component type)))
(defun monitor-values (type &rest components)
  (declare (ignore components type)))
(defun monitor-list (type &rest components)
  (declare (ignore components type)))
(defun snn-run (ms &optional (step 0.1))
  (declare (ignore ms step)))
(defun set-to-octave nil)
(defun plot-all nil)

(defmacro monitor-print (&body code)
  (let ((code-str (format nil "~D" code)))
    `(progn
       (format t "~&Entering code: ~D~%" ,code-str)
       (prog1
	   (progn ,@code)
	 (format t "Exited")))))

(defmacro monitor-exit (&body code)
  (let ((code-str (format nil "~D" code)))
    `(progn 
       (let ((flag)(ret))
	 (unwind-protect 
	      (progn 
		(setf ret (progn ,@code) flag t))
	   (if (null flag)
	       (format t "~&Broke inside: ~D" ,code-str)))
	 ret))))
	       
(defun put-connection (pre post)
;     (if (not (gethash ',pre *current-form*)) 
;	 (setf (getf (gethash ',pre *current-form*) :sensory) t))
  (if (not (find pre *components*))
      (push pre *components*))
  (push post (getf (gethash pre *current-form*) :next)))
 ; (format t "after put-connection ~D, ~D, *components*= ~D" pre post *components*))

(defun feedforward-snn (&rest layers)
;  (format t "~D" layers)
;  (break "ffsnn called!")
  (mapcar (lambda (preobs postobs)
	    (let ((joiner (lambda (a) (if (listp a) a (list a)))))
	      (setf preobs (mapcan joiner preobs) postobs (mapcan joiner postobs)))
	    (loop for p1 in preobs
	       do (loop for p2 in postobs
		     do (put-connection p1 p2))))
	  layers (append (cdr layers) (list '(ter)))))

(defun reset-reader ()
  (progn (setf *current-form* (make-hash-table)) 
	 (setf *components* nil *symbols* nil *psyms* nil *ssyms* nil *extra-run* nil)))
    
(defun testcall (fn &rest args)
  (format t "Calling ~D with ~D" fn args)
  (prog1 (apply fn args)
    (format t "Exited")))

(defun string-from-stream (stream delimit-char1 &optional delimit-char2 nil-end)
  (let ((out
	 (loop named reader 
	    with got-char1 = nil
	    for read-ch = (peek-char nil stream nil nil t)
	    if (or (eq delimit-char1 read-ch) (and nil-end (null read-ch)))
	    do (setf got-char1 t)
	    end
	    until (and got-char1 (or (not delimit-char2) (eq (peek-char nil stream nil nil t) delimit-char2)))
	    until (and (null read-ch) got-char1 (null delimit-char2))
	    if (null read-ch)
	    do (break (format nil "Characters ~D and ~D not found in stream !" delimit-char1 delimit-char2))
	    end
	    if (not (eq delimit-char1 read-ch))
	    do (setf got-char1 nil)
	    end
	    collect (read-char stream nil nil t))))
    (coerce (if (not delimit-char2) out (subseq out 0 (1- (length out)))) 'string)))

(defun dotted-word (string &optional (prefix "") (fixdelimit "-"))
  (destructuring-bind (fixpart nums)
      (if fixdelimit
	  (cl-ppcre::split fixdelimit string)
	  (list "" string))
    (let ((intstart)(intstep 1)(intstop)
	  (split (cl-ppcre::split "[.]" nums)))
      (setf intstart (read-from-string (first split)))
      (if (= (length split) 3)
	  (setf intstop (read-from-string (third split)))
	  (if (= (length split) 5)
	      (setf intstep (read-from-string (third split)) intstop (read-from-string (fifth split)))
	      (progn
		;;(assert (<= (length split) 5) (string)
		;;(format nil "Unknown number of arguments provided in ~D. Format is: nstart..nstop or nstart..nstep..nstop" string))
		(return-from dotted-word (dotted-word string prefix fixdelimit)))))
      (loop for i from intstart to intstop by intstep
	 collect (read-from-string 
		  (concatenate 'string prefix fixpart (write-to-string i)))))))

(defun empty-string-p (string)
  (loop for ch in (coerce string 'list) always (or (eq ch #\Space) (eq ch #\Newline) (eq ch #\Tab) )))

(defun merge-brackets (string-list &optional acc1 acc2 (bcount 0))
  (let ((str (first string-list)))
    (if (find #\( str) (incf bcount))
    (if (> bcount 0)
	(setf acc2 (concatenate 'string acc2 " " str))
	(if str (push str acc1)))
    (if (find #\) str) (decf bcount))
    (if (and (= bcount 0) acc2) (progn (push acc2 acc1) (setf acc2 nil)))
    (if str (merge-brackets (rest string-list) acc1 acc2 bcount) (nreverse acc1))))

;(set-syms p n1 n2 n3) -> (progn (setf |n1-potn| (gensym) |n2-potn| (gensym) |n3-potn| (gensym))
;				(setf (getf (gethash n1 *current-form*) :p) |n1-potn|) ... )

(defmacro set-syms (sym &body comps)
  (let ((potn-syms
	 (loop for c in comps collect (make-symbol (format nil "~D_~D" c (if (eq sym 'p) 'p 's))))))
    (setf *symbols* (append *symbols* potn-syms))
    (mapcar (lambda (symb psym) 
	      (setf (getf (gethash symb *current-form*)
			  (if (eq sym 'p) (prog1 :p (push psym *psyms*))
			      (if (eq sym 's) (prog1 :s (push psym *ssyms*))
				  (progn (format t ">") (pprint sym) (format t "<"))))) psym))
	    comps potn-syms)
    `(setf ,@(mapcan (lambda (a) (list a '(gensym))) potn-syms))	   
    nil))

(defun set-comp-props (&key components fixed-args list-args)
  (loop for c in components
     do (loop for fa in fixed-args
	   do (progn
		;;(format t "~D->~D = ~D  " c (first fa) (second fa))
		(setf (getf (gethash c *current-form*) (first fa)) (second fa)))))
  (loop for varg in list-args
     do (loop for vnum in (second varg)
	   for c in components
	   do (progn
		;;(format t "~D->~D = ~D  " c (first varg) vnum)
		(setf (getf (gethash c *current-form*) (first varg)) vnum)))))

(set-snn-define-macros)
