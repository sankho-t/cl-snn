(in-package :snn)

(defun average (list)
  (/ (apply #'+ list) (length list)))

(defun get-spike-times (result)
  (if result
      (if (not (zerop (second (first result)))) 
	  (append `(,(ffirst result)) (get-spike-times (rest result)))
	  (get-spike-times (rest result)))))

(defun earlier-time-stamps (time list)
  (cond (list 
	 (if (< (first (car list)) time) 
	     (append (list (car list)) (earlier-time-stamps time (cdr list)))))))

(defun ffirst (list)  (first (first list)))

(defun int-power (base exp &optional (acc 1))
  (declare (type real base acc))
  (declare (type integer exp))
  (if (zerop exp) acc
      (int-power base (1- exp) (* base acc))))

(defun round-equal (value1 value2 &optional (decimal-places 3))
  (declare (type single-float value1 value2))
  (declare (optimize (speed 3)))
  (and (not (null value1)) (not (null value2))
       (= (round (* value1  (int-power 10 decimal-places))) 
	  (round (* value2  (int-power 10 decimal-places))))))

(defun get-min-cond-plist (args cond-key val-key)
  (car (sort (let ((col)) (cond-list-formeval 
			   (lambda (N) (push (getf N val-key) col)) args #'(lambda (N) (getf N cond-key))) col) #'<)))

(defun create-time-list (end-time &optional (this-time 0) )
  (cond ((< this-time end-time) (append `( ,(list this-time (random 10))) (create-time-list end-time (+ this-time 1))))))

(defun factorial (n &optional (acc 1))
  (declare (type integer n acc))
  (if (> n 0) (factorial (1- n) (* acc n))
      acc))

(defun bubble-sort (array column)
  (macrolet ((swap (v1 v2)
	       `(let ((v1 ,v1)(v2 ,v2))
		  (setf ,v2 v1)
		  (setf ,v1 v2))))
    (let ((len (second (array-dimensions array))))
      (dotimes (x len)
	(do ((y 0 (1+ y))) ((= (1+ y) len) t)
	  (when (> (aref array y column) (aref array (1+ y) column))
	    (loop for i from 0 to (1- (second (array-dimensions array)))
		 do (swap (aref array y i) (aref array y (1+ y)))))))
      array)))

(defun bubble-2 (array column)
  (macrolet ((swap (v1 v2)
	       `(let ((v1 ,v1)(v2 ,v2))
		  (setf ,v2 v1) (setf ,v1 v2))))
    (let ((rows (1- (first (array-dimensions array))))
	  (swapped t))
      (loop for i from (1- rows) downto 0 until (not swapped)
	 do (progn (setf swapped nil)
		   (loop for j from 0 to i
		      do (if (> (aref array j column) (aref array (1+ j) column))
			     (progn
			       (setf swapped t)
			       (loop for x from 0 to (1- (second (array-dimensions array)))
				  do (swap (aref array j x) (aref array (1+ j) x)))))))))
    array))
	   
(defun poisson-train (x-range delx spikes &optional (x-mean (/ x-range 2)))
  (assert (integerp (/ x-range delx)) (x-range delx) "non integral poisson distribution domain")
  (let* ((p-dist (make-array `(,(/ x-range delx) 2)))
	 (ind-max (first (array-dimensions p-dist))))
    (loop
       for index from 0 to (1- ind-max)
       for x-val from 0 to x-range by delx
       do (progn 
	    (setf (aref p-dist index 0) x-val)
	    (let ((p (/ (* (expt x-mean x-val) (exp (- x-mean))) (factorial x-val))))
	      (setf (aref p-dist index 1) p)
	      (setf (aref p-dist index 2) (* p (random 10))))))
    p-dist))

(defun random-train (spikes duration)
  (sort (let ((sptimes))
	  (loop until (= (length sptimes) spikes)
	     do (progn 
		  (push (random duration) sptimes)
		  (setf sptimes (remove-duplicates sptimes))))
	  sptimes) #'<))

(defmacro if-ret (condn &optional else)
  `(let ((ret ,condn))
     (if ret
	 ret
	 ,else)))

(defmacro if-then (&body args)
  (labels ((seek-between (start stop &key from-start to-end)
	     (let ((start (position start args))
		   (stop  (if-ret (position stop args) (length args))))
	       (loop 
		  for o in args
		  for i from 0 to (length args)
		  when (and (if from-start t (> i start))
			    (if to-end t (< i stop)))			   
		  collect o))))
    `(if (progn ,@(seek-between nil 'then :from-start t))
	 (progn ,@(seek-between 'then 'else))
	 (progn ,@(if (find 'else args)
		      (seek-between 'else nil :to-end t))))))

(defmacro if-else (&body args)
  (labels ((seek-between (start stop &key from-start to-end)
	     (let ((start (position start args))
		   (stop  (if-ret (position stop args) (length args))))
	       (loop 
		  for o in args
		  for i from 0 to (length args)
		  when (and (if from-start t (> i start))
			    (if to-end t (< i stop)))			   
		  collect o))))
    `(if ,(first args)
	 (progn ,@(subseq args 1 (position 'else args)))
	 (progn ,@(if (find 'else args)
		      (seek-between 'else nil :to-end t))))))

(defmacro ->DEB (format-string &rest vars) 
  `(cond (debug (format t ,format-string ,@vars))))

(defmacro test-exp (N &body code)
  `(progn
     (format t "~&Entering ~D" ,N)
     ,(if (= (mod N 2) 0)
	  (if (> (length code) 1)
	      `(pprint (quote ,code))
	      `(pprint (quote ,@code))))
     (prog1
	 ,(if (< N 0) 
	      `(progn (format t "~&Skipping") t)
	      `(progn ,@code))
       (format t "~&Exited ~D" ,N))))

(defmacro test-comp (n &body code)
  (progn
    (format t "~&Compiling ~D" n)
    (prog1
	`(progn ,@code)
      (format t "~&Compiled ~D" n))))

(defmacro avg (&body nums)
  `(/ (reduce #'+ ,@nums) (length ,nums)))

(defun create-sym (sym-name count)
  (make-symbol (concatenate 'string (symbol-name sym-name) (write-to-string count))))

(defmacro switch (test-form val-exform-pairs &key default (test #'eq))
  (if val-exform-pairs
      `(let ((method (concatenate 'string "switch " ,(write-to-string test-form))))
	 method
	 (if (funcall ,test ,test-form ,(caar val-exform-pairs))
	     (progn ,@(cdar val-exform-pairs))
	     (switch ,test-form ,(cdr val-exform-pairs) :default ,default :test ,test)))
      `,default))

(defmacro replace-char (search replace string)
  `(coerce (mapcar #'(lambda (C) (cond ((eq C ,search) ,replace) ('t C))) (coerce ,string 'list)) 'string))

(defmacro conc-string (&rest num-strs)
  (labels ((make-arg (args)
	     (if args
		 (if (eq (first args) :num)
		     (append `((write-to-string ,(second args))) (make-arg (cddr args)))
		     (append `(,(first args)) (make-arg (cdr args)))))))
    `(concatenate 'string ,@(make-arg num-strs))))
	
(defmacro clamp (value min max &optional disp-warn)
  (let ((disp-msg `(format t "Warning: Form ~D is beyond range, with value " (quote ,value))))
    `(let ((i ,value))
       (cond ((< i ,min)
	      (if ,disp-warn (progn ,disp-msg (format t "~D" i)))
	      ,min)
	     ((> i ,max)
	      (if ,disp-warn (progn ,disp-msg (format t "~D" i)))
	      ,max)
	     (t i)))))

(defmacro sv (symbol)
  `(symbol-value ,symbol))
  
(defmacro set-input (symbol value)
  `(set ,symbol (clamp (+ (/ ,value 2) 3) 3 4.4 t)))

(defmacro setq-input (symbol value)
  `(setq ,symbol (clamp (+ (/ ,value 2) 3) 3 4.4 t)))

(let ((count)(cal 0))
  (defun switch-run (fn)
    (lambda ()
      (incf cal)
      (cond ((= (mod cal count) 0)
	     (funcall fn)))))
  (defun switch-set (times)
    (print "switch-set")
    (setf count times)))

(defmacro for-array (array loop-dim loopvar result &body body)
  `(do ((,loopvar 0 (+ ,loopvar 1)))
       ((= ,loopvar (array-dimension ,array ,loop-dim)) ,result)
     ,@body))

(defmacro mactry2 (&body all)
  `(+ ,@all *reduced-step*))

(defmacro push-end (value place)
  `(cond ((zerop (length ,place)) (setq ,place (list ,value)))
	 (t (setq ,place (append ,place (list ,value))))))

(defmacro push-end2 (value place)
  `(cond ((zerop (length ,place)) (setf ,place (list ,value)))
	 (t (nconc ,place (list ,value)))))

(defmacro remove-earlier-times (place time)
  `(loop
      (cond ((null (car ,place)) (return))
	    ((<= (ffirst ,place) ,time) (setf ,place (cdr ,place)))
	    (t (return)))))

(defmacro remove-earlier-times2 (place time)
  `(loop
      (cond ((null ,place) (return))
	    ((<= (ffirst ,place) ,time) 
	     (->DEB " Popping @~D " ,time)
	     (pop ,place))
	    (t (return)))))

(defmacro search-plist (place key value)
  `(find ,value ,place :test #'(lambda (v tst) (eq v (getf tst ,key)))))

(defmacro cond-list-formeval (fn args cond-fn)
  (let ((collector (gensym))(iterator (gensym)))
    `(let ((,collector))
       (dolist (,iterator ,args ,collector)
	 (cond ((not (null (funcall ,cond-fn ,iterator)))
		(push (funcall ,fn ,iterator) ,collector)))))))

(defun ap-list (&key ((:startat start) 0) ((:stopat stop) 0) ((:step step) 1))
  (cond ((not (> (* start step) (* stop step)))
	 (append `(,start) (ap-list :startat (+ start step) :stopat stop :step step)))))

(defmacro step-size (steps)
  `(1+ (length ,steps)))

(defmacro setf-t (&body vars)
  `(progn 
     ,@(mapcar (lambda (v) `(setf ,v t)) vars)))

(defmacro setf-nil (&body vars)
  `(progn 
     ,@(mapcar (lambda (v) `(setf ,v nil)) vars)))

(defmacro steping-2 (steps)
  `(if (null ,steps) 1 4))

(defmacro set&plot (potnsyms &optional spikesyms (print-legend t) window-title (show-legend (not cl-octave::*LEGACY*)))
  `(progn
     (octave/sym-set ,@potnsyms ,@spikesyms)
     (syms-plot ,potnsyms ,spikesyms ,print-legend ,window-title ,show-legend)))

(defmacro concstr (&body args)
  `(concatenate 'string ,@args))

(defmacro syms-plot (potnsyms &optional spikesyms (print-legend t) window-title (show-legend t))
  (let ((colors '(r g b k m c y))(cind -1)(cindex))
    (labels ((comma-list (syms)
	       (mapcar (lambda (s) 
			 (let ((sname (symbol-name s))
			       (cname (string-downcase (symbol-name (nth (mod (incf cind) 
									      (length colors)) colors)))))
			   (setf cindex (append cindex `((,sname ,cname))))
			   (concstr "," sname "(:,1)," sname "(:,2),'" cname "'")))
		       syms)))
      `(progn
	 (let ((should-plot "")(draw-legend (if ,show-legend "" "%")))
	   (let ((nullvars))
	     ,@(mapcar (lambda (var)
			 `(if (null (symbol-value ,var)) (setf nullvars (list* ',var nullvars))))
		       spikesyms)
	     (if-then nullvars then (format t "~&~{~a~#[~;, and ~:;, ~]~} is null." nullvars))
	     ,@(mapcar (lambda (svar)
			 (let ((sname (symbol-name svar)))
			   `(octave/exec "if (size(" ,sname ",2)==1), " 
					 ,sname "= [" ,sname " rand(1,1)*ones(size(" ,sname ",1))]; end")))
		       spikesyms)
	     ,(if potnsyms
		  `(octave/exec should-plot "plot("
				(subseq	 ,(let ((pargs (comma-list potnsyms)))
					       `(funcall (lambda () (concstr ,@pargs))))
					 1) ");"))
	     (octave/exec should-plot "hold on")
	     (octave/exec should-plot
			  ,(if spikesyms
			       (reduce (lambda (a b) (concstr a b))
				       (mapcar (lambda (s v)
						 (concstr 
						   "if sum(size(" (symbol-name v) "))~=0, "  "stem(" 
						   (subseq s 1) "); end;"))
					       (progn (setf cind -1) (comma-list spikesyms)) spikesyms))))
	     ;;	   (octave/exec should-plot "hold off")
	     ,(if print-legend `(format t "~&Plot legend: ~D~%" (quote ,cindex)))
	     (octave/exec should-plot draw-legend "legend("
			(subseq
			 ,(let ((syms (mapcar (lambda (N) (concstr ",'" (first N) "'")) cindex)))
			       `(funcall (lambda () (concstr ,@syms))))
			 1)
			",'location','southeastoutside');")
	   (and cl-octave::*LEGACY* (progn (setf draw-legend "%") 
					   (format t "Dont know how to draw legend in this octave !")))
	   (octave/exec should-plot draw-legend "legend('boxon')")
	   (octave/exec should-plot draw-legend "legend('orientation','horizontal')")
	   (octave/exec should-plot draw-legend "xlabel('Time (ms)')")
	   (octave/exec should-plot draw-legend "ylabel('Potential (mV)')")
	   ,(if window-title `(octave/exec "title('" ,window-title "');"))
	   (octave/exec should-plot "figure")
	   ))))))

(defun reset-syms-fn (&rest syms)
  (dolist (i syms) 
    (set i nil)))

(defmacro value-syms (&body syms)
  (labels ((r (syms)
	     (cond (syms
		    (append `((format t "~&(symbol-value ~D)= ~D" ,(format nil "~D" (car syms)) (symbol-value ,(car syms))))
			    (r (rest syms)))))))
    `(progn ,@(r syms))))

(defmacro maptimes (times fn)
  (let ((_times (gensym))(_collect (gensym))(_i (gensym)))
    `(let ((,_times ,times)(,_collect))
       (dotimes (,_i ,_times ,_collect)
	 (setf ,_collect (append ,_collect (list (funcall ,fn))))))))

(defmacro current (&key ((:symbol sym) nil) ((:value value) 0) ((:right-shift rshift) 0))
  `(list 
    :whois #'(lambda () "analog in")
    :current #'(lambda (tn) (cond ((> tn ,rshift) 
				   ,(cond ((null sym) value)
					  ('T `(symbol-value ,sym))))
				  ('T 0)))))
(defmacro try (a b)
  `(list :bla #'(lambda (d) (+ ,a ,b c d))))

(defmacro with-name (name &body code)
  (if *MEMCALL-DISPLAY*	 
      `(block name-block (let ((myname ,name)) ,@code))
      `(block name-block (progn ,@code))))

(defmacro with-syms (syms &body code)
  (labels ((gs (syms)
	     (cond (syms
		    (append `((,(car syms) (gensym ,(write-to-string (car syms)))))
			    (gs (rest syms)))))))
    `(let ,(gs syms) ,@code)))

(defmacro def-syms (&rest syms)
  (labels ((gs (syms)
	     (cond (syms
		    (append `((defvar ,(car syms) (gensym))) (gs (rest syms)))))))
    `(progn ,@(gs syms))))

(defmacro memcall (obj tag &body args)
  `(let ((fun (getf ,obj ,tag)))
     (and (null fun) (break (format nil "No ~D tag found in closure ~D" ,tag ,obj)))
     ,(if (and *MEMCALL-DISPLAY* (or (find tag *MEMCALL-DISPLAY*) (find :all *MEMCALL-DISPLAY*)))
	  `(if (and *MEMCALL-DISPLAY* (or (find ,tag *MEMCALL-DISPLAY*) (find :all *MEMCALL-DISPLAY*)))
	       (let ((callcount (incf *CALL-COUNTER*))
		     (objname (if (getf ,obj :whois) (funcall (getf ,obj :whois)) "<unnamed>")))
		 (format t "(~A) ~D -> (~D ~D ~{ ~S~})  " callcount myname objname ,tag `(,,@args))
		 (let ((tmp (funcall fun ,@args)))
		   (format t "Exited (~A)~%" callcount)
		   tmp))
	       (funcall fun ,@args))
	  `(funcall fun ,@args))))

(defmacro return-first (exp1 &body exp-rest)
  (let ((retvar (gensym)))
    `(let ((,retvar ,exp1))
       (progn ,@exp-rest ,retvar))))

(defmacro memcall-exp (tag call-sv &rest objs)
  (labels ((make-call-list (objs)
	     (cond (objs
		    (append (if call-sv `((memcall (sv ,(car objs)) ,tag))
				`((memcall ,(car objs) ,tag)))
			    (make-call-list (rest objs)))))))
    `(progn ,@(make-call-list objs))))

(defmacro memcall-list (tag &rest objs)
  (labels ((make-call-list (objs)
	     (cond (objs
		    (append `((dolist (i ,(car objs) t)  (memcall i ,tag) ))
			    (make-call-list (rest objs)))))))
    `(progn ,@(make-call-list objs))))

(defmacro memcall-list-1 (tag arg &rest objs)
  (labels ((make-call-list (objs)
	     (cond (objs
		    (append `((let ((out)) (dolist (i ,(car objs) out)  (setq out (append (list (memcall i ,tag ,arg)) out)) )))
			    (make-call-list (rest objs)))))))
    `(progn ,@(make-call-list objs))))

(defmacro memcall-exp-list (tag &key ((:listcall listcall) nil listcallp) ((:atomcall atomcall) nil atomcallp)(call-val))
  `(progn
     ,@(append (cond (listcallp `((memcall-list ,tag ,@listcall))))
	       (cond (atomcallp `((memcall-exp ,tag ,call-val ,@atomcall)))))))

(defun expand-sym-list (symlist)
  (cond (symlist
	 (append (list (symbol-value (car symlist))) (expand-sym-list (rest symlist))))))

(defun create-sym-list (N &optional (name "") (num 0))
  (cond ((not (zerop N))
	 (let ((x (gensym (concatenate 'string name (write-to-string num) "_"))))
	   (set x (random 10))
	   (append (list x) (create-sym-list (1- N) name (1+ num)))))))

(defmacro units-form (exp &body mult)
  `(float (* ,@mult (expt 10 ,exp))))

(defmacro mili (&body mult)
  `(units-form -3 ,@mult))
(defmacro nano (&body mult)
  `(units-form  -9 ,@mult))
(defmacro mega (&body mult)
  `(units-form 6 ,@mult))
(defmacro kilo (&body mult)
  `(units-form 3 ,@mult))
