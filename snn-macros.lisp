(in-package SNN)

(defvar *MEMCALL-DISPLAY* `())
(defvar *CALL-COUNTER*)

(eval-when (:compile-toplevel)
  (setf *MEMCALL-DISPLAY* `())
  (format nil "~&Define symbols, build network, then simulate."))

(defmacro build-snn (build-forms &key symbols sym-lists extra-init extra-run display-build display-making
				      (debug-file "debug-auto") (display-progress 5))
  "compiles a network as specified in build-forms, auto scans the build-forms to find out
the neurons, synapses, etc. and creates the run function"
  (let ((atom-neurons)(synapses)(self-neurons)(list-neurons)(terms)(synapse-forms)(non-synapse-forms))
    (labels ((seek-car (match-car)
	       (loop for L in build-forms when (eq (first (cadr L)) match-car) collect (first L)))
	     (seek-cars (&rest match-cars)
	       (labels ((seek-cars2 (match-cars2)
			  (if match-cars2
			      (append (reverse (seek-car (car match-cars2))) (seek-cars2 (rest match-cars2))))))
		 (seek-cars2 match-cars)))
	     (sym-synapse (sym) (or (eq sym 'synapse) (eq sym 'snn::synapse)))
	     (sym-neuron (sym) (or (eq sym 'neuron) (eq sym 'snn::neuron)))
	     (sym-spiker (sym) (or (eq sym 'spiker) (eq sym 'snn::spiker)))
	     (sym-delay-net (sym) (or (eq sym 'delay-net3*) (eq sym 'snn::delay-net3*)))
	     (sym-terminator (sym) (or (eq sym 'terminator) (eq sym 'snn::terminator)))
	     (sym-bunch (sym) (or (eq sym 'neuron-bunch) (eq sym 'snn::neuron-bunch)))
	     (seek-p (key)
	       (loop for L in build-forms when (and (not (= (length (cadr L)) 1))
						    (or (and (sym-terminator (caadr L)) (getf (cdadr L) key))
							(getf (cadr L) key)))
		    collect (first L)))
	     (seek-sensory-bunch ()
	       (loop for L in build-forms when (and (or (sym-bunch (caadr L)) (sym-delay-net (caadr L)))
						    (getf (cadr L) :rest-args)
						    (let ((arg (getf (cadr L) :rest-args)))
						      (or (getf arg :in) (getf arg :sensory) (getf arg :in-v) 
							  (getf arg :in-s)))
						    t)
		    collect (car L)))
	     (process-create-list ()
	       (loop for L in build-forms collect (destructuring-bind (name (&rest call-list)) L
						    (and (evenp (length call-list))
							 (and (not (getf call-list :name))
							      (setf (getf call-list :name) (symbol-name name)))))))
	     (put-name (exp)
	       (let ((name (car exp)))
		 (if (let ((sym (caadr exp)))
		       (or (sym-synapse sym) (sym-neuron sym) (sym-spiker sym)))
		     (if (null (getf (cadr exp) :name))
			 (setf (cadr exp) (append (cadr exp) `(:name ,(symbol-name name))))))
		 (if (let ((sym (caadr exp)))
		       (or (sym-bunch sym) (sym-delay-net sym)))
		     (if (getf (cadr exp) :rest-args)
			 (if (null (getf (getf (cadr exp) :rest-args) :name))
			     (setf (getf (getf (cadr exp) :rest-args) :name) (symbol-name name)))))))
	     (fix-synapse-list ()
	       (mapcar (lambda (ex)
			 (let ((ex2 (copy-list ex)))
			   (if (sym-synapse (caadr ex2))
			       (destructuring-bind (name (type psn &rest rest)) ex
				 (if (not (listp psn))
				     (setq psn `(list ,psn)))
				 `(,name (,type ,psn ,@rest)))
			       ex2)))
		       build-forms))
	     (init-build ()
	       (append `((memcall-exp-list :dry-run 
					   ,@(cond (atom-neurons `(:atomcall ,atom-neurons)))
					   ,@(cond (list-neurons `(:listcall ,list-neurons)))))
		       `((memcall-exp-list :dry-run-2 ,@(cond (synapses `(:atomcall ,synapses :call-val t)))))
		       extra-init)))
      (block create-block
	`(with-name "with-snn"
	   (setf *CALL-COUNTER* 0)
;	   (setf *silent* ,display-struct)
	   ,(dolist (i build-forms) (put-name i))
	   ,(if display-build (pprint build-forms))
	   ,(progn (setf build-forms (fix-synapse-list)
			 synapse-forms (let ((collect))
					 (dolist (f build-forms collect) 
					   (if (sym-synapse (caadr f)) (push f collect))))
			 non-synapse-forms (remove-if (lambda (a) (eq a 'synapse)) build-forms :key #'caadr))  nil)
	   (let* ,(append (mapcar (lambda (s) (list s `(gensym "synapse"))) (mapcar #'car synapse-forms))
			  (if (sym-terminator (caadar non-synapse-forms)) 				  
			      non-synapse-forms (reverse non-synapse-forms)))
	     ,@(mapcar (lambda (sform) (append `(set) sform)) synapse-forms)
	     ,(format display-making "~&Neurons: ~D" 
		      (setf atom-neurons (let ((N (seek-cars 'neuron 'spiker)))
					   (cond ((null N) (format t "~&No neuron found !!")
						  '(return-from create-block))) N)))
	     ,(format display-making "~&Synapses: ~D" 
		      (setf synapses (let ((N (seek-cars 'synapse)))
				       (cond ((null N) (format t "~&No synapse found !!")
					      (return-from create-block)))  N)))
	     ,(format display-making "~&Neuron bunches: ~D" 
		      (setf list-neurons (seek-cars 'neuron-bunch 'delay-net3*)))		      
	     ,(format display-making "~&Start neurons: ~D" 
		      (setf self-neurons 
			    (let ((N (reverse (remove-duplicates
					       (append (seek-p :in) (seek-p :sensory) (seek-p :spike-times) 
						       (seek-p :in-v) (seek-p :in-s)
						       (mapcar (lambda (N) (cons N t)) (seek-sensory-bunch)))))))
			      (cond ((null N) (format t "~&No sensory neurons found !!") 
				     (return-from create-block))) N)))
	     ,(progn (setq terms (let ((N (seek-cars 'terminator)))
				   (cond ((null N) (format t "~&No terminators found !!") 
					  (return-from create-block)))  N)) t)
	     ,@(init-build)
	     (lambda (net-time &optional (delta_t 0.1))
	       (declare (type fixnum net-time delta_t))
	       (declare (optimize (speed 3)))
;	       ,(if *MEMCALL-DISPLAY* `(format *MEMCALL-DISPLAY* "debug-diplay is ON"))
	       (memcall-exp-list :reset ,@(cond (atom-neurons `(:atomcall ,atom-neurons))) 
				 ,@(cond (list-neurons `(:listcall ,list-neurons))))
	       (memcall-exp-list :reset ,@(cond (synapses `(:atomcall ,synapses :call-val t))))
	       (memcall-exp-list :reset ,@(cond (terms `(:atomcall ,terms))))
	       (setq *reduced-step* '())
	       (reset-syms ,@symbols)
	       ,(cond (sym-lists `(mapcan (lambda (s) (set s nil)) ,sym-lists)))
	       ,@extra-run
	       (let ((backp))
		 (with-open-file (*debug-stream* (make-pathname :name ,debug-file :type "txt")
						 :direction :output :if-exists :overwrite :if-does-not-exist :create)
		   backp
		   (unwind-protect 
			(let* ((simtime 0))
			  (declare (type real simtime))
			  (if ,display-progress
			      (setf backp 
				    (ccl:process-run-function 
				     "status" 
				     (lambda ()
				       (format t "~&<---- Simulation start")
				       (unwind-protect
					    (progn 
					      (loop
						 (sleep ,display-progress)
						 (if (>= simtime net-time) (return)
						     (format t "~&Progress= ~1$%, step reduction= ~D~%" 
							     (* 100 (/ simtime net-time)) (step-size *reduced-step*) )))
					      (format t "~&Progress= 100%"))
					 (format t "~&Simulation over ---->"))))))
			  (loop 
			     (cond ((> simtime net-time) (return t)))
			     (run-snn simtime delta_t (step-size *reduced-step*) ,@self-neurons 
				      ,@(reverse (mapcan (lambda (s) (list (cons s nil))) synapses)))
			     (if (< (step-size *reduced-step*) 0.001) (return t))
			     (incf simtime (/ delta_t (step-size *reduced-step*)))))
		   (if ,display-progress (ccl:process-kill backp))))))))))))

(defmacro delay-net3* (delay-list postsynapse bla &key 
		       ((:potnsyms potnsyms)) ((:spikesyms spikesyms)) ((:weightsyms weightsyms))((:name myname) "dnet")
		       ((:rest-args rest-args)))
  (declare (ignorable bla myname))
  (let ((count 0))
    (labels ((create-neuros ()
	       (if delay-list
		   (append `((neuron ,postsynapse 
				     :name ,(format nil "~D" (car delay-list))
				     :delay ,(car delay-list)
				     ,@(cond (potnsyms
					      (prog1 (let ((psym (car potnsyms)))   `(:p ,psym))
						(setq potnsyms (rest potnsyms)))))
				     ,@(cond (spikesyms
					      (prog1  (let ((ssym (car spikesyms)))  `(:s ,ssym))
						(setq spikesyms (rest spikesyms)))))
				     ,@(cond (weightsyms
					      (prog1  (let ((wsym (car weightsyms))) `(:weight-sym ,wsym))
						(setq weightsyms (rest weightsyms)))))
				     ,@(cond (rest-args rest-args))))
			   (progn (setf delay-list (rest delay-list)) (create-neuros)))
		   (progn (format nil "~&delay-net max= ~D" count) nil))))
      `(list ,@(create-neuros)))))

(defmacro neuron-bunch (size postsynapse constant-args variable-keys &rest fn-key-index)
  `(list 
    ,@(loop
	for i from 1 to size
	collect `(neuron ,postsynapse ,@constant-args ,@(mapcan #'(lambda (a b) (list a (funcall b i)))
								variable-keys fn-key-index)))))

(defmacro run-snn (stime dt rstep &rest elements)
  (labels ((run (elmnts)
	     (cond ((and elmnts (not (consp (first elmnts))))
		    (let ((ret (gensym))(elm (first elmnts)))
		      `(let ((,ret (memcall ,elm :run ,stime)))
			 (cond ((getf ,ret :sim-break)
				(setq ,stime (- (getf ,ret :simtime) (/ ,dt ,rstep)))
				(return-from run-block))
			       (t ,(run (rest elmnts)))))))
		   ((and elmnts (consp (first elmnts)) (not (cdr (first elmnts))))
		    (let ((ret (gensym))(elm (car (first elmnts))))
		      `(let ((,ret (memcall (sv ,elm) :run ,stime)))
			 (cond ((getf ,ret :sim-break)
				(setq ,stime (- (getf ,ret :simtime) (/ ,dt ,rstep)))
				(return-from run-block))
			       (t ,(run (rest elmnts)))))))
		   ((and elmnts (consp (first elmnts)))
		    (let ((ret (gensym))(ret2 (gensym))(elm (car (first elmnts))))
		      `(let* ((,ret (memcall-list-1 :run ,stime ,elm))
			      (,ret2 (mapcan (lambda (N) (if (numberp (getf N :simtime)) 
							     (list (getf N :simtime)))) ,ret)))
			 (cond (,ret2
				(setq ,stime (- (first (sort ,ret2 #'<)) (/ ,dt ,rstep)))
				(return-from run-block))
			       (t ,(run (rest elmnts)))))))
		   (t `(progn ,@(cleanup elements)))))
	   (cleanup (elmnts)
	     (cond (elmnts
		    (cond ((not (consp (first elmnts)))
			   (append `((memcall ,(first elmnts) :cleanup-after-run))
				   (cleanup (rest elmnts))))
			  ((and (consp (first elmnts)) (not (cdr (first elmnts))))
			   (append `((memcall (sv ,(car (first elmnts))) :cleanup-after-run))
				   (cleanup (rest elmnts))))
			  ((and (consp (first elmnts)) (cdr (first elmnts)))
			   (append `((memcall-list :cleanup-after-run ,(car (first elmnts))))
				   (cleanup (rest elmnts))))))
		   )))
    (if *MEMCALL-DISPLAY*
	`(let ((myname "runner"))
	   (declare (ignorable myname))
;	   (if (> ,stime 6) (break))
	   (block run-block ,(run elements)))
	`(block run-block ,(run elements)))))

(defmacro reset-syms (&body syms)
  (labels ((r (syms)
	     (cond ((not (null syms)) (append `((set ,(car syms) nil)) (r (rest syms)))))))
    `(progn ,@(r syms))))

(defmacro build-network-nD-1C (dimensions learn-vals &optional (data-marker))
  ;; macro to expand to code for generating the SNN, learn data and return a testing function
  (let* ((delays (ap-list :startat 200 :stopat 400 :step 1)) ; startat 4 stopat 30 step 0.1
	 (in-wts (create-sym-list dimensions "in-wt"))
	 (sig-list (create-sym-list dimensions "sig-"))
	 (learned-delay-list (create-sym-list dimensions "learned-delays-x"))
	 (w-syms (create-sym-list (* (length delays) dimensions) "wt-syms"))
	 (input-syms (create-sym-list dimensions "input-vals"))
	 (in-syms (create-sym-list dimensions "in-syms"))
	 (spike-syms (create-sym-list dimensions "spike-time-syms"))
	 (spike-s (gensym "final-spike-time"))
	 (learned-wt 0.6))	 
    (labels ((create-single-ds ()
	       ;; for each dimension, create the required neurons
	       (let ((count 1)(learned))
		 (mapcan (lambda (in-sym in-wt-sym spike-sym sig-sym)
			   ;; name each structure i.e. synapse and neuron according to counter
			   (let ((ni (create-sym 'ni count))(syn-emit (create-sym 'syn-emit count))
				 (n (create-sym 'n count))(syn-in (create-sym 'syn-in count))
				 (nn (create-sym 'nn count))(d (create-sym 'd count))
				 (wt-syms (if (not learned) (subseq w-syms 0 (length delays)))))
			     (incf count)
			     (if (not learned) (setq w-syms (subseq w-syms (length delays))))
			     ;; create and return the structure code
			     `((,ni (neuron syn-inf :w 1.52))
			       (,syn-emit (synapse ,ni ,@(if (not learned) `(:sig-spike ,sig-sym))))
			       (,n (neuron ,syn-emit :w 2 :s ,spike-sym :disp-on-spike nil))
			       (,syn-in (synapse ,n))
			       (,nn (neuron ,syn-in :in-s ,in-sym :w 1
					    ,@(if (not learned) `(:weight-sym ,in-wt-sym))))
			       (,d (delay-net3* ,(if learned (sv (nth (1- count) learned-delay-list)) delays)
						,syn-in nil 
						,@(if (not learned) `(:weightsyms ,wt-syms))
						:rest-args (:in-v 20
								  :w ,(if learned learned-wt 0.001)
								  :accurate-timing nil
								  :disp-on-spike nil
								  :single-spike t 
								  ,@(if (not learned) `(:store ,sig-sym))))))))
			 in-syms in-wts spike-syms sig-list))))
      ;; return code starts
      ;; create bindings for all the used symbols in the snn function
      `(with-syms (,@in-wts ,@sig-list ,@w-syms ,@spike-syms ,spike-s ,@in-syms)
	 (let (;; create bindings for the delay variables in the expansion
	       ,@(mapcar #'list `(,@learned-delay-list))
	       ;; length of learning data
	       (data-length (length ,learn-vals))
		 ;; no debug
		 (*MEMCALL-DISPLAY* nil))
	   (let (;; binding for the snn variable
		 (snn (build-snn ((ter (terminator))
				  (syn-out (synapse ter))
				  (n-out (neuron syn-out :s ,spike-s))
				  (syn-inf (synapse n-out))
				  ,@(create-single-ds))
				 :symbols (,spike-s ,@spike-syms)
				 :extra-run ,(mapcar (lambda (sig) `(set ,sig (cons t nil))) sig-list)
				 :display-progress nil :display-making nil :display-build nil)))
	     (let ,(let ((count 0))
			(mapcar (lambda (isym)
				  (prog1 `(,isym 
					   (sort 
					    (mapcan (lambda (A) (let ((x (nth ,count A))) (if (numberp x) (list x))))
						    ,learn-vals) #'>))
				    (incf count)))
				input-syms))
	       (dummy 1
		      ;; from each learning tuple, choose one from the top
		      (choose ((v (mapcar #'list ,@input-syms)))
			;; each nth input symbol have to be set to the nth element if the learning tuple
			,@(let ((count -1))
			       (mapcar (lambda (A) `(set-input ,A (nth ,(incf count) v))) in-syms))
			;; set all the input weights to 1
			;,@(mapcar (lambda (A) `(set ,A 1)) in-wts)
			;; run the snn
			(funcall snn 50 0.1)
			;; optionally print a message informing input tuple and output spike tyimes
			(format nil "~&Passed ~D (~D)" v (sv ,spike-s))
			;; if there is no output spike, have to learn the data
			(if (null (sv ,spike-s))
			    ;; learning part
			    (progn 
			      ;; which ever spike syms havent spiked, set those weights to a high value
			      (or 
			       ,@(mapcar (lambda (S I) `(cond ((null (sv ,S)) (set ,I 10) t))) spike-syms in-wts)
			       (break "none is set to high weight"))
			      ;; run the snn
			      (funcall snn 50 0.1)
			      ;; for each spike symbol
			      (or 
			       ,@(mapcar (lambda (I S D)
					   ;; if that weight symbol was set to high
					   `(cond ((= (sv ,I) 10)
						   ;; no idea what these two lines do although i wrote them
						   (pprint (sv ,S))
						   ;(break)
						   (set (cddr (sv ,S)) ,learned-wt)
						   (push (list (read-from-string (cadr (sv ,S)))) ,D) t)))
					 in-wts sig-list learned-delay-list)
			       (break "no learning occured !")))
			    ;; no need to learn
			    (format t "\spike @~D\\" (list ,@(mapcar (lambda (s) `(cadr (sv ,s))) sig-list))))
			(incf *LEARN-POS*)))))
	   (pprint (list ,@(mapcar (lambda  (L) `,L) learned-delay-list)))
	   (setq ,@(mapcan (lambda (L) `(,L (mapcar #'car ,L))) learned-delay-list))
	   (setq ,@(mapcan (lambda (L) `(,L (append '(list) ,L))) learned-delay-list))
;	   ,@(mapcar #'(lambda (L) `(pprint ,L)) learned-delay-list)
	   (eval `(let ((spike-s (gensym "_spike-s"))
			(in-syms (create-sym-list ,,dimensions "_in-syms"))
			(spike-syms (create-sym-list ,,dimensions "_spike-time-syms")))
		    (labels ((create-single-d2 ()
			     (let ((count 1))
			       (mapcan (lambda (in-sym spike-sym)
					 (let ((ni (create-sym 'ni count))(syn-emit (create-sym 'syn-emit count))
					       (n (create-sym 'n count))(syn-in (create-sym 'syn-in count))
					       (nn (create-sym 'nn count))(d (create-sym 'd count))
					       (delays (nth (1- count) (list ,,@learned-delay-list))))
					   (incf count)
					   `((,ni (neuron syn-inf :w 1.52))
					     (,syn-emit (synapse ,ni))
					     (,n (neuron ,syn-emit :w 2 :s ,spike-sym))
					     (,syn-in (synapse ,n))
					     (,nn (neuron ,syn-in :in-s ,in-sym :w 1))
					     (,d (delay-net3* ,delays
							      ,syn-in nil
							      :rest-args (:in-v 20
										:w ,,,learned-wt
										:accurate-timing nil
										:single-spike t))))))
				       in-syms spike-syms))))
		      (eval `(let ((*MEMCALL-DISPLAY* nil))
			       (with-syms (,@spike-syms ,spike-s ,@in-syms)
				 (let ((snn (build-snn ((ter (terminator))
							(syn-out (synapse ter))
							(n-out (neuron syn-out :s ,spike-s))
							(syn-inf (synapse n-out))
							,@(create-single-d2))
						       :display-build nil
						       :display-progress nil
						       :symbols (,@spike-syms ,spike-s))))
				   (list :run (lambda (N) (funcall snn N 0.1))
					 :sym-in (lambda () (list ,@in-syms))
					 :sym-out (lambda () ,spike-s)
					 :sym-spike-in (lambda () (list ,@spike-syms))
					 :set-excite (lambda (in-data)
						       ,@(let ((form))
							      (dotimes (cnt (length in-syms) form)
								(setf form 
								      (append form
									      `((set-input ,(nth cnt in-syms)
											   (nth ,cnt in-data))))))))
					 :train-data (lambda () (list ,,,data-marker ,,data-length))
					 :check (lambda () (and (sv ,spike-s) 
								,@(mapcar (lambda (s) `(sv ,s)) spike-syms)))))))))
		    )))))))

(defmacro do-snn (build-form spike-potn-lpairs &optional time-plot-repeat addl-syms post-create-forms &body collectable-result)
  (destructuring-bind (ssyms psyms) spike-potn-lpairs
    (destructuring-bind (time plot repeat) time-plot-repeat
      (let ((symbols (append psyms ssyms)))
	`(with-syms ,(append symbols addl-syms)
	   (octave/run t t)
	   (prog1
	       (loop repeat ,repeat
		  with snn = (build-snn ,build-form :symbols ,symbols :extra-run ,post-create-forms)
		  do (funcall snn ,time)
		  collect (progn ,@collectable-result))
	     ,(if plot `(progn (octave/sym-set ,@symbols) (syms-plot ,psyms ,ssyms)))
	     ))))))

(defmacro model-function-step (model)
  `(switch ,model
	   ((:izhikevich
	     (setf vn (+ potn Iv (* dt (+ Ii (* 0.04 potn potn) (* 5 potn) 140 (- recv) )))
		   un (+ recv (* dt A (- (* B potn) recv)))))
	    (:ReSuMe
	     (let* ((Vm potn)(Vr (- (mili 60)))(Isyn Ii)(Ins 0)(Iin (nano 0.1))
		    (t1 (+ Ii (nano 0.1) 0)) (t2 (* t1 R)))
;	       (break)
	       (setf vn (+ Vm (* dt (+ Vr (- Vm) (* R (+ Isyn Ins Iin))))))
	       (format nil "~&~D ~D ~D" t1 t2 vn)))
;		       (setf vn (+ potn (/ (+ (- (mili 60) potn t2) (* R C))))))) ; Change 0 to gaussian noise value
	    (:LIF
	     (setf vn (+ potn (* dt (/ (+ (- potn) (* R Ii)) (* R C)))))))
	   :default (break (format nil "Unknown model: ~D" ,model))))

(defmacro model-function-reset (model)
  `(switch ,model
	   ((:izhikevich   (setf vn (float c) un (+ un d)) t)
	    (:ReSuMe (setf vn (mili -65) disablet (+ tn 5)))
	    (:LIF    (setf vn 0)))))

(defun model-params (model)
  (switch model
	  ((:izhikevich `(:vn -70.0 :un -14.0))
	   (:ReSuMe `(:vn ,(mili (+ -60 (1- (random 3))))))
	   (:LIF `(:vn 0)))))

(defun model-threshold (model)
  (switch model
	  ((:izhikevich 30) (:ReSuMe (mili -55)) (:LIF 1))))

(defmacro with-params (&body code)
  `(let ((A)(B)(C)(D)(R))
     (declare (type float A B C D R))
     (switch model 
	     ((:LIF (setf C 1 R 10))
	      (:ReSuMe (setf C (nano 1) R (mega 10)))
	      (:izhikevich (set-izhi-params)))
	     :default (assert t (A B D C R)
			      (format nil "Unknown model: ~D for ~D.~&Please set the required model parameters." model myname)))
     A B C D R
     ,@code))

(defmacro set-izhi-params ()
  (let ((types '((:regular-spiking (0.02 0.2 -65 6))
		 (:fast-spiking (0.1 0.2 -65 2))
		 (:phasic-spiking (0.02 0.25 -65 6))
		 (:tonic-bursting (0.02 0.2 -50 2))
		 (:phasic-bursting (0.02 0.25 -55 0.05))
		 (:mixed-mode (0.02 0.2 -55 4))
		 (:spike-frequency-adaptation (0.01 0.2 -65 8))
		 (:class-1 (0.02 -0.1 -55 6))
		 (:class-2 (0.2 0.26 -65 0))
		 (:spike-latency (0.02 0.2 -65 6))
		 (:subthreshold-oscillations (0.05 0.26 -60 0))
		 (:resonator (0.1 0.26 -60 -1))
		 (:integrator (0.02 -0.1 -55 6))
		 (:rebound-spike (0.03 0.25 -60 4))
		 (:rebound-burst (0.03 0.25 -52 0))
		 (:threshold-variability (0.03 0.25 -60 4))
		 (:bistability (1 1.5 -60 0))
		 (:dap (1 0.02 -60 -21))
		 (:accomodation (0.02 1 -55 4))
		 (:inhibition-induced-spiking (-0.02 -1 -60 8))
		 (:inhibition-induced-bursting (-0.026 -1 -45 0)))))
    `(block boo
       (loop 
	  for typed in ',types
	  when (eq (car typed) type)
	  do (return-from boo 
	       (let ((xx (cadr typed)))
		 (setf A (first xx) B (second xx) C (third xx) D (fourth xx)))))
       (if type (format t "~&Unknown neuron type: ~D, assuming regular~%" type)
	   (setf A 0.02 B 0.2 C -65 D 6)))))

(defmacro list-string-num (list)
  `(mapcar #'read-from-string ,list))

(defmacro dummy (&body code)
  `(progn ,@code))

(defun gensym* (&rest strs)
  (gensym (reduce (lambda (s1 s2) (concatenate 'string s1 " " s2)) strs)))
