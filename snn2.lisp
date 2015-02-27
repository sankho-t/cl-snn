(in-package SNN)

(defparameter *debug-stream* nil)
(defparameter *reduced-step* 1)
(defconstant crossmargin 4)
(defconstant *epsp-threshold* 10) 

(defun spiker (postsynapse &key ((:name myname) "unnamed spiker")
			     spike-times ((:w strength) 1) w-fixed delay store s debug (delay-sym (gensym* myname "delay")) sig-spike
			     (weight-sym (gensym* myname "weight")) (spiked-sym (gensym* myname "spikesym")))
  (declare (type real strength delay))
  (declare (type symbol weight-sym delay-sym))
  (declare (type list spike-times))
  (let ((to-pop)(synind)(stimes)(hasrun)(spiked)(rtime))
    (declare (type boolean to-pop hasrun spiked))
    (macrolet ((reset () `(progn (setf stimes spike-times))))
      (if (and spike-times (atom spike-times)) (setf spike-times (list spike-times)))
      (reset)
      (set weight-sym strength) (set delay-sym (or delay 0))
      (list
       :run (lambda (tn &optional (Iv 0))
	      (if (eq tn 0) (format t "~&~D, delayvalue= ~D~%" myname (sv delay-sym)))
	      (if w-fixed (set weight-sym w-fixed))
	      (and spiked-sym (set spiked-sym nil))
	      (macrolet ((has-spiked (has)
			   `(prog1 '(:spiked ,has)
			      ,(if has `(and spiked-sym (set spiked-sym t)))
			      (setf hasrun t to-pop nil spiked ,has rtime tn)
			      (memcall (sv postsynapse) :spike-switch synind ,has tn))))
		;; check if spikes remain in list
		(if stimes
		    ;; calculate time of next spike
		    (let ((next-sp (+ (first stimes) (sv delay-sym))))
		      ;; check if it has to spike in past time
		      (if-then (> tn next-sp)
			       then
			       (->DEB " | Back-tracking to ~d from ~d" next-sp tn)
			       `(:sim-break t :simtime ,next-sp)
			       else
			       (->DEB " | Executing at ~D" tn)
			       (if (round-equal tn next-sp)
				   ;; spike in list for present time
				   (prog1 (has-spiked t) (setf to-pop t))
				   ;; spike not in list for present time
				   (if (> Iv *epsp-threshold*) 
				       ;; if input has exceeded threshold then spike else not
				       (has-spiked t) (has-spiked nil)))))
		    ;; no spikes remain in list
		    (if (> Iv *epsp-threshold*) (has-spiked t) (has-spiked nil)))))
       :propagate (lambda () (memcall (sv postsynapse) :spike-switch synind spiked rtime))
       :dry-run (lambda () (memcall (sv postsynapse) :dry-run-1 weight-sym delay-sym (lambda (index) (setf synind index)) myname))
       :cleanup-after-run (lambda ()
			    (cond ((and spiked hasrun)
				   (if sig-spike (format t "~&~D spiked @~D ms" myname rtime))
				   ;; if spiked from list then pop it
				   (if to-pop (pop stimes))
				   ;; backward compatibility
				   (and store (car (sv store)) (set store (cons t (cons myname weight-sym))))
				   ;; push to monitor symbol
				   (if s (push `(,rtime ,(sv weight-sym)) (sv s)))))
			    (setf hasrun nil spiked nil))
       :reset (lambda () (reset))
       :spike-sym (lambda () spiked-sym)
       :syn-index (lambda () synind)
       :whois (lambda () myname)))))

(defmacro perform-dynamics ()
  `(let* ((dt (- (float tn) t0))
	  (Ii (cond (is-sensory (+ (if isym (memcall isym :current (- tn delaytime)) 0)
				   (if in-v in-v 0) (if in-s (sv in-s) 0))) (t 0))))
     ;; according to model, find next state
     (model-function-step model)	       
     (setf runtn (float tn))
     (cond ((and (>= vn (model-threshold model)) (and simbreak (eq model :izhikevich)) (< (count weight-sym *reduced-step*) 5))
	    ;; potential exceeded too much, have to signal for backtracking simulation time
	    ;; this neuron is anticipating a spike
	    (cond (t (setf anticipating-spike t) (push weight-sym *reduced-step*)))
	    (pop delayedpulses)
	    ;; linearly interpolate spike threshold crossing time
	    `(:sim-break t :simtime ,(+ (* dt (/ (- (model-threshold model) potn) (- vn potn))) t0)))
	   (t
	    (let 
		((spiked (cond ((prog1 (>= vn (model-threshold model))
				  (format nil "~&~D >= ~D ? ~D." vn (model-threshold model) (>= vn (model-threshold model))))
				;; spiked, so if this decreased simulation time step, then increase now
				(cond (anticipating-spike 
				       (setf anticipating-spike nil)
				       (setf *reduced-step* (remove weight-sym *reduced-step*))))
				;; reset neuron potential and recovery variable
				(model-function-reset model)))))
	      ;; set spiked flag to be used during propagation
	      (setf aspike spiked)
	      ;; if self-propagating, then call the next synapse
	      (if (or sensory is-sensory)
		  (memcall (sv postsynapse) :spike-switch synind aspike tn))
	      `(:spiked ,spiked))))))

(defun neuron 
    (postsynapse &key ((:name myname) "unnamed neuron")
     (model :izhikevich)(type :regular-spiking) ((:in isym) nil is-sensory) sensory single-spike
     (weight-sym (gensym* myname "weight")) ((:w strength) 1) w-fixed (delay-sym (gensym* myname "delay")) ((:delay delaytime) 0) 
     (spiked-sym (gensym* myname "spikedsym")) ((:s outsym))((:p potnsym)) store in-v in-s disp-on-spike
     ((:accurate-timing simbreak) t)((:disable-for-zero-wt zwt-disable) t) debug)
  (declare (optimize (speed 3)))
  (let* ((potn (getf (model-params model) :vn)) (recv (getf (model-params model) :un))
	 (t0 0.0)(aspike)(anticipating-spike)(delayedpulses)(synind)(runtn 0.0)(ostate 0)(rtime)(disablet)
	 (vn potn)(un recv)(runcheck)(enabled))
    (declare (type real potn recv t0 runtn vn un))
    (declare (type boolean aspike runcheck enabled anticipating-spike))
    (with-params
      ;; initialization
      (set weight-sym strength)
      (set delay-sym delaytime)
      (if outsym (set outsym nil))
      (setf is-sensory (or is-sensory in-v in-s))
      ;; member functions
      (list
       :reset (lambda ()
		(setf potn (getf (model-params model) :vn) recv (getf (model-params model) :un)
		      t0 0.0 aspike nil anticipating-spike nil runtn 0.0
		      ostate 0 delayedpulses nil enabled t)
		(if delay-sym (setf delaytime (symbol-value delay-sym))))
       :run (lambda (tn &optional (Iv 0))
	      (declare (optimize (speed 3)))
	      (if w-fixed (set weight-sym w-fixed))
	      (if delay-sym (setf delaytime (sv delay-sym)))
	      (block run-block
		(setf aspike nil runcheck t)
		(if spiked-sym (set spiked-sym nil))
		(if (or (and zwt-disable (= (sv weight-sym) 0)) (and disablet (<= disablet tn)))
		    (progn (setf rtime tn) (break "run") (return-from run-block)))
		;; add delay to signal and store
		(if (> Iv 0)
		    (progn
		      (->DEB " | Spike added at time ~D " (+ delaytime tn))
		      (push-end2 `( ,(+ delaytime tn) ,Iv) delayedpulses)))
		;; if delayed pulses start at present time
		(cond ((not enabled) 
		       (setf rtime tn)
		       (memcall (sv postsynapse) :spike-switch synind nil tn)
		       (return-from run-block)))
		;;(if delayedpulses (->DEB "~% DPulses: [~D]  ~D ~D ~D " delayedpulses (ffirst delayedpulses) (< (ffirst delayedpulses) t0) t0))
		(prog1
		    (cond ((or anticipating-spike (and (ffirst delayedpulses) (round-equal tn (ffirst delayedpulses))))
			   ;; check and execute delayed pulse upto 3 decimal places
			   (->DEB " | Executing at ~D ~D ~D ~D" anticipating-spike tn (ffirst delayedpulses) Iv)
			   (setf rtime tn)
			   (let ((Iv (or (second (first delayedpulses)) 0))) (perform-dynamics)))
			  ((and (ffirst delayedpulses) (< (ffirst delayedpulses) tn))
			   (setf vn potn un recv)
			   ;; delayed signal is pending, do not execute at this time but at the delayed signal time
			   (->DEB " | Back-tracking to ~d from ~d" (ffirst delayedpulses) tn)
			   `(:sim-break t :simtime ,(ffirst delayedpulses)))
			  (t
			   (->DEB " | Idling at ~d" tn)
			   (setf rtime tn)
			   ;; no signal present now, hence no external input
			   (let ((Iv 0)) (perform-dynamics))))
		  ;; after run code here
		  (and aspike spiked-sym (set spiked-sym t)))))
       :cleanup-after-run (lambda ()
			    (and aspike outsym (push `(,runtn ,(sv weight-sym)) (symbol-value outsym)))
			    (and aspike disp-on-spike (format t "< Neuron ~D has spiked @~D !! >" myname rtime))
			    ;; simulation didnt break since last calculation, hence make state permanent
			    (setf potn vn recv un t0 runtn runcheck nil)
			    (if (and disablet (> runtn disablet)) (setf disablet nil))
			    (and aspike single-spike (setf enabled nil))
			    (and potnsym (push `(,runtn ,(if aspike (model-threshold model) vn)) (sv potnsym)))
			    (and aspike store (car (sv store))
				 (set store (cons t (cons myname weight-sym))))
			    (remove-earlier-times2 delayedpulses t0))
       :syn-index (lambda () synind)
       :spike-sym (lambda () spiked-sym)
       :propagate (lambda () (memcall (sv postsynapse) :spike-switch synind aspike rtime))
       :dry-run (lambda ()   (memcall (sv postsynapse) :dry-run-1 weight-sym delay-sym (lambda (index) (setq synind index)) myname))
       :whois (lambda () myname)
     ))))


(defun synapse (postneurons &key plasticity sig-spike disp-on-spike epsp weight-history p ((:name myname) "unnamed synapse") (w 1))
  (declare (optimize (speed 3)(safety 0)(debug 3)))
  (let ((trainsym)(delayedinc nil)(dry-in)(inspikes-tab)(delsyms-tab)(syms-tab)(strength 10)(on-spike-call)(aspike)(rtime)(train-ind)
	(spiked-times (make-hash-table))(traintime)(state :init)(outsym) (resume)(in-history (make-hash-table))(in-potential 0) (method))
    (macrolet ((neuron-spiked (ind)     `(aref inspikes-tab ,ind))
	       ;; return spiked boolean value according to index of neuron
	       (trainer-spiked ()       `(sv trainsym))
	       ;; return spiked boolean value for trainer neuron
	       (output-spiked ()        `(sv outsym))
	       ;; return spiked boolean value for output neuron
	       (any-neuron-spiked ()    `(loop for s across inspikes-tab thereis s))
	       ;; return generalized boolean for any neuron if spiked
	       (inc-delay (sym gaptime) `(set ,sym (+ (sv ,sym) (* 5 (+ 1 (exp (- ,gaptime)))))))
	       ;; given symbol and the delay, update the symbol value according to an exponentially falling kernel
	       (dec-delay (sym gaptime) `(set ,sym (- (sv ,sym) (* 5 (- (sigmoid ,gaptime) 0.5)))))
	       ;; same but for decrement
	       (with-input (var &body code) `(for-array inspikes-tab 0 ,var t ,@code))
	       ;; execute enclosed code iterating over all the input neurons
	       (when-spiked (var code &body else) `(if (aref inspikes-tab ,var) ,code (progn ,@else)))
	       ;; given the neuron index, if it has spiked then execute code else the else part
	       (spiked-time (index) `(getf (gethash ,index spiked-times) :spiketime))
	       ;; retrieve the latest spiking time from spiked-times table
	       (wsym-ind (var) `(aref syms-tab ,var))
	       ;; given index of the neuron, retrieve the  weight symbol
	       (dsym-ind (var) `(aref delsyms-tab ,var))
	       ;; given index of the neuron, retrieve the delay symbol
	       (perform-training (plasticity)
		 ;; training codes lie here, insert your own here as a seperate switch case
		 `(switch ,plasticity 
			  ((:ReSuMe
			   (with-input i (when-spiked i (memcall resume :add-in-spike i rtime)))
			   (with-input i
			     (and (or (output-spiked) (trainer-spiked))
				  (not (eq (wsym-ind i) trainsym))
				  (set (wsym-ind i) (memcall resume :get-lambda i (sv (wsym-ind i)) rtime
							    :output-spiked (output-spiked)
							    :trainer-spiked (trainer-spiked))))))
			   (:delay 
			    (switch state 
				    ((:init 
				      (if (trainer-spiked) (setf traintime rtime state :wait-for-any)))
				     (:wait-for-any 
				      (if-then (trainer-spiked) 
					       then (setf state :wait-for-trainee) (setf traintime rtime)
					      else (if 
						    (any-neuron-spiked) 
						    (setf state :trainee-spiked))))
				     (:trainee-spiked 
				      (map nil (lambda (sym) 
						 (inc-delay sym (- rtime traintime ))) delsyms-tab)
				      (setf state :wait-for-any))
				     (:wait-for-trainee 
				      (if (any-neuron-spiked) 
					  (map nil (lambda (sym) 
						     (dec-delay sym (- rtime traintime))) delsyms-tab))))))	
			   (:strength&delay 
			    ;; for single spikes only
			    (labels ((set-weights-and-delays-for-preneuron (tr-time &optional neuron-index)
				       '(print "inside now")
				       ;; given trainer spike time, evaluate and set new weight and delay for preneurons
				       (if (not neuron-index)
					   ;; if no specific neuron's specified then set for all neurons
					   (loop for index being the hash-keys in spiked-times
					      ;; loop over all the neurons
					      unless (eq train-ind index)
					      ;; only if we aren't looking at the trainer
					      do (let ((wsym (wsym-ind index))(dsym (dsym-ind index))(sptime (spiked-time index)))
						   ;; get the weight symbol, delay symbol and spiking time
						   (let ((diff-time (abs (- tr-time sptime))))
						     ;; find time gap b/w neuron and trainer
						     (if (> tr-time sptime)
							 ;; if trainer spiked later, increase delay of neuron
							 (progn 
							   (format t "~&tr: ~D, sp: ~D (delaysym ~D)" tr-time sptime dsym)
							   (inc-delay dsym diff-time))
							 ;; if trainer spiked before, decrease delay of neuron
							 (progn
							   (print "dd")
							   (dec-delay dsym diff-time))))
						   (set-weight wsym sptime rtime))))))
			      (let ((ct (random 100)))
				(cond ((trainer-spiked)
				       (print 'trainer-spiked)
				       (loop for i being the hash-keys in spiked-times
					  do (print (gethash i spiked-times)))))
				(cond ((trainer-spiked)
				       ;; set weights and delays for all neurons that have previously spiked
				       (print 'cond1)
				       (set-weights-and-delays-for-preneuron rtime)
				       (setf traintime rtime)))
				(if (and traintime (any-neuron-spiked))
				    ;; trainer has already spiked and some neuron has spiked now
				    (progn
				      (print 'cond2)
				      (with-input i (when-spiked i (format t "~&i= ~D" i)))
				      (with-input i (when-spiked i (set-weights-and-delays-for-preneuron traintime i))))
				    (if (any-neuron-spiked)
					(progn
					  (print 'cond3)
					  ;; trainer hasn't spiked till now yet other neurons have
					  (with-input i (setf (spiked-time i) rtime))))))))
			   (:strength2
			    (if-then (trainer-spiked)
				     then 
				     (setf traintime rtime)
				     ;; for all the previous spikers, get their index and spiked time
				     (loop
					for index being the hash-keys in spiked-times
					do (let ((sym (wsym-ind index))
						 (sptime (spiked-time index)))
					     ;; set the respective weights according to trainer (now) and self spike time
					     ;; if none other than trainer spiked, do nothing
					     (set-weight sym sptime rtime)))
				     ;; range the weights and set them
				     (flush-weights rtime)
				     (clrhash spiked-times)
				     else 
				     ;; store spiking times and wait till next trainer spike
				     (with-input i (when-spiked i (setf (spiked-time i) rtime)))))
			   (:strength
			    (cond ((trainer-spiked)
				   ;; check if any neuron has spiked
				   (setf-nil delayedinc)
				   (if (any-neuron-spiked)
				       ;; for each neuron (not trainer), if spiked then increment weight else decrement
				       (with-input i (if '(not (= i trainind))   ; <---- problem
							 (when-spiked i (sym-weight-increment :sym (wsym-ind i))
								      (sym-weight-decrement :sym (wsym-ind i)))))
				       ;; if none spiked, switch on delayed increment
				       (setf-t delayedinc)))
				  ;; trainer hasn't spiked but delayed increment is on and others have spiked
				  ((and delayedinc (any-neuron-spiked))
				   ;; increment twice whosoever have spiked
				   (setf-nil delayedinc)
				   (with-input i (when-spiked i (sym-weight-increment :sym (wsym-ind i) :multiply 2))))
					;; decrement others ??
				  ((any-neuron-spiked)
				   ;; if neuron spiked without trainer and delayedinc, then decrement weight
				   (with-input i (when-spiked i (sym-weight-decrement :sym (wsym-ind i))))))))
			  :default (assert (not plasticity) (plasticity)
					   (format nil "Unknown plasticity model ~D specified for ~D" plasticity myname))))
	       (with-net-input (var time &body code)
		 ;; run code after setting var as the total input potential taking care of the epsp
		 `(let ((,var
			 (let ((net-input 0))
			   (if (not epsp)
			       ;; if epsp function hasn't been set then just sum up the current potentials
			       (with-input i
				 ;; for all the inputs
				 (cond ((aref inspikes-tab i)
					;; if there is spike corresponding to input i, then sum up the weighted spikes
					(incf net-input (* strength (symbol-value (aref syms-tab i)))))))
			       ;; if epsp's been set
			       (prog1 (setf net-input
					    (loop for index being the hash-keys in in-history
					       ;; for all the history elements
					       for w = (getf (gethash index in-history) :weight)
					       for tn = (getf (gethash index in-history) :time)
					       summing (funcall epsp w (- ,time tn))))
				      (if (> net-input *epsp-threshold*) (clrhash in-history))))
			   (* net-input w))))
		    ,@code)))
      ;; initialization
      myname
      (assert (or (null epsp) (functionp epsp)) (epsp) 
	      (format nil "In synapse ~D, :epsp must either be a function or null" myname))
      ;; members
      (list 
       :run (lambda (stime)
	      (setf method 'run)
	      (with-net-input synpot stime
		(setf in-potential synpot)
		(let ((i 0)(max-i (length postneurons)))
		  (setq aspike (> synpot 0))		
		  (loop
		     ;; run each postneurons and examine return value
		     (let ((nret (memcall (nth i postneurons) :run stime synpot)))
		       (if (getf nret :sim-break)
			   ;; neuron has time break, exit with predicted next sim-time as output
			   (return `(:sim-break t :simtime ,(getf nret :simtime)))))
		     (cond ((= (incf i) max-i)
			    ;; all postneurons run, now propagate
			    (setf rtime stime)
			    (and aspike (not (null on-spike-call)) (not (cdr on-spike-call))
				 (funcall (car on-spike-call)))
			    (mapcar (lambda (PSN) (progn (memcall PSN :propagate))) postneurons)
			    (return `(:sim-break nil))))))))
       :cleanup-after-run (lambda ()
			    (setf method 'cleanup)
			    ;; if display flag ON, then display
			    (and aspike disp-on-spike (format disp-on-spike "~D spiked @<value>" myname))
			    ;; if signal symbol supplied
			    (and aspike sig-spike (car (sv sig-spike))
				 (progn
				   ;(break (format nil "~D" rtime))
				   (format nil "!! ~D ~D !!" rtime (sv sig-spike))
				   (set sig-spike (cons nil (cdr (sv sig-spike))))))
			    ;; if potnmonitor is set store current timestamped potential
			    (if p (push `(,rtime ,in-potential) (sv p)))
			    ;; call cleanup of postsynaptic neurons
			    (and aspike on-spike-call (cdr on-spike-call) (funcall (car on-spike-call)))
			    (mapcar (lambda (PSN) (memcall PSN :cleanup-after-run)) postneurons)
			    ;; if weight history is needed
			    (if weight-history (push (list (map 'list (lambda (s) (sv s)) delsyms-tab)) (sv weight-history)))
			    ;; check required parameters for plasticity models
			    (if (and (eq plasticity :resume) (or (null trainsym) (null outsym)))
				(break (format nil "Trainer and/or output syms not defined for ~D" myname)))
			    (perform-training plasticity))
       :reset (lambda () (clrhash in-history) (clrhash spiked-times) (if sig-spike (set sig-spike (cons t nil))))
       :set-trainer-sym (lambda (tr) (setf trainsym tr))
       :set-trainer-sym-ind (lambda (tr index) (setf trainsym tr train-ind index))
       :set-output-sym (lambda (tr) (setf outsym tr))
       :set-spike-fn (lambda (f &optional (oncleanup t)) (setq on-spike-call (cons f oncleanup)))
       :dry-run-1 (lambda (ws ds fn name) (push `(,ws ,ds ,fn ,name) dry-in))
       :dry-run-2 (lambda () 
		    (format nil " (~D) -> ~D" (mapcar 'fourth dry-in) myname)
		    (setf inspikes-tab (make-array `(,(length dry-in)) :element-type 'boolean :initial-element NIL)
			  ;; inspikes-tab is a boolean vector indicating if nth neuron has spiked
			  syms-tab (make-array `(,(length dry-in)) :element-type 'symbol)
			  ;; syms-tab is an array of weight symbols
			  delsyms-tab (make-array `(,(length dry-in)) :element-type 'symbol)
			  ;; delsyms-tab is an array of delay symbols
			  resume (make-resume-store (make-array (length dry-in) :initial-element (* 20 (expt 10 -11)))))
		    (let ((index -1)(net-in-wt 0))
		      (dolist (i dry-in t)
			;; store weight symbols and delay symbols in apropriate arrays
			(setf (aref syms-tab (incf index)) (first i)
			      (aref delsyms-tab index) (second i))
			;; add up all the incoming weights
			(incf net-in-wt (sv (first i)))
			;; call back neuron to set its index
			(funcall (third i) index))
		      (incoming-weights= net-in-wt)))
       :spike-switch (lambda (index state time)
		       (setf (aref inspikes-tab index) state)
		       (and epsp state
			    (progn
			      (setf (getf (gethash index in-history) :time) time)
			      (setf (getf (gethash index in-history) :weight) (sv (aref syms-tab index))))))
       :whois (lambda () myname)
       ))))

(let ((inc-dec-step 0.2)(max-weight 2)(min-weight 0.2))
  (defun sym-weight-increment (&rest params)
    (set (getf params :sym) (let ((val (+ (symbol-value (getf params :sym))
					  (* inc-dec-step (if (null (getf params :multiply)) 1
							      (getf params :multiply))))))
			      (if (> val max-weight) 
				  (symbol-value (getf params :sym))
				  val))))
  (defun sym-weight-decrement (&rest params)
    (set (getf params :sym) (let ((val (- (symbol-value (getf params :sym)) 
					  (* inc-dec-step (if (null (getf params :multiply)) 1
							      (getf params :multiply))))))
			      (if (< val min-weight) 
				  (symbol-value (getf params :sym))
				  val)))))

(let ((net-max 0)(wts-to-be-set)(A 2)(tau 10)(wt-history))
;; routines for scaling input presynaptic weights 
  (defun incoming-weights= (in-weights)
    ;; set net input weight of the synapse
    (setf wt-history nil net-max in-weights))
  (defun set-weight (weight-sym sp-time tr-time)
    ;; given individual spiking time and trainer spike time, calculate resultant weight
    (let ((wt (+ (sv weight-sym) (* A (exp (- (/ (- sp-time tr-time) tau)))))))
      ;; store the resultant weight temporarily
      (push `(,weight-sym ,wt) wts-to-be-set)))
  (defun flush-weights (rtime)
    ;; set the weights after normalizing by summing up all the weights
    (let ((net-wt (reduce #'+ (mapcar #'second wts-to-be-set)))(wt-hist-push))
      (loop 
	 ;; for all the symbol and weight pairs to be set
	 for sym-wt-pairs in wts-to-be-set
	 do (let* ((sym (first sym-wt-pairs))
		   ;; normalize the weight value by dividing with the net
		   (wt (* (second sym-wt-pairs) (/ net-max net-wt))))
	      ;; store the weight permanently to generate weight history
	      (push wt wt-hist-push)
	      ;; display 
	      (format t " setting ~D to ~D " sym wt)
	      ;; finally set the weight
	      (set sym wt)))
      ;; append the weight list to obtain history
      (push (append wt-hist-push (list rtime)) wt-history))
    ;; weights have been set, hence nullify the store
    (setf wts-to-be-set nil))
  (defun oct-set-history ()
    (octave/run t t)
    (octave/set "wthis" '())
    (octave/set "wthis" wt-history)))
	 
(defun terminator (&key ((:p pmon)))
  (declare (optimize (speed 3)(safety 0)(debug 1)))
  (let ((outstore))
    (if pmon
	(list
	 :reset (lambda () (set pmon nil))
	 :run (lambda (&rest params) (setq outstore params) `())
	 :propagate (lambda ())
	 :cleanup-after-run (lambda () (push outstore (symbol-value pmon)))
	 :whois (lambda () "terminator"))
	(list 
	 :reset (lambda ()) :whois (lambda () "terminator")
	 :run (lambda (&rest params) params nil) :propagate (lambda ()) :cleanup-after-run (lambda ())))))

(defun make-resume-store (a_di &optional (ad 1))
  (let ((store (make-hash-table)))
    (list 
     :add-in-spike
     (lambda (index time)
       (push time (getf (gethash index store) :sptimes)))
     :get-lambda 
     (lambda (index weight this-time &key output-spiked trainer-spiked)
       (let ((integral ad)(int-val 0))
	 (loop named summer
	    for time in (getf (gethash index store) :sptimes) do 
	      (if (> time this-time) (return-from summer))
	      (setf int-val (* (aref a_di index) (exp (- time this-time))))
	      (incf integral int-val)
	      (if (< int-val 0.1) (return-from summer)))
	 (if output-spiked (- weight integral)
	     (if trainer-spiked (+ weight integral)
		 weight)))))))

(defun epsp-1 ()
  (lambda (weight tgap)
    (* weight 2 (/ (exp (* -0.05 tgap)) (1+ (exp (* -0.05 tgap)))))))

(defun sigmoid (x) (/ 1 (+ 1 (exp (- x)))))
