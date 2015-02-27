;; tryout stuff here

(in-package :snn)

(defun try1 (val)
  (octave/run t t)
  (with-syms (s1 s2fast s2)
    (let ((snn (build-snn ((n1 (neuron syn1 :s s1 :w 1.5 :in-v val))
                           (syn1 (synapse (list n2 n2f)))
                           (n2f (neuron syn2 :s s2fast :type :fast-spiking))
			   (n2 (neuron syn2 :s s2))
                           (syn2 (synapse ter))
                           (ter (terminator)))
                          :symbols (s1 s2 s2fast))))
      (funcall snn 100)
      (if (> (length (sv s2)) 0) (format t "spiked ! :("))
      (octave/sym-set s1)
      (syms-plot (s1))
      (list (length (sv s1)) (length (sv s2fast)) (length (sv s2)))
      )))
  
(defun try12 (val &optional (time 10))
  {
  [ n1 -> syn1 -> n2 n2f -> syn2 ]
  @n1 #< :in-v val :w 0.5 #>
  @n2f #< :type :fast-spiking #>
  ^s n1 
  }do (progn 
	(snn-run time)
	(plot-all)
	(monitor-value n1 s)))

(defun try-loop ()
  (octave/run t t)
  (with-syms (p1 p2)
    (let ((snn (build-snn ((n1 (neuron syn :inputfn (current :value 10) :potnmonitor p1))
			   (syn (synapse (list n2 n3)))
			   (n2 (neuron syn))
			   (n3 (synapse ter))
			   (ter (terminator)))
			  :symbols (p1 p2))))
      (funcall snn 100)
      (octave/sym-set p1 p2)
      (syms-plot (p1 p2)))))

(defun try-res (time)
  {
  [ sp1 spt -> syn1 -> n2 -> syn2 ]
  @sp1 #< :spike-times '(2 6 10 12) :w 2 #>
  @n2 #< :model :resume #>
  @spt #< :spike-times '(3 8 10 11) :w-fixed 0.1 #>
  @syn1 #< :epsp (epsp-1) :plasticity :resume #>
  (setn syn1 :output n2)
  (setn syn1 :trainer spt)
  ^p syn1
  ^s sp1 spt n2
  }do (progn
	(snn-run time)
	(plot-all)))

(defun try-learn2 ()
  {
  [ sp1 spt -> syn1 ]
  @sp1 #< :spike-times '(2 5 8) :w-fixed 0.1 #>
  @spt #< :spike-times '(3 7) :w 1 #>
  @syn1 #< :epsp (epsp-1) :plasticity :strength&delay #>
  (setn syn1 :trainer spt)
  ^p syn1
  ^s sp1 spt
  }do (progn
	(snn-run 10)
	(plot-all)
	(snn-run 10)
	(plot-all)))

(defun try-nolearn2 ()
  {
  [ sp1 spt -> syn1 ]
  @sp1 #< :spike-times '(2 5 8) :w-fixed 0.1 #>
  @spt #< :spike-times '(3 7) :w 1 #>
  @syn1 #< :epsp (epsp-1) #>
  ^p syn1
  ^s sp1 spt
  }do (progn
	(snn-run 10)
	(plot-all)))

(defun try-izhi (value)
  (let ((*debug-stream* t)(ins (gensym)))
    (set ins value)
    {
    [ nb -> syn1 -> n1 n2 -> syn2 ]
    @n1 #< :delay 10 #>
    @n2 #< :delay 0 #>
    @nb #< :in-s ins :w 4 #>
    ^p nb n1 n2
    ^s n1 n2
    }do (progn
	  (snn-run 100)
	  (plot-all)
	  (monitor-values s n1 n2))))

(defun try-izhi-exp ()
  (with-syms (n1s n2s nbs)
    (let ((snn (build-snn ((ter (terminator))
			   (syn2 (synapse ter))
			   (n1 (neuron syn2 :delay 10 :s n1s))
			   (n2 (neuron syn2 :s n2s))
			   (syn1 (synapse (list n1 n2)))
			   (nb (neuron syn1 :in-v 10 :w 4 :s nbs)))
			  :symbols (n1s n2s nbs))))
      (funcall snn 100)
      (values (sv n1s) (sv n2s) (sv nbs)))))

(defun random-spikes (&key num max min)
  (if (>= num (- max min)) (break (format nil "Number of spikes ~D >= (Maximum - Minimum) ~D !!" num (- max min))))
  (loop with ret = nil
     until (= (length ret) num)
     do (setf ret (remove-duplicates (sort (push (+ (random (- max min)) min) ret) #'<)))
     finally (return ret)))

(defun try-resletter (&optional (times 5) (duration 250))
  (let ((xx 0))
    {
    [spt sp1 -> syn -> spout -> syn2]
    @spt #< :spike-times 30 :w-fixed 0.1 #>
    @sp1 #< :spike-times (incf xx 20) :w (* 2.5 (expt 10 -10)) #>
    @syn #< :plasticity :ReSuMe :epsp (epsp-1) #>
    (setn syn :trainer spt)
    (setn syn :output spout)
    ^s spt spout
    }do (progn 
	  (loop repeat times
	     do (progn 
		  (snn-run duration)
		  (format t "~&out: ~D~%" (monitor-value spout s)))))))

(defun try-sig ()
  (with-syms (signal1)
    {
    [sp1 nn1 -> syn -> n2 -> syn2]
    @sp1 #< :spike-times 12 :store signal1 #>
    @nn1 #< :in-v 5 :store signal1 :disp-on-spike t #>
    @syn #< :w 1 #>
    @syn2 #< :sig-spike signal1 #>
    ^s nn1 sp1 n2
    }do (progn
	  (set signal1 `(t))
	  (snn-run 30)
	  (format t "~&Signal 1 value= ~D" (sv signal1))
	  (monitor-values s sp1 nn1 n2))))

(defun test()
  {
  [n1 n2 -> syn1 -> #n-3..10 -> syn]
  @n-1..6 #< :in-v 5 #>
  ^s n-1..5
  ^p n3
  }do (progn
	(snn-run 30)
	(plot n3 n4)))

(defun test (val)
  {
  [n1 -> syn]
  @n1 #< :model :ReSuMe :in-v val#>
  ^p n1
  ^s n1
  }do (progn
	(snn-run 50)
	;(plot n1)
	(monitor-values s n1)))

(defun test ()
  {
  [sp1 sp2 spT -> syn]
  @sp1 #< :spike-times 5 #>
  @sp2 #< :spike-times 11 #>
  @spT #< :spike-times 10 #>
  @syn #< :plasticity :strength&delay #>
  (setn syn :trainer spT)
  }do (progn
	(snn-run 15)))

(defun str&delayTest ()
  {
  [#sp-1..10 spT -> syn -> nn -> syn2]
  @spT #< :spike-times 10 #>
  @sp-1..10 #< :w 1 :spike-times #v5..15 #>
  @syn #< :plasticity :strength&delay #>
  ^s sp-1..10 spT nn
  (setn syn :trainer spT)
  (setn syn :output nn)
  }do (progn
	(loop for i upto 5
	   do (snn-run 40)
	   do (plot-all)
	   do (break)
	   collect (monitor-value nn s))))

(defun str&delayTest-aux ()
  {
  [sp1 spT -> syn -> nn -> syn2]
  @spT #< :spike-times 10 :sig-spike t #>
  @sp1 #< :spike-times 8 :sig-spike t #>
  @syn #< :plasticity :strength&delay #>
  ^s sp1 nn
  (setn syn :trainer spT)
  (setn syn :output nn)
  }do (progn
	(loop for i upto 5
	   do (snn-run 40)
	   collect (monitor-values s sp1))))

