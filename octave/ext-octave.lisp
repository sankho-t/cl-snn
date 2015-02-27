;; This code starts up an octave process in background and passess commands to and from
;; its terminal for all jobs. 
;; Best method should be: Statically use liboctave libraries to implicitly convert the data from Lisp to .mat
;; For now, data can only pass to octave not reverse

(defpackage cl-octave
  (:use common-lisp ccl)
  (:export octave/run octave/exec octave/term octave/figure octave/set octave/sym-set octave/quit octave/status octave/save-session octave/sym-set-list
	   octave/exec-debug
	   octave/load-session  get-dims-from-list list-to-array list-to-array3))

(in-package :cl-octave)

(defvar *PROCESS-OCTAVE* nil)
(defvar *PROCESS-OCTAVE-LISTENER*)
(defvar *STREAM-TO-OCTAVE*)
(defvar *STREAM-FROM-OCTAVE*)
(defvar *STREAM-LOCK* (ccl:make-read-write-lock))
(defvar *OCTAVE-PATH* nil)
(defvar *OCTAVE-READY* nil)
(defvar *PLOT-INITD* nil)
(defvar *REQUIRE-I-FLAG* nil)
(defvar *LEGACY*)
(defvar *OCTAVE-FOUND*)

(defun set-octave-path ()
  (if (find :linux *features*) (setf *OCTAVE-PATH* "octave")
      (or (and (setf *OCTAVE-PATH*
		     (or (probe-file "D:\\Octave\\Octave-3.6.1\\bin\\octave.exe")
			 (probe-file "C:\\Octave\\Octave-3.6.1\\bin\\octave.exe")
			 (probe-file "C:\\Octave-3.6.1\\bin\\octave.exe")
			 (probe-file "C:\\Software\\Octave-3.6.4\\bin\\octave.exe")
			 (probe-file "octave.exe")))
	       (progn (format t "octave found at ~D" *OCTAVE-PATH*) t))
	  (progn
	    (format t "Octave not found. Specify location ? (y/n):")
	    (if (eq (read) 'y)
		(assert (and *OCTAVE-PATH* (probe-file *OCTAVE-PATH*)) (*OCTAVE-PATH*)  "Path to octave.exe :"))))))

(eval-when (:load-toplevel)
  (if (not *OCTAVE-PATH*) (set-octave-path))
  (if *OCTAVE-PATH*
      (print "octave package loaded !")))

(defun validate-version (verstring)
  (let ((start -1))
    (dolist (n '(3 6 0) '(nil t))
      (multiple-value-bind (n2 pos) (parse-integer verstring :start (1+ start) :junk-allowed t)
	(if (null n2) (return '(t nil)))
	(setf start pos)
	(if (< n2 n) (return '(nil t)))))))

(defun octave/run (&optional no-auto-load no-warning)
  "Runs octave as a background process"
  (if *OCTAVE-PATH*
      (cond ((eq (octave/status) :running)
	     (format (not no-warning) "~&Octave process already running!") nil)
	    (t
	     (format t "~&Starting octave.")
	     (let ((process (ccl:run-program (namestring *OCTAVE-PATH*) `("-q" ,(if *REQUIRE-I-FLAG* "-i" ""))
					     :wait nil :input :stream :output :stream :sharing :lock
					     :status-hook 
					     (lambda (N) (format t "~1&Octave process status changed to ~A" 
								 (ccl:external-process-status N))))))
	       (cond ((eq (ccl:external-process-status process) :running)
		      (setf *PROCESS-OCTAVE* process
			    *STREAM-TO-OCTAVE* (ccl:external-process-input-stream process)
			    *STREAM-FROM-OCTAVE* (ccl:external-process-output-stream process)
			    *PROCESS-OCTAVE-LISTENER* 
			    (ccl:process-run-function 
			     "octave-listener" 
			     (lambda ()
			       (format t "~%Octave started (wait till READY).~&Output from octave terminal:" )
			       (loop
				  (sleep 2)
				  (loop
				     (ccl:with-read-lock (*STREAM-LOCK*) 
				       (cond ((listen *STREAM-FROM-OCTAVE*)
					      (let ((xx (read-line *STREAM-FROM-OCTAVE*)))
						(and (not *OCTAVE-READY*) (string= xx "READY")
						     (setf *OCTAVE-READY* t))
						(format t "~&>~A" xx)))
					     (t (return)))))))))
		      (octave/init-session no-auto-load)  t)
		     (t (format t "Octave run fail (status=~D)~%" (ccl:external-process-status process)) nil)))))
      (if (not no-warning)
	  (break "Octave wasn't found on your system!"))))

(defun octave/exec-reply (command)
  (ccl:process-suspend *PROCESS-OCTAVE-LISTENER*)
  (unwind-protect
       (ccl:with-write-lock (*STREAM-LOCK*)
	 (write-line command *STREAM-TO-OCTAVE*)
	 (finish-output *STREAM-TO-OCTAVE*)
	 (sleep 1)
	 (loop 
	    (if (listen *STREAM-FROM-OCTAVE*)
		(return (read-line *STREAM-FROM-OCTAVE*)))))
    (ccl:process-resume *PROCESS-OCTAVE-LISTENER*)))

(defparameter *exec-print* nil)

(defmacro octave/exec (&body strings)
  `(progn
     (if *exec-print* (print (concatenate 'string ,@strings)))
     (octave/exec1 (concatenate 'string ,@strings))))

(defmacro wait-till-ready ()
  `(progn 
     (setf *OCTAVE-READY* nil)
     (octave/exec "disp('READY')")
     (loop (if *OCTAVE-READY* (return) (sleep 1)))))

(defun octave/init-session (&optional no-load-prev)
  (setf *PLOT-INITD* nil)
  (and (not no-load-prev)
       (probe-file "cl-octave")
       (octave/exec "load cl-octave"))
  (octave/exec "who")
  (octave/exec "disp('READY')")
  (loop (if *OCTAVE-READY* (return)) (sleep 1))
  '(setf *LEGACY*
	(loop named get-version repeat 10
	   for (try-again version-ok)  = (validate-version (print (subseq (octave/exec-reply "version") 6)))
	   do (sleep 2)
	   if (not try-again)
	   do (progn 
		(octave/exec "graphics_toolkit('gnuplot')")
		(return-from get-version version-ok))
	   end
	   finally (break "Could not get octave version !")))
  (octave/exec "graphics_toolkit('gnuplot')")
  (setf *LEGACY* nil)
  (wait-till-ready)
  (setf *PLOT-INITD* t))

(defun octave/save-session (&optional (save-file "cl-octave") (format :oct))
  "Possible formats are :mat7, :mat6 and :oct"
  (octave/exec "save " (cond ((eq format :mat7) "-mat7-binary ")
			     ((eq format :mat6) "-mat-binary ")
			     (t ""))
	       save-file)
  (octave/exec "disp('Session saved in " save-file "')" ))

(defun octave/load-session (&optional (load-file "cl-octave"))
  (octave/exec "load " load-file)
  (octave/exec "disp('Session loaded from " load-file "')"))

(defun octave/execf (&rest strings)
  "Execute a single statement supplied as a single or mutiple strings"
  (octave/exec (reduce (lambda (a b) (concatenate 'string a b)) strings)))

(defun octave/exec1 (string)
  "Execute a single statement supplied as a single string"
  (ccl:with-write-lock (*STREAM-LOCK*)
    (write-line string *STREAM-TO-OCTAVE*)
    (finish-output *STREAM-TO-OCTAVE*)))

(defun octave/exec-debug (&rest comms)
  (let ((comm (reduce (lambda (a b) (concatenate 'string a b)) comms)))
    (format t "~&~D" comm)
    (octave/exec1 comm)))

(defun octave/term ()
  "Start an octave terminal"
  (let ((i 0))
    (format t "~&Type end to exit~%")
    (loop
       (let ((comm (progn (format t "octave:~D>" (incf i)) (read-line))))
	 (if (string= comm "end") (return))
	 (cond ((not (eq (octave/status) :running)) 
		(format t "~&Octave process stopped !")(setq i 0)(return)))
	 (octave/exec comm)))))

(defun octave/figure (&rest comm-strings)
  (let ((str-out (reduce (lambda (a b) (concatenate 'string a b)) comm-strings)))
    (octave/exec str-out)
    (octave/exec "figure")))

(defmacro octave/set (&rest strvar-vals)
  "Set variables with the corresponding values: (octave/set \"a\" '((1 2)(3 4)) \"b\" #(1 2 3))"
  (labels ((m (strvar-vals)
	     (if strvar-vals
		 (append `((octave/set-array ,(first strvar-vals) ,(second strvar-vals)))
			 (m (rest (rest strvar-vals)))))))
    `(progn ,@(m strvar-vals))))

(defmacro octave/sym-set (&body vars)
  "Set variables having names and values of supplied symbols: (octave/sym-set sym-1 sym-2 sym-3)"  
  `(progn  
     ,@(loop for v in vars collect
	    `(octave/set-array ,(check-oct-var-syntax (symbol-name v)) (symbol-value ,v)))))

(defun check-oct-var-syntax (name)
  (if (find #\- name) (break (format nil "Illegal octave variable name: ~D" name))
      name))
	  
(defun octave/sym-set-list (symlist)
  (mapcar (lambda (S) (octave/set (symbol-name S) (symbol-value S))) symlist))

(defgeneric octave/set-array (str-var val)
  (:documentation "used for setting array/list/numbers to octave array"))

(defmethod octave/set-array (str-var (val array))
  (cond ((> (length (array-dimensions val)) 2)
	 (array-to-mat-ndim str-var val))
	((= (length (array-dimensions val)) 2)
	 (octave/exec str-var "=" (array-to-mat-string val) ";"))
	((= (length (array-dimensions val)) 1)
	 (octave/exec str-var "=" (array-to-mat-string-1D val) ";"))
	(t (octave/exec str-var "= [];")))
  (octave/exec (format nil "disp('size(~D):');size(~D)" str-var str-var)))

(defmethod octave/set-array (str-var (val list))
  (cond ((= (length (get-dims-from-list val)) 1)
	 (octave/exec str-var "=" (format nil "[~{~A~#[~:;,~]~}]" val) ";"))
	(t (octave/set-array str-var (list-to-array val)))))

(defmethod octave/set-array (str-var (val number))
  (octave/exec str-var "=" (write-to-string val) ";"))

(defun octave/status ()
  (if (null *PROCESS-OCTAVE*)
      "Start octave first!"
      (ccl:external-process-status *PROCESS-OCTAVE*)))

(defun octave/quit ()
  (if (eq (octave/status) :running)
      (progn
	(octave/exec "quit")
	(setf *OCTAVE-READY* nil *PLOT-INITD* nil)
	(ccl:process-kill *PROCESS-OCTAVE-LISTENER*))))

(defun octave/kill ()
  (setf *OCTAVE-READY* nil *PLOT-INITD* nil)
  (and *PROCESS-OCTAVE*
       (ccl:process-kill *PROCESS-OCTAVE*))
  (and *PROCESS-OCTAVE-LISTENER*
       (ccl:process-kill *PROCESS-OCTAVE-LISTENER*)))

(defun array-to-mat-string-1D (arr)
  (format nil "[~{~A~#[~:;,~]~}]" (coerce arr 'list)))	    

(defun array-to-mat-string (arr)
  (format nil "[~a]"
	  (let ((str ""))
	    (do ((i 0 (+ i 1))) ((= i (array-dimension arr 0)) str)
	      (do ((j 0 (+ j 1))) ((= j (array-dimension arr 1))) 
		(setq str (concatenate 'string str " " (write-to-string (aref arr i j)))))
	      (setq str (concatenate 'string str " ;" ))))))

(defun list-inc (list max)
  (do ((i (1- (length list)) (1- i)))((< i 0) nil)
    (cond ((< (1+ (nth i list)) (nth i max))
	   (return (progn (setf (nth i list) (1+ (nth i list)))
			  list))))))

(defun array-to-mat-ndim (var arr)
  (labels ((sub-array-2d (arr dims)
	     (let* ((dim1 (second (reverse (array-dimensions arr))))
		    (dim2 (car (last (array-dimensions arr))))
		    (arr2 (make-array `(,dim1 ,dim2))))
	       (do ((i 0 (1+ i)))((= dim1 i) arr2)
		 (do ((j 0 (1+ j)))((= dim2 j))
		   (setf (aref arr2 i j) (apply #'aref arr `(,@dims ,i ,j)))))))
	   (w (dims)
	     (cond (dims
		    (octave/exec-bla var (format nil "(:,:,~{~A~#[~:;,~]~})" (mapcar #'1+ dims)) "="
				     (array-to-mat-string (sub-array-2d arr dims)) ";" )
		    (w (list-inc dims (subseq (array-dimensions arr) 0 
					      (- (length (array-dimensions arr)) 2))))))))
    (w (fill (subseq (array-dimensions arr) 2) 0)))
  )

(defun octave/exec-bla  (&rest abcd)
  (declare (ignorable abcd))
  (format t "~D" abcd))

(declaim (inline setcons))
(defun setcons (lst val)
  (setq lst (cons val lst)) val)

(defun arr-to-list (arr)
  (let ((prg)(genlist))
    (do ((i 0 (+ i 1))) ((= i (length (array-dimensions arr))) (reverse prg))
      (setq prg (cons 
		 `(let ((lst))
		    (do (( ,(setcons genlist (gensym)) 
			    (array-dimension ,arr ,i) 
			    ,(car genlist))) ((= ,(car genlist)) (reverse lst))
		      (setf lst (cons (list (aref ,arr ,@genlist)) lst))))
		 prg)))))

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

(defun list-to-array (list)
  "Converts a list to an n-dim array"
  (let ((array (make-array (get-dims-from-list list))))
    (labels ((set-array (prev-dims list)
	       (let ((new-dim 0))
		 (mapcar (lambda (N) 
			   (prog1
			       (if (listp N) (set-array (append prev-dims `(,new-dim)) N)
				   (setf (apply #'aref array (append prev-dims (list new-dim))) N))
			     (incf new-dim)))
			 list))))
      (set-array `() list)
      array)))

(defun list-to-array3 (list)
  (let* ((dims (get-dims-from-list list))
	 (array (make-array dims :initial-contents list)))
    array))

(defun get-dims-from-list (list &optional (dims nil))
  (cond ((not (atom list))
	 (setq dims (append dims `(,(length list))))
	 (funcall #'get-dims-from-list (first list) dims))
	(t dims)))

(defun save-to-file (array var fname &key overwrite)
  (with-open-file (file fname :direction :output :if-does-not-exist :create
			:if-exists (if overwrite :supersede :append))
    (let ((dims (array-dimensions array)))
      (format file "# name: ~D~%# type: ~D~%# rows: ~D~%# columns: ~D" var "matrix" (first dims) (second dims))
      (loop for i from 1 to (first dims) do
	   (format file "~%")
	   (loop for j from 1 to (second dims) do
		(format file " ~D" (aref array (1- i) (1- j)))))
      (format file "~%~%~%")
      )))

(defvar *temp-file* "ftemp")

(defgeneric save (str-var value)
  (:documentation "Transfer 2D data to octave through file mechanism"))

(defmethod save (str-var (value array))
  (save-to-file value str-var "ftemp"))

(defmethod save (str-var (value list))
  (let ((array (make-array (get-dims-from-list value) :initial-contents value)))
    (save-to-file array str-var "ftemp")))

(defmacro octave/sym-set-fast (&body vars)
  "Set variables having names and values of supplied symbols: (octave/sym-set sym-1 sym-2 sym-3)"  
  `(progn  
     (and (probe-file *temp-file*)
	  (delete-file *temp-file*))
     ,@(loop for v in vars collect
	    `(save ,(symbol-name v) (symbol-value ,v)))
     (octave/exec "load " *temp-file*)
     ,@(loop for v in vars collect
	    `(octave/exec "size(" ,(symbol-name v) ")"))))
