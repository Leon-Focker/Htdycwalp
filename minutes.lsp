;; * minutes

(in-package :ly)

;; ** classes

;;; one minute of the entire piece with mainly structural information
(defclass minute ()
  ((id :accessor id :initarg :id :initform nil)
   ;; start time of the minute in seconds
   (start-time :accessor start-time :type number :initarg :start-time
	       :initform 0)
   ;; list of tape-minutes and instrument-minutes
   (layers :accessor layers :type list :initarg :layers
	       :initform '())))

;;; I already named an entire package layers and now this...?
(defclass minute-layer ()
  ((id :accessor id :initarg :id :initform nil)
   ;; start time of the minute in seconds
   (start-time :accessor start-time :type number :initarg :start-time
	       :initform 0)
   (number :accessor number :type integer :initarg :number :initform 0)
   ;; ratios by which each minute will be divided
   ;; the duration for each division is = (* (/ 60 sum-of-ratios) ratio)
   (division-ratios :accessor div-ratios :initarg :div-ratios :initform '(1))
   ;; every division of the minute will have a state, which is a number
   ;; depending on the type of layer this number can be something like static,
   ;; isorhythmic etc.
   (states :accessor states :initarg :states :type list :initform '(0))
   ;; every division of the minute will have a dynamic, which is a number
   ;; depending on the type of layer this number can be something like forte,
   ;; crescendo or even rest.
   (dynamics :accessor dynamics :initarg :dynamics :type list :initform '(1))))

(defclass tape-layer (minute-layer)
  ((instruments :accessor instruments :type list :initarg :instruments
		:initform '(computer))))
    
(defclass instrument-layer (minute-layer)
  ((instruments :accessor instruments :type list :initarg :instruments
		:initform '())))

;; ** print objects

(defmethod print-object ((mn minute) stream)
  (format stream "<MINUTE ~a>" (id mn)))

(defmethod print-object ((mnl minute-layer) stream)
  (format stream "<MINUTE-LAYER ~a>" (id mnl)))

;; ** some methods

(defmethod update-layer-start-times ((mn minute))
  (let ((time (start-time mn)))
    (loop for layer in (layers mn)
	  do (setf (start-time layer) time))
    mn))

(defmethod get-section-durations ((mnl minute-layer))
  (let* ((ratios (div-ratios mnl))
	 (sum (apply #'+ ratios)))
    (loop for ratio in ratios collect (* (/ 60 sum) ratio))))
  
(defmethod get-section-durations ((mn minute))
  (loop for layer in (layers mn)
	collect (get-section-durations layer)))

;; this is a stub, not needed atm
(defmethod interpret-layer ((tl tape-layer))
  ;;&rest keyargs &key &allow-other-keys)
  (let* ((durs (get-section-durations tl))
	 (states (states tl)))
    (setf states
	  (mapcar #'(lambda (x) (case x (1 'C4)
				 (2 'd4)
				 (3 'e4)
				 (4 'f4)
				 (5 'g4)
				 (6 'a4)
				 (7 'b4)
				 (t 'c5)))
		  states))
    (loop for ins in (instruments tl)
	  collect `(,ins computer ,durs ,states ,(marks tl)))))

;; this is a stub, not needed atm
(defmethod interpret-layer ((il instrument-layer))
  ;;&rest keyargs &key &allow-other-keys)
  (let* ((durs (get-section-durations il))
	 (states (states il))
	 (dynamics (dynamics il))
	 (new-durs '())
	 (pitches '())
	 (marks '()))
    (loop for state in states and dur in durs and dyn in dynamics
	  do (if (= 0 dyn)
		 ;; when dynamics = 0 make a rest:
		 (progn (push dur new-durs) (push nil pitches))
		 ;; interpret states
		 (case state
		   (1 (push 'c4 pitches))
		   (2 (push 'd4 pitches))
		   (3 (push 'e4 pitches))
		   (4 (push 'f4 pitches))
		   (5 (push 'g4 pitches))
		   (6 (push 'a4 pitches))
		   (7 (push 'b4 pitches))
		   (t (push 'c5 pitches))))
	  finally (setf new-durs (reverse new-durs) pitches (reverse pitches)))
    ;; dynamics into marks
    (loop for dyn in dynamics and i from 0
	  unless (= 0 dyn)
	    do (case dyn
		 (1 (push `(,i p) marks))
		 (2 (push `(,i mf) marks))
		 (3 (push `(,i ff) marks))
		 (4 (push `(,i cresc-beg) marks) (push `(,(1+ i) cresc-end) marks))
		 (5 (push `(,i dim-beg) marks) (push `(,(1+ i) dim-end) marks))))
    (setf marks (reverse marks))    ; important, idk why exactly
    ;; assemble lists for lists-to-xml
    (loop for ins in (instruments il) and i from 0
	  collect (,instrument
		      ,(cond ((< (length (string instrument)) 6) instrument)
			     ((string= (subseq (string instrument) 0 6) "VIOLIN")
			      'violin)
			     (t instrument))
		      ,durs ,pitches ,marks))))

;; ** Interpretation

;; *** interpret-layer-by-instrument
;;; TODO atm this is just a copy of the same method for instrument-layers
;;; obvsly this should do something else entirely. Probably not even be notated.
(defmethod interpret-layer-by-instrument ((tl tape-layer)
					  &optional instrument)
  (let* ((durs (get-section-durations tl))
	 (states (states tl))
	 (dynamics (dynamics tl))
	 (new-durs '())
	 (pitches '())
	 (marks '()))
    (loop for state in states and dur in durs and dyn in dynamics
	  do (if (= 0 dyn)
		 ;; when dynamics = 0 make a rest:
		 (progn (push dur new-durs) (push nil pitches))
		 ;; interpret states
		 (case state
		   (1 (push 'c4 pitches))
		   (2 (push 'd4 pitches))
		   (3 (push 'e4 pitches))
		   (4 (push 'f4 pitches))
		   (5 (push 'g4 pitches))
		   (6 (push 'a4 pitches))
		   (7 (push 'b4 pitches))
		   (t (push 'c5 pitches))))
	  finally (setf new-durs (reverse new-durs) pitches (reverse pitches)))
    ;; dynamics into marks
    (loop for dyn in dynamics and i from 0
	  unless (= 0 dyn)
	    do (case dyn
		 (1 (push `(,i p) marks))
		 (2 (push `(,i mf) marks))
		 (3 (push `(,i ff) marks))
		 (4 (push `(,i cresc-beg) marks) (push `(,(1+ i) cresc-end) marks))
		 (5 (push `(,i dim-beg) marks) (push `(,(1+ i) dim-end) marks))))
    (setf marks (reverse marks))    ; important, idk why exactly
    ;; assemble lists for lists-to-xml
    `(,instrument
	 computer ,durs ,pitches ,marks)))

;; (defun test (nr)
;;   (lambda (&rest args) (unless (= nr (length args)) (warn "wrong nummber of arguments: ~a" (length args)))))

;;; a lot of work to do
(defmethod interpret-layer-by-instrument ((il instrument-layer)
					  &optional instrument)
  (let* ((durs (get-section-durations il))
	 (states (states il))
	 (dynamics (dynamics il))
	 (new-durs '())
	 (pitches '())
	 (marks '()))
    (loop for state in states and dur in durs and dyn in dynamics
	  do (if (= 0 dyn)
		 ;; when dynamics = 0 make a rest:
		 (progn (push dur new-durs) (push nil pitches))
		 ;; interpret states
		 (case state
		   (1 (push 'c4 pitches))
		   (2 (push 'd4 pitches))
		   (3 (push 'e4 pitches))
		   (4 (push 'f4 pitches))
		   (5 (push 'g4 pitches))
		   (6 (push 'a4 pitches))
		   (7 (push 'b4 pitches))
		   (t (push 'c5 pitches))))
	  finally (setf new-durs (reverse new-durs) pitches (reverse pitches)))
    ;; dynamics into marks
    (loop for dyn in dynamics and i from 0
	  unless (= 0 dyn)
	    do (case dyn
		 (1 (push `(,i p) marks))
		 (2 (push `(,i mf) marks))
		 (3 (push `(,i ff) marks))
		 (4 (push `(,i cresc-beg) marks) (push `(,(1+ i) cresc-end) marks))
		 (5 (push `(,i dim-beg) marks) (push `(,(1+ i) dim-end) marks))))
    (setf marks (reverse marks))    ; important, idk why exactly
    ;; assemble lists for lists-to-xml
    `(,instrument
	 ,(cond ((< (length (string instrument)) 6) instrument)
		((string= (subseq (string instrument) 0 6) "VIOLIN") 'violin)
		(t instrument))
	 ,durs ,pitches ,marks)))

;; ** make

(defun make-minute (id start-time &optional (layers '()))
  (make-instance 'minute :id id :start-time start-time :layers layers))

(defun make-tape-layer (id-uniquifier number start-time
			&optional (div-ratios '(1)) (states '(0))
			  (dynamics '(1)))
  (unless (integerp number) (error "number must be an integer, not ~a" number))
  (make-instance 'tape-layer
		 :id (intern (format nil "tape-layer-~a" id-uniquifier) :ly)
		 :number number
		 :start-time start-time
		 :div-ratios div-ratios
		 :states states
		 :dynamics dynamics))

(defun make-instrument-layer (id-uniquifier number start-time
			      &optional (div-ratios '(1)) (states '(0))
				(dynamics '(1)))
  (unless (integerp number) (error "number must be an integer, not ~a" number))
  (make-instance 'instrument-layer
		 :id (intern (format nil "instr-layer-~a" id-uniquifier) :ly)
		 :number number
		 :start-time start-time
		 :div-ratios div-ratios
		 :states states
		 :dynamics dynamics))

;; ** operations on lists of minutes
;;; (get-related-minute-layers (access-minutes) 0 'number)
;;; (get-related-minute-layers (access-minutes) 'french-horn 'instruments)
(defun get-related-minute-layers (list-of-minutes value
				  &optional (slot 'number) (error-fun #'warn))
  (loop with flag
	for minute in list-of-minutes
	for layer = (car (member value (layers minute)
				 :key slot
				 :test (lambda (x y)
					 (find x (if (listp y) y (list y))))))
	unless layer do (setf flag t)
	  when layer collect layer into result
	    finally (when flag
		      (funcall error-fun "not all minutes had a layer with ~
                                          this value for ~a: ~a" slot value))
		    (return result)))

(defun get-all-related-durations (list-of-minutes layer-number
				  &optional (error-fun #'warn))
  (loop for layer in (get-related-minute-layers
		      list-of-minutes layer-number 'number error-fun)
	append (get-section-durations layer)))

(defun interpret-minutes-by-instrument (list-of-minutes instrument)
  (let* ((layers (get-related-minute-layers list-of-minutes instrument
					    'instruments))
	 (starts (loop for i in layers collect (start-time i)))
	 (interpretation '()))
    (unless layers (error "not a single layer with ~a found in ~a"
			  instrument list-of-minutes))
    (do ((i 0 (1+ i)) int)
	((>= i (length layers)) (setf interpretation (reverse int)))
      (push (interpret-layer-by-instrument (nth i layers) instrument) int))
    ;; append durs and pitches, fill in gaps and increment mark-indices.
    (loop with player = (caar interpretation)
	  with nr-of-durs = 0
	  with time = 0
	  for i in interpretation and start in starts
	  unless (>= time start)
	    append (list (- start time)) into durs
	  unless (>= time start)
	    append (list nil) into pitches
	  unless (>= time start)
	    do (incf time (- start time))
	       (incf nr-of-durs)
	  when (> time start)
	    do (warn "current time greater than next start-time: ~a" time)
	  append (third i) into durs
	  append (fourth i) into pitches
	  append (mapcar #'(lambda (x) (incf (car x) nr-of-durs) x) (fifth i))
	    into marks
	  do (incf nr-of-durs (length (third i)))
	     (incf time (apply #'+ (third i)))
	  finally (return `(,player ,(second i) ,durs ,pitches ,marks)))))

;;; interpret all layers of a list-of-minutes unless you provide a list of
;;; instruments - then only those will be interpreted.
;;; TODO - differ between tape and instrument.
(defun interpret-minutes (list-of-minutes
			  &optional instruments (error-fun #'warn))
  (let ((instr '()))
    (unless instruments
      (loop for minute in list-of-minutes
	    do (loop for layer in (layers minute)
		     do (loop for ins in (instruments layer)
			      unless (member ins instruments)
				do (push ins instruments))))
      (setf instruments (reverse instruments)))
    ;; now collect again but in the order we want:
    (loop for i in '(double-bass cello viola violin-2 violin-1 percussion tuba
		     bass-trombone french-horn c-trumpet percussion bassoon
		     b-flat-clarinet oboe flute)
	  when (find i instruments) do (push i instr))
    (loop for i in instruments unless (find i instr) do (push i instr))
    ;; now go through the new list of instruments.
    (loop for ins in instr
	  collect (interpret-minutes-by-instrument list-of-minutes ins))))

;; ** visualize

(defun visualize-minutes (list-of-minutes list-of-layer-numbers
			  path-and-filename
			  &optional (size-factor 1) (specify-time-and-layer t)
			    show-dynamics-instead-of-state)
  (format t "~&visualizing minutes...")
  (imago::show-minutes list-of-minutes list-of-layer-numbers path-and-filename
		       size-factor specify-time-and-layer
		       show-dynamics-instead-of-state))

;; ** minutes

;;; Michael Edwards challenged me to not use a single global variable, so I 
;;; guess I'm going to use closures:
;;; minutes holds all 11 minute objects, which are filled with tape and
;;; instrument-layers.
;;; number-of-divisions stores the total number of parts in each layer.
;;; lnrs is a plist with the ids/numbers for the layers. First is tape layer,
;;; the rest are instrument-layers.
(let ((minutes '())
      (number-of-divisions '())
      (l-nrs '(0 1 2 3)))
  ;; init minutes:
  (setf minutes
	(loop for i from 0 to 10
	      for time = (* i 60)
	      for tape-layer = (make-tape-layer (car l-nrs) (car l-nrs) time)
	      for instrument-layers = '()
	      do (setf instrument-layers
		       (loop for k in (cdr l-nrs) ; net numbers for layers (1-3)
			     collect (make-instrument-layer k k time)))
	      collect (make-minute i time
				   (cons tape-layer instrument-layers))))
  ;; init number-of-divisions:
  (loop for nr in (reverse l-nrs)
	for layers = (get-related-minute-layers minutes nr 'number #'error)
	for len = (length (loop for i in layers append (div-ratios i)))
	do (push-key-value nr len number-of-divisions))
  ;; closures to access minutes and co everywhere:
  (defun access-minutes () minutes)
  (defun (setf access-minutes) (new-value) (setf minutes new-value))
  (defun minutes-nod () number-of-divisions)
  (defun (setf minutes-nod) (new-value) (setf number-of-divisions new-value))
  (defun minutes-layer-numbers () l-nrs))

;; add undivided layers for reference when visualizing, number 111
(loop for minute in (access-minutes) and i from 0
      do (setf (layers minute)
	       (append (layers minute)
		       `(,(make-tape-layer 111 111 (* i 60) '(1)
					   `(,(if (= 0 (mod i 2)) 0 5))
					   `(,(if (= 0 (mod i 2)) 0 5)))))))

;; EOF mintues.lsp
