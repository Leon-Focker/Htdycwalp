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
   (dynamics :accessor dynamics :initarg :dynamics :type list :initform '(1))
   (chords :accessor chords :initarg :chords :type list :initform '(() ()))))

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

(defmethod interpret-layer ((ml minute-layer))
  (loop for ins in (instruments ml)
	collect (interpret-layer-by-instrument ml ins)))

;; ** Interpretation

;; *** interpret-layer-by-instrument
;; **** interpret tape
;;; STATES: 1 - static
;;;         2 - static rhythms
;;;         3 - morphing rhythms
;;;         4 - changing rhythmic speed
;;;         5 - changing pulse speed
;;;         6 - changing timbres
;;;         7 - drifting rhythmic speeds
;;;         8 - drifting pulse speeds
;;;
;;; Each state is allowed to also use methods described in any state above it.
;;;
;;; TODO atm this is just a copy of the same method for instrument-layers
;;; obvsly this should do something else entirely. Probably not even be notated.
(defmethod interpret-layer-by-instrument ((tl tape-layer) instrument)
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
		 (1 (push `(,i cresc-beg) marks) (push `(,(1+ i) cresc-end) marks))
		 (2 (push `(,i dim-beg) marks) (push `(,(1+ i) dim-end) marks))
		 (3 (push `(,i p) marks))
		 (4 (push `(,i mf) marks))
		 (5 (push `(,i ff) marks))))
    (setf marks (reverse marks))    ; important, idk why exactly
    ;; assemble lists for lists-to-xml
    `(,instrument
	 computer ,durs ,pitches ,marks)))

;; (defun test (nr)
;;   (lambda (&rest args) (unless (= nr (length args)) (warn "wrong nummber of arguments: ~a" (length args)))))

;;; **** interpret instruments
;;; STATES: 1 - static
;;;         2 - static rhythms
;;;         3 - morphing rhythms
;;;         4 - changing timbres
;;;         5 - isorhythmic rhythms
;;;         6 - converging pitches (glissandi)
;;;         7 - drifting pitches (glissandi)
;;;         8 - drifting metres (notes per section)
;;;
;;; Each state is allowed to also use methods described in any state above it.
;;;
(defmethod interpret-layer-by-instrument ((il instrument-layer) instrument)
  (let* ((durs (get-section-durations il))
	 (states (states il))
	 (dynamics (dynamics il))
	 (indices '(0))
	 (new-durs '())
	 (pitches '())
	 (marks '()))
    (loop for state in states and dur in durs and dyn in dynamics
	  with index = 0
	  do (multiple-value-bind (d p m)
		 (if (= 0 dyn)
		     ;; when dynamics = 0 make a rest:
		     (ins-get-rest dur)
		     ;; interpret states
		     (case state
		       (2 (ins-get-static-rhythm instrument dur))
		       (3 (ins-get-morphing-rhythms instrument dur))
		       (4 (ins-get-changing-timbres instrument dur))
		       (5 (ins-get-isorhythmic-rhythms instrument dur))
		       (6 (ins-get-converging-pitches instrument dur))
		       (7 (ins-get-drifting-pitches instrument dur index sum))
		       (8 (ins-get-drifting-metres instrument dur))
		       (t (ins-get-static instrument dur))))
	       (incf index (length d))
	       (setf new-durs (append new-durs d)
		     pitches (append pitches p)
		     marks (append marks m)
		     indices (append indices (list index))))
	  sum dur into sum)
    ;; dynamics into marks
    (loop for dyn in dynamics and i from 0
	  unless (= 0 dyn)
	    do (case dyn
		 (1 (push `(,(nth i indices) cresc-beg) marks)
		  (push `(,(nth (1+ i) indices) cresc-end) marks))
		 (2 (push `(,(nth i indices) dim-beg) marks)
		  (push `(,(nth (1+ i) indices) dim-end) marks))
		 (3 (push `(,i p) marks))
		 (4 (push `(,i mf) marks))
		 (5 (push `(,i ff) marks))))
    (setf marks (reverse marks))    ; important, idk why exactly
    ;; assemble lists for lists-to-xml
    `(,instrument
	 ,(cond ((< (length (string instrument)) 6) instrument)
		((string= (subseq (string instrument) 0 6) "VIOLIN") 'violin)
		(t instrument))
	 ,new-durs ,pitches ,marks)))

;; *** ins-get-rest
(defun ins-get-rest (dur)
  (values `(,dur) '(nil) '() 1))

;; *** ins-get-static
(defun ins-get-static (instrument dur)
  (case instrument
    ((tuba) (values `(,dur) '(bf0) '()))
    ((violin-1 violin-2 viola cello double-bass)
     (values `(,dur) '(c4) '()))
    (t (values `(,dur) '(nil) '()))))

;; *** ins-get-static-rhythm
(defun ins-get-static-rhythm (instrument dur)
  (case instrument
    (t (values `(,dur) '(d4) '()))))

;; *** ins-get-morphing-rhythms
(defun ins-get-morphing-rhythms (instrument dur)
  (case instrument
    (t (values `(,dur) '(e4) '()))))

;; *** ins-get-changing-timbres
(defun ins-get-changing-timbres (instrument dur)
  (case instrument
    (t (values `(,dur) '(f4) '()))))

;; *** ins-get-isorhythmic-rhythms
(defun ins-get-isorhythmic-rhythms (instrument dur)
  (case instrument
    (t (values `(,dur) '(g4) '()))))

;; *** ins-get-converging-pitches
(defun ins-get-converging-pitches (instrument dur)
  (case instrument
    (t (values `(,dur) '(a4) '()))))

;; *** ins-get-drifting-pitches
(defun ins-get-drifting-pitches (instrument dur index sum)
  (let* ((new-durs '())
	 (pitches '())
	 (marks '())
	 (nr 0)	 
	 spitch
	 tpitch)
    (multiple-value-bind (st md nd) (get-st-md-nd sum dur)
      ;; get start and target pitch
      (setf spitch (note-to-midi 'e5)
	    tpitch (+ 72 (random 8)))
      ;; set number of notes
      (unless (= 0 st) (incf nr))
      (unless (= 0 nd) (incf nr))
      (incf nr (/ md 4))
      ;; custom stuff:
      (case instrument
	(tuba (incf nr) (setf st dur) (setf spitch (note-to-midi 'bf0))))
      ;; get notes and gliss marks
      (loop for i from 0 below nr
	    do (cond ((and (= i 0) (not (= 0 st)))
		      (push st new-durs))
		     ((and (= i (1- nr)) (not (= 0 nd)))
		      (push nd new-durs))
		     (t (push 4 new-durs)))
	       (push (midi-to-note
		      (round (+ (* (/ i nr) (- tpitch spitch)) spitch)))
		     pitches)
	       (when (< i (1- nr))
		 (push `(,(+ index i) beg-gliss) marks)
		 (push `(,(+ index i 1) end-gliss) marks)))
      ;; Return list of durations pitches marks
      (values (reverse new-durs) (reverse pitches) (reverse marks)))))

;; *** ins-get-drifting-metres
(defun ins-get-drifting-metres (instrument dur)
  (case instrument
    (t (values `(,dur) '(b4) '()))))

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
(defun interpret-minutes (list-of-minutes &optional instruments)
  (let ((instr '()))
    (unless instruments
      (loop for minute in list-of-minutes
	    do (loop for layer in (layers minute)
		     do (loop for ins in (instruments layer)
			      unless (member ins instruments)
				do (push ins instruments))))
      (setf instruments (reverse instruments)))
    ;; now collect again but in the order we want:
    (loop for i in '(double-bass cello viola violin-2 violin-1 tuba
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
