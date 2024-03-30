;; * minutes

(in-package :ly)

;; not the prettiest solution but avoids style-warnings for now:
(declaim (ftype (function) imago::show-minutes))

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

;; ** Interpretation

;; *** ins-get-rest
(defun ins-get-rest (dur)
  (values `(,dur) '(nil) '() 1))

;; *** tape-get-static
(defun tape-get-static (dur)
  (values `(,dur) '(c4) '()))

;; *** tape-get-static-rhythm
(defun tape-get-static-rhythm (dur sum)
  (let* ((new-durs '())
	 (pitches '())
	 (nr 0)
	 (spitch 'c4))
    (multiple-value-bind (st md nd) (get-st-md-nd sum dur)
      ;; set number of notes
      (unless (= 0 st) (incf nr))
      (unless (= 0 nd) (incf nr))
      (incf nr (* (/ md 4) 16))
      (loop for i from 0 below nr
	    do (push (cond ((and (= i 0) (not (= 0 st))) st)
			   ((and (= i (1- nr)) (not (= 0 nd))) nd)
			   (t .25))
		     new-durs)
	       (push (if (or (= i (1- nr)) (= 0 (mod i 2))) nil spitch) pitches))
      (values (reverse new-durs) (reverse pitches) '()))))

;; *** tape-get-morphing-rhythm
(defun tape-get-morphing-rhythm (dur sum)
  (let* ((new-durs '())
	 (pitches '())
	 (nr 0)
	 (spitch 'c4))
    (multiple-value-bind (st md nd) (get-st-md-nd sum dur)
      ;; set number of notes
      (unless (= 0 st) (incf nr))
      (unless (= 0 nd) (incf nr))
      (incf nr (* (/ md 4) 16))
      (loop for i from 0 below nr
	    do (push (cond ((and (= i 0) (not (= 0 st))) st)
			   ((and (= i (1- nr)) (not (= 0 nd))) nd)
			   (t .25))
		     new-durs)
	       (push (if (= 0 (random 2)) nil spitch) pitches))
      (values (reverse new-durs) (reverse pitches) '()))))

;; *** tape-get-changing-rhythmic-speed
(defun tape-get-changing-rhythmic-speed (dur index)
  (values `(,dur) '(c4) `((,index "changing-rhythimc-speed"))))

;; *** tape-get-changing-pulse-speed
(defun tape-get-changing-pulse-speed (dur index)
  (values `(,dur) '(c4) `((,index "changing-pulse-speed"))))

;; *** tape-get-changing-timbres
(defun tape-get-changing-timbres (dur index)
  (values `(,dur) '(c4) `((,index "tape-get-changing-timbres"))))

;; *** tape-get-changing-timbres
(defun tape-get-changing-timbres (dur index)
  (values `(,dur) '(c4) `((,index "tape-get-changing-timbres"))))

;; *** tape-get-drifting-rhythmic-speeds
(defun tape-get-drifting-rhythmic-speeds (dur index)
  (values `(,dur) '(c4) `((,index "tape-get-drifting-rhythmic-speeds"))))

;; *** tape-get-drifting-pulse-speed
(defun tape-get-drifting-pulse-speed (dur index)
  (values `(,dur) '(c4) `((,index "tape-get-drifting-pulse-speed"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *** ins-get-static
(defun ins-get-static (instrument dur chord)
  (case instrument
    ((tuba) (values `(,dur) '(bf0) '()))
    (double-bass (values `(,dur) '(b0) '()))
    (bass-trombone (values `(,dur) `(,(second (nth 2 chord))) '()))
    ((french-horn c-trumpet b-flat-clarinet bassoon oboe flute
		  violin-1 violin-2 viola cello)
     (values `(,dur) `(,(note-for-ins instrument (nth 2 chord))) '()))
    (percussion (values `(,dur) `(,(nth (random (length chord)) (nth 2 chord))) '()))
    (t (ins-get-rest dur))))

;; *** ins-get-static-rhythm
;;; pulse goes through instruments and number of instruments playing one pulse
;;; is determined by the "weight" of that pulse
;;; (by indisp with 13 pulses = 1 "bar")
#+nil(defun ins-get-static-rhythm (instrument dur index sum chord)
  (declare (ignore index))
  (let* ((new-durs '())
	 (pitches '())
	 (nr 0)
	 spitch)
    (multiple-value-bind (st md nd) (get-st-md-nd sum dur)
      ;; set number of notes
      (unless (= 0 st) (incf nr))
      (unless (= 0 nd) (incf nr))
      (incf nr (* (/ md 4) 16))
      ;; custom stuff:
      (case instrument
	(tuba (setf spitch nil))
	(double-bass (setf spitch 'b0))
	(t (setf spitch (note-for-ins instrument (nth 2 chord)))))
      (loop for i from 0 below nr
	    do (push (cond ((and (= i 0) (not (= 0 st))) st)
			   ((and (= i (1- nr)) (not (= 0 nd))) nd)
			   (t .25))
		     new-durs)
	       (push (if (or (= i (1- nr)) (= 0 i)) nil spitch) pitches))
      (values (reverse new-durs) (reverse pitches) '()))))


(defun ins-get-static-rhythm (instrument dur index sum chord)
  ;;(declare (ignore index))
  (let* ((new-durs '())
	 (pitches '())
	 (marks '())
	 (spitch (note-for-ins instrument (nth 2 chord)))
	 (divisor 4) ;; divide one bar (= 4 seconds)
	 (active-ls '(1 1 0 0 0 0 0 1)))
    (multiple-value-bind (st md nd) (get-st-md-nd sum dur)
      ;; custom stuff:
      (case instrument
	(tuba (setf spitch nil))
	(percussion (setf spitch nil marks `((,index "solo"))))
	((flute oboe cello)
	 (setf active-ls '(1 1 1 0 0 1 1 0 1)))
	((b-flat-clarinet c-trumpet)
	 (setf active-ls '(1 1 1 0 0 1 1 0 1) divisor 6))
	((bassoon french-horn bass-trombone)
	 (setf active-ls '(0 0 1 1 0 0 0 1 1 0 0 0 1 1 0 0)))
	(double-bass (setf spitch 'b0)
	 (setf active-ls '(0 0 0 1 1 1 0 0 0 0 1 1 1 1 0 0)))
	((violin-1 violin-2 viola)
	 (setf active-ls '(0 0 0 1 1 1 0 0 0 0 0 1 1 1 1 0 0))))
      ;; get durations:
      (loop for i from 0 below (round (* (/ md 4) divisor)) and last = 0 then el
	    for el = (nth-mod i active-ls)
	    with ind = (if (= st 0) 0 1)
	    with cnt = 0
	    with chord-cnt = 2
	    do (when (= el 0)
		 (when (= last 1)
		   (push (* cnt (/ 4 divisor)) new-durs)
		   (push spitch pitches)
		   (push `(,ind pp) marks)
		   (push `(,ind cresc-beg) marks)
		   (push `(,(1+ ind) f) marks)
		   (push `(,ind cresc-end) marks)
		   (incf ind)
		   (incf chord-cnt -1)
		   (setf spitch
			 (note-for-ins instrument
				       (nth-mod chord-cnt chord))))
		 (push (/ 4 divisor) new-durs)
		 (push nil pitches)
		 (incf ind))
	       (if (= el 1) (incf cnt) (setf cnt 0))
	    finally (when (and el (= 1 el))
		      (push (* cnt (/ 4 divisor)) new-durs)
		      (push spitch pitches))
		    (unless (= 0 nd)
		      (push nd new-durs)
		      (push nil pitches))
		    (setf new-durs (reverse new-durs))
		    (setf pitches (reverse pitches))
		    (unless (= 0 st)
		      (push st new-durs)
		      (push nil pitches)))
      (values new-durs pitches (reverse marks)))))

;; *** ins-get-morphing-rhythms
;;; like ins-get-static-rhythm but morph rhythm (indisp-fun).
(defun ins-get-morphing-rhythms (instrument dur index sum chord)
  (declare (ignore index))
  (let* ((new-durs '())
	 (pitches '())
	 (nr 0)
	 spitch)
    (multiple-value-bind (st md nd) (get-st-md-nd sum dur)
      ;; set number of notes
      (unless (= 0 st) (incf nr))
      (unless (= 0 nd) (incf nr))
      (incf nr (* (/ md 4) 16))
      ;; custom stuff:
      (case instrument
	(tuba (setf spitch nil))
	(double-bass (setf spitch 'b0))
	(t (setf spitch (note-for-ins instrument (nth 1 chord)))))
      (loop for i from 0 below nr
	    do (push (cond ((and (= i 0) (not (= 0 st))) st)
			   ((and (= i (1- nr)) (not (= 0 nd))) nd)
			   (t .25))
		     new-durs)
	       (push (if (= 0 (random 2)) nil spitch) pitches))
      (values (reverse new-durs) (reverse pitches) '()))))

;; *** ins-get-changing-timbres
(defun ins-get-changing-timbres (instrument dur chord)
  (case instrument
    (t (values `(,dur) `(,(note-for-ins instrument (nth 2 chord))) '()))))

;; *** ins-get-isorhythmic-rhythms
;;; strings: start clb unisono and then drift apart.
(defun ins-get-isorhythmic-rhythms (instrument dur index sum chord)
  (let* ((new-durs '())
	 (pitches '())
	 (marks '())
	 (nr 0)
	 spitch)
    (multiple-value-bind (st md nd) (get-st-md-nd sum dur)
      ;; set number of notes
      (unless (= 0 st) (incf nr))
      (unless (= 0 nd) (incf nr))
      (incf nr (* (/ md 4) 16))
      ;; custom stuff:
      (case instrument
	((violin-2 violin-1)
	 (setf spitch (note-for-ins instrument (nth 0 chord))) (push `(,index clb) marks)
	 (push `(,(+ index (min (1- nr) 2)) "schneller werden") marks))
	((viola cello)
	 (setf spitch (note-for-ins instrument (nth 0 chord))) (push `(,index clb) marks)
	 (push `(,(+ index (min (1- nr) 2)) "langsamer werden") marks))
	((double-bass)
	 (setf spitch 'b0) (push `(,index clb) marks))
	(t (setf spitch (note-for-ins instrument (nth 0 chord)))
	 (decf nr (* (/ md 4) 16)) (incf nr (* (/ md 4) 20))))
      (loop for i from 0 below nr
	    do (push (cond ((and (= i 0) (not (= 0 st))) st)
			   ((and (= i (1- nr)) (not (= 0 nd))) nd)
			   (t (case instrument
				((violin-2 violin-1 viola cello double-bass) .25)
				(t .2))))
		     new-durs)
	       (push (if (or (= i (1- nr)) (= 0 i)) nil spitch) pitches))
      (values (reverse new-durs) (reverse pitches) (reverse marks)))))

;; *** ins-get-converging-pitches
;;; for now just the opposite of driftig-pitches
(defun ins-get-converging-pitches (instrument dur index sum chord)
  (let* ((new-durs '())
	 (pitches '())
	 (marks '())
	 (nr 0)	 
	 spitch
	 tpitch)
    (multiple-value-bind (st md nd) (get-st-md-nd sum dur)
      ;; get start and target pitch
      (setf tpitch (note-for-ins instrument (nth 0 chord))
	    ;; spitch (+ tpitch (1+ (random 7))))
	    spitch (note-for-ins instrument (nth 2 chord)))
      ;; set number of notes
      (unless (= 0 st) (incf nr))
      (unless (= 0 nd) (incf nr))
      (incf nr (/ md 4))
      ;; custom stuff:
      (case instrument
	(tuba (setf nr 1 st dur md 0 nd 0 spitch (note-to-midi 'bf0)))
	(double-bass (setf spitch (note-to-midi 'b0) tpitch (note-to-midi 'e1))))
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

;; *** ins-get-drifting-pitches
(defun ins-get-drifting-pitches (instrument dur index sum chord)
  (let* ((new-durs '())
	 (pitches '())
	 (marks '())
	 (nr 0)	 
	 spitch
	 tpitch)
    (multiple-value-bind (st md nd) (get-st-md-nd sum dur)
      ;; get start and target pitch
      (setf spitch (note-for-ins instrument (nth 0 chord))
	    ;; tpitch (+ spitch (1+ (random 7))))
	    tpitch (note-for-ins instrument (nth 2 chord)))
      ;; set number of notes
      (unless (= 0 st) (incf nr))
      (unless (= 0 nd) (incf nr))
      (incf nr (/ md 4))
      ;; custom stuff:
      (case instrument
	(tuba (setf nr 1 st dur md 0 nd 0 spitch (note-to-midi 'bf0)))
	(double-bass (setf spitch (note-to-midi 'e1) tpitch (note-to-midi 'b0))))
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
#|
(lists-to-midi (gen-melodic-line 16 55 '(30 65) '(0 0  .2 0 1 7) '(0 0 .2 0  1 1)) '(1) (loop for i from 0 below 16 collect i) :file "/E/code/ensemble/test4.mid")
|#
(defun ins-get-drifting-metres (instrument dur index sum chord)
  (let* ((new-durs '())
	 (pitches '())
	 (marks '())
	 spitch
	 (divisor 2)
	 (ambitus '(40 75))
	 (diss-env '(0 0 .3 0  1 7))
	 (var-env '(0 0  .3 0  1 1))
	 (nr 0))
    (multiple-value-bind (st md nd) (get-st-md-nd sum dur)
      ;; set number of notes
      (unless (= 0 st) (incf nr))
      (unless (= 0 nd) (incf nr))
      ;; custom stuff:
      (case instrument
	((violin-2 violin-1) (setf spitch (note-for-ins instrument (nth 0 chord)) divisor 3)
	 (incf nr (* (/ md 4) divisor)))
	(viola (setf spitch (note-for-ins instrument (nth 0 chord)) divisor 4)
	 (incf nr (* (/ md 4) divisor)))
	(cello (setf spitch (note-for-ins instrument (nth 0 chord)) divisor 5)
	 (incf nr (* (/ md 4) divisor)))
	(t (setf spitch (note-for-ins instrument (nth 0 chord)))
	 (incf nr (* (/ md 4) divisor))))
      ;; get melodic line
      (setf pitches (gen-melodic-line nr spitch ambitus diss-env var-env))
      ;; get notes and gliss marks
      (push `(,index "drifting...") marks)
      (loop for i from 0 below nr
	    do (cond ((and (= i 0) (not (= 0 st)))
		      (push st new-durs))
		     ((and (= i (1- nr)) (not (= 0 nd)))
		      (push nd new-durs))
		     (t (push (/ 4 divisor) new-durs))))
      ;; Return list of durations pitches marks
      (values (reverse new-durs) pitches (reverse marks)))))

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
		       (2 (tape-get-static-rhythm dur sum))
		       (3 (tape-get-morphing-rhythm dur sum))
		       (4 (tape-get-changing-rhythmic-speed dur index))
		       (5 (tape-get-changing-pulse-speed dur index))
		       (6 (tape-get-changing-timbres dur index))
		       (7 (tape-get-drifting-rhythmic-speeds dur index))
		       (8 (tape-get-drifting-pulse-speed dur index))
		       (t (tape-get-static dur))))
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
	 computer ,new-durs ,pitches ,marks)))

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
	 (chords (chords il))
	 (indices '(0))
	 (new-durs '())
	 (pitches '())
	 (marks '()))
    (loop for state in states and dur in durs and dyn in dynamics
	  and chord in chords
	  with index = 0
	  do (multiple-value-bind (d p m)
		 (if (= 0 dyn)
		     ;; when dynamics = 0 make a rest:
		     (ins-get-rest dur)
		     ;; interpret states
		     (case state
		       (2 (ins-get-static-rhythm instrument dur index sum chord))
		       (3 (ins-get-morphing-rhythms instrument dur index sum chord))
		       (4 (ins-get-changing-timbres instrument dur chord))
		       (5 (ins-get-isorhythmic-rhythms instrument dur index sum chord))
		       (6 (ins-get-converging-pitches instrument dur index sum chord))
		       (7 (ins-get-drifting-pitches instrument dur index sum chord))
		       (8 (ins-get-drifting-metres instrument dur index sum chord))
		       (t (ins-get-static instrument dur chord))))
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

(defmethod interpret-layer ((ml minute-layer))
  (loop for ins in (instruments ml)
	collect (interpret-layer-by-instrument ml ins)))

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

;; *** set-related-dynamics
;;; loop through the dynamics of a source layer and a target layer and decide
;;; the targets dynamics with the ones of source
;;; #'case is applied to case-list - determine how to replace/couple dynamics.
;;; transition fun gets nr-of-elements and list '(t nil), where t means, that a
;;; value will be replaced. If transition-fun is nil, no transition is made --
;;; all dynamics are changed.
(defun set-related-dynamics (list-of-minutes source-layer-nr target-layer-nr
			     case-list
			     &optional (transition-fun 'fibonacci-transitions))
  (let* ((slayers (get-related-minute-layers list-of-minutes source-layer-nr))
	 (tlayers (get-related-minute-layers list-of-minutes target-layer-nr))
	 (tdurs (get-all-related-durations list-of-minutes target-layer-nr))
	 (trans (ml t (apply #'+ tdurs)))
	 (cnt 0))
    (when transition-fun
      (setf trans (funcall transition-fun (apply #'+ tdurs) '(t nil))))
    (loop for slayer in slayers and tlayer in tlayers
	  for sdynamics = (dynamics slayer) and tdynamics = (dynamics tlayer)
	  do (setf (dynamics tlayer)
		   (loop for d in tdynamics and n from 0
			 for s = (nth n sdynamics)
			 collect (if (nth cnt trans)
				     ;; since case is a macro, not a function,
				     ;; we can't just use apply...
				     (loop for ls in case-list
					   do (when
						  (find s (if (listp (car ls))
							      (car ls)
							      (list (car ls))))
						(return (cadr ls)))
					   finally (return d))
				     d)
			   into new
			 do (incf cnt)
			 finally (return new))))))

(defun interpret-minutes-by-instrument (list-of-minutes instrument
					&optional separate)
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
	  append (third i) into durs when separate append '(4) into durs
	  append (fourth i) into pitches when separate append '(()) into pitches
	  append (mapcar #'(lambda (x) (incf (car x) nr-of-durs) x) (fifth i))
	    into marks
	  do (incf nr-of-durs (+ (length (third i)) (if separate 1 0)))
	     (incf time (apply #'+ (third i)))
	  finally (return `(,player ,(second i) ,durs ,pitches ,marks)))))

;;; interpret all layers of a list-of-minutes unless you provide a list of
;;; instruments - then only those will be interpreted.
(defun interpret-minutes (list-of-minutes &optional instruments (separate t))
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
	  collect (interpret-minutes-by-instrument list-of-minutes ins
						   separate))))

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
