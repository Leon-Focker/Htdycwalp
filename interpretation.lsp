;; * Interpretation

(in-package :ly)

;; ** synthesis

;; *** variadic
;; this is a monster and could have been avoided with propper planning:
;; return a function that wants pairs of variable names and values
;; the body can contain any variables, as long as they are then given to the
;; generated function. For example:
#|
(funcall (variadic (progn (+ 5 6) (+ 1 y))) 'x 1 'y 2)
=> 3
|#
(defmacro variadic (&body body)
  `(lambda (&rest args)
     (let* ((vars (loop for i in args by #'cddr collect i))
	    (vals (loop for i in (cdr args) by #'cddr collect i))
	    (unused (loop for i in vars unless (find i (flatten ',body))
			  collect i)))
       (eval `(let ,(mapcar #'(lambda (var val) `(,var ,val)) vars vals)
		(declare (ignore ,@unused))
		,',@body)))))

;; *** get-indisp
;;; determine indisp-fun by start-time (of a layer)
(defun get-indisp (start-time)
  ;; minute 1
  (cond ((< start-time 60)
	 (variadic (rqq-to-indispensability-function
		    '(13 (1 1 1 1 1 1 1 1 1 1 1 1 1)) t)))
	;; minute 2
	((< start-time 120)
	 (variadic (rqq-to-indispensability-function
		    '(13 ((8 (1 1 1 1 1 1 1 1)) (5 (1 1 1 1 1)))) t)))
	;; minute 3
	((< start-time 180)
	 (variadic (section-val
		    time
		    0
		    (rqq-to-indispensability-function
		     '(13 ((8 (1 1 1 1 1 1 1 1)) (5 (1 1 1 1 1)))) t)
		    30
		    (rqq-to-indispensability-function
		     '(13 ((7 (1 1 1 1 1 1 1)) (6 (1 1 1 1 1 1)))) t))))
	;; minute 4
	((< start-time 240)
	 (variadic (section-val
		    time
		    0
		    (rqq-to-indispensability-function
		     '(13 ((8 (1 1 1 1 1 1 1 1 1)) (5 (1 1 1 1 1 1))))
		     ;;'(13 ((8 (1 1 1 1 1 1 1 1)) (5 (1 1 1 1 1))))
		     t)
		    45
		    (rqq-to-indispensability-function
		     '(13 ((4 (1 1 1 1)) (5 (1 1 1 1 1)) (4 (1 1 1 1))))
		     ;;'(13 ((4 (1 1 1 1)) (5 (1 1 1 1 1)) (4 (1 1 1 1))))
		     t))))
	;; minute 5
	((< start-time 300)
	 (variadic (section-val
		    time
		    0
		    (rqq-to-indispensability-function
		     '(13 ((3 (1 1 1)) (5 (1 1 1 1 1)) (2 (1 1)) (3 (1 1 1)))) t)
		    30
		    (rqq-to-indispensability-function
		     '(13 ((8 (1 2 1 1 5)) (5 (1 1 3 1)))) t))))
	;; minute 6
	;;((< start-time 360) (variadic ))
	;; minute 7
	((< start-time 420)
	 (variadic (section-val
		    time
		    0
		    (rqq-to-indispensability-function
		     '(13 ((4 (1 1 1 1)) (4 (1 1 1 1)) (4 (1 1 1 1)) (1 (1)))) t)
		    20
		    (rqq-to-indispensability-function
		     '(13 ((5 (1 1 1 1)) (4 (1 1 1 1)) (3 (1 1 1 1)) (1 (1)))) t)
		    40
		    (rqq-to-indispensability-function
		     '(13 ((5 (1 1 1 1)) (2 (1 1 1 1)) (4 (1 1 1 1)) (2 (1)))) t))))
	;; minute 8
	((< start-time 480)
	 (variadic (rqq-to-indispensability-function
		    '(13 ((4 (1 1 1 1)) (4 (1 1 1 1)) (4 (1 1 1 1)) (1 (1)))) t)))
	;;((< start-time 540) (variadic (rqq-to-indispensability-function
	;;	      '(13 ((3 (1 1 1)) (5 (1 1 1 1 1)) (2 (1 1)) (3 (1 1 1)))) t)))
	;((< start-time 600) (variadic ))
	;((< start-time 660) (variadic ))
	(t (variadic (rqq-to-indispensability-function
		      '(13 (1 1 1 1 1 1 1 1 1 1 1 1 1)) t)))))

;; *** tape-get-rhythm
;;; determine rhythm(-fun) by state (1-8)
(defun tape-get-rhythm (s)
  (case s
    (1 (variadic 1/13))
    (2 (variadic 1/13))
    (3 (variadic 1/13))
    (4 (variadic 1/13))
    (5 (variadic (float (dry-wet 1/13 7/13 (expt (- (* 2 line) 1) 2)))))
    (6 (variadic (float (dry-wet 11/26 1/26 (expt (- (* 2 line) 1) 2)))))
    (7 (variadic 1/8))
    (8 (variadic (float (dry-wet 1/13 7/13 (expt (- (* 2 line) 1) 2)))))))

;; *** interpret-tape
;;; returns:
;;; start-times
;;; indisp-fun
;;; rhythm
;;; srt
;;; amp
;;; time-mult
;;; duration
(defmethod interpret-tape ((tl tape-layer))
  (flet ((get-amp-env (d)
	   (case d
	     (0 '(0 0  1 0))
	     (1 '(0 0  1 1))
	     (2 '(0 1  1 0))
	     (3 '(0 .2  1 .2))
	     (4 '(0 .7  1 .7))
	     (5 '(0 .95  1 .95))))
	 (get-srt (s)
	   (case s
	     (1 (variadic .5))
	     (2 (variadic .5))
	     (3 (variadic (dry-wet 0.9 amp-val (* line 2))))
	     (4 (variadic (dry-wet 0.9 amp-val (* line 2))))
	     (5 (variadic (dry-wet 0.9 amp-val (* line 2))))
	     (6 (variadic (dry-wet 0.9 amp-val (expt (- (* 2 line) 1) 2))))
	     (7 (variadic (dry-wet 0.9 amp-val (* line 2))))
	     (8 (variadic (dry-wet 0.9 amp-val (* line 2))))))
	 (get-time-mult (s)
	   (case s
	     (1 (variadic 1))
	     (2 (variadic 1))
	     (3 (variadic 1))
	     (4 (variadic (- 5 (* line 2.5))))
	     (5 (variadic (+ 1 (* line 5))))
	     (6 (variadic (- 5 (* line 2.5))))
	     (7 (variadic (+ 1 (* line 5))))
	     (8 (variadic (- 5 (* line 2.5)))))))
    (let* ((durs (get-section-durations tl))
	   (start-times (get-start-times durs))
	   (dynamics (dynamics tl))
	   (states (states tl))
	   (envelopes '())
	   (rhythm '())
	   (srt '())
	   (time-mult '()))
      (setf envelopes (loop for d in dynamics collect (get-amp-env d)))
      (setf rhythm (loop for s in start-times and st in states
			 for rhythm = (tape-get-rhythm st)
			 collect s collect rhythm)
	    srt (loop for s in start-times and st in states
		      for srt = (get-srt st)
		      collect s collect srt)
	    time-mult (loop for s in start-times and st in states
			    for mult = (get-time-mult st)
			    collect s collect mult))
      (values start-times
	      (get-indisp (start-time tl))
	      (apply #'sections rhythm)
	      (apply #'sections srt)
	      (combine-envelopes envelopes durs)
	      (apply #'sections time-mult)))))

;; ** notation

;; *** tape states

;;; arbitrary

;; **** tape-get-static
(defun tape-get-static (dur)
  (values `(,dur) '(c4) '()))

;; **** tape-get-static-rhythm
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

;; **** tape-get-morphing-rhythm
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

;; **** tape-get-changing-rhythmic-speed
(defun tape-get-changing-rhythmic-speed (dur index)
  (values `(,dur) '(c4) `((,index "changing-rhythimc-speed"))))

;; **** tape-get-changing-pulse-speed
(defun tape-get-changing-pulse-speed (dur index)
  (values `(,dur) '(c4) `((,index "changing-pulse-speed"))))

;; **** tape-get-changing-timbres
(defun tape-get-changing-timbres (dur index)
  (values `(,dur) '(c4) `((,index "tape-get-changing-timbres"))))

;; **** tape-get-drifting-rhythmic-speeds
(defun tape-get-drifting-rhythmic-speeds (dur index)
  (values `(,dur) '(c4) `((,index "tape-get-drifting-rhythmic-speeds"))))

;; **** tape-get-drifting-pulse-speed
(defun tape-get-drifting-pulse-speed (dur index)
  (values `(,dur) '(c4) `((,index "tape-get-drifting-pulse-speed"))))

;; *** ins states

;; **** ins-get-rest
(defun ins-get-rest (dur)
  (values `(,dur) '(nil) '() 1))

;; **** ins-get-static
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

;; **** ins-get-static-rhythm
(defun ins-get-static-rhythm (instrument dur index sum chord start-time)
  (if (equal instrument 'percussion)
      (perc-get-static-rhythm dur index sum chord start-time)
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
	  ;; get durations, pitches and marks
	  (loop for i from 0 below (round (* (/ md 4) divisor))
		and last = 0 then el
		for el = (nth-mod i active-ls)
		with ind = (if (= st 0) 0 1)
		with cnt = 0
		with chord-cnt = 2
		do (when (= el 0)
		     (when (= last 1)
		       (push (* cnt (/ 4 divisor)) new-durs)
		       (push spitch pitches)
		       ;; the dynamic marks somehow create a mess...
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
			  (push nil pitches))))
	(values new-durs pitches (reverse marks)))))

;; **** perc-get-static-rhythm
(defun perc-get-static-rhythm (dur index sum chord start-time)
  ;;(declare (ignore index))
  (let* ((new-durs '())
	 (pitches '())
	 (nr 0)
	 (thrsld 1/4)
	 (indisp (funcall (get-indisp start-time) 'time sum))
	 (spitch (list (note-for-ins 'percussion (nth-mod 0 chord))
		       (note-for-ins 'percussion (nth-mod 2 chord)))))
    (declare (ignore spitch))
    (multiple-value-bind (st md nd) (get-st-md-nd sum dur)
      ;; set number of notes
      (unless (= 0 st) (incf nr))
      (unless (= 0 nd) (incf nr))
      (incf nr (* (/ md 4) 16))
      ;; get durations and pitches
      (loop for i from 0 below nr
	    for ind = (/ (1+ (funcall indisp (mod (/ i 13) 1))))
	    for note = (+ (if (= 0 (nth i (fibonacci-transition nr))) 0 12)
			  (if (> ind thrsld) 61 60))
	    for double = (and (find ind '(1/4 1/3 1/7 1/8)) ;; (= (mod i 7) 0)
			      (not (or (= i (1- nr)) (= i 0))))
	    do (push (cond ((and (= i 0) (not (= 0 st))) st)
			   ((and (= i (1- nr)) (not (= 0 nd))) nd)
			   (t (if double 1/8 1/4)))
		     new-durs)
	       (push (if (or (= i (1- nr)) (= 0 i)) nil note) pitches)
	       (when double (push note pitches) (push 1/8 new-durs)))
      (values (reverse new-durs) (reverse pitches) `((,index f))))))

;; **** ins-get-morphing-rhythms
;;; like ins-get-static-rhythm but morph rhythm (indisp-fun).
(defun ins-get-morphing-rhythms (instrument dur index sum chord start-time)
  (declare (ignore index))
  (let* ((new-durs '())
	 (pitches '())
	 (nr 0)
	 (spitch (note-for-ins instrument (nth 1 chord)))
	 (spitch2 nil)
	 (thrsld 1/7)
	 (indisp1 (funcall (get-indisp start-time) 'time sum))
	 (indisp2 (funcall (get-indisp start-time) 'time (+ sum dur 1)))
	 (pitches1 '())
	 (pitches2 '())
	 (ptrn1 '())
	 (ptrn2 '())
	 (morph '())
	 (i-div 13)
	 ;; modify this for different pulses
	 ;; tempo 60: 1(q), 4 (s), 6 (ts)
	 ;; tepmo 90: 6 (s), 9/2 (triplets), 15/2 (quintuplet)
	 (divisor 4)) ;; dividing 1 second (q=60) into this many notes
    (multiple-value-bind (st md nd) (get-st-md-nd sum dur)
      ;; custom stuff:
      (case instrument
	(tuba (setf spitch nil))
	(double-bass (setf spitch 'b0 thrsld 0))
	(violin-1 (setf thrsld 1/3))	; i-div 6.5))
	(violin-2 (setf thrsld 1/4))	; i-div 6.5))
	(viola (setf thrsld 1/6))	; i-div 6.5))
	(cello (setf thrsld 1/8))	; i-div 6.5))
	(flute (setf thrsld 1/5))    
	(oboe (setf thrsld 1/2))	
	(b-clarinet (setf thrsld 1/7))       
	(bassoon (setf thrsld 1/9))
	(c-trumpet (setf thrsld 1/8 i-div 8))    
	(french-horn (setf thrsld 1/6 i-div 8))	
	(bass-trombone (setf thrsld 1/5 i-div 8))       
	(tuba (setf thrsld 1/3 i-div 8))
	(percussion (setf spitch2 (note-for-ins instrument (nth 2 chord))
			  thrsld 1/2)))
      ;; set number of notes
      (unless (= 0 st) (incf nr))
      (unless (= 0 nd) (incf nr))
      (incf nr (* md divisor)) ;; war (* (/ md 4) 16)
      ;; get durations and different pitch lists
      (loop for i from 0 below nr
	    for ind1 = (/ (1+ (funcall indisp1 (mod (/ i i-div) 1))))
	    for ind2 = (/ (1+ (funcall indisp2 (mod (/ i i-div) 1))))
	    do (push (cond ((and (= i 0) (not (= 0 st))) st)
			   ((and (= i (1- nr)) (not (= 0 nd))) nd)
			   (t (/ 1 divisor))) ;; .25
		     new-durs)
	       (push (if (>= ind1 thrsld) spitch spitch2) pitches1)
	       (push (if (>= ind2 thrsld) spitch spitch2) pitches2))
      ;; make "patterns" with only 1s, because we want the indices
      (setf ptrn1 (make-list (length pitches1) :initial-element 1)
	    ptrn2 (make-list (length pitches2) :initial-element 1))
      ;; morph patterns and get indices
      (setf morph (morph-patterns `(,(reverse ptrn1) ,(reverse ptrn2))
				  0 nil nil (length ptrn1) t))
      ;; collect pitches from indices
      (setf pitches
	    (loop for m in morph
		  collect (nth (cadr m) (if (= 0 (car m)) pitches1 pitches2))))
      (values (reverse new-durs) pitches '()))))

;; **** ins-get-changing-timbres
(defun ins-get-changing-timbres (instrument dur chord)
  (case instrument
    (t (values `(,dur) `(,(note-for-ins instrument (nth 2 chord))) '()))))

;; **** ins-get-isorhythmic-rhythms
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

;; **** ins-get-converging-pitches
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

;; **** ins-get-drifting-pitches
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

;; **** ins-get-drifting-metres
#|
(lists-to-midi (gen-melodic-line 16 55 '(30 65) '(0 0  .2 0 1 7) '(0 0 .2 0  1 1)) '(1) (loop for i from 0 below 16 collect i) :file "/E/code/ensemble/test4.mid")
|#
(defun ins-get-drifting-metres (instrument dur index sum chord)
  (let* ((new-durs '())
	 (pitches '())
	 (marks '())
	 spitch
	 (divisor 2)
	 (ins (my-get-standard-ins instrument))
	 (ambitus (list (midi-note (lowest-sounding ins))
			(- (midi-note (highest-sounding ins)) 5)))
	 (diss-env '(0 0 .3 0  1 7))
	 (var-env '(0 0  .3 0  1 1))
	 (nr 0))
    (multiple-value-bind (st md nd) (get-st-md-nd sum dur)
      ;; set number of notes
      (unless (= 0 st) (incf nr))
      (unless (= 0 nd) (incf nr))
      ;; custom stuff:
      (case instrument
	((violin-2 violin-1 flute)
	 (setf spitch (note-for-ins instrument (nth 0 chord)) divisor 3)
	 (incf nr (* (/ md 4) divisor)))
	((viola b-flat-clarinet)
	 (setf spitch (note-for-ins instrument (nth 0 chord)) divisor 4)
	 (incf nr (* (/ md 4) divisor)))
	((cello bassoon oboe)
	 (setf spitch (note-for-ins instrument (nth 0 chord)) divisor 5)
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

;; **** interpret tape for notation
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

;;; **** interpret instruments for notation
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
	 (start-time (start-time il))
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
		       (2 (ins-get-static-rhythm instrument dur index sum chord start-time))
		       (3 (ins-get-morphing-rhythms instrument dur index sum chord start-time))
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

;; *** interpret-layer
(defmethod interpret-layer ((ml minute-layer))
  (loop for ins in (instruments ml)
	collect (interpret-layer-by-instrument ml ins)))

;; *** interpret-minutes-by-instrument
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

;; *** interpret-minutes
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

;; EOF interpretation.lsp
