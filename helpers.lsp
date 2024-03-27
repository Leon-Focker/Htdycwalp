;; * helpers.lsp

(in-package :ly)

;; *** nth-mod
(defun nth-mod (n rthm-ls)
  (nth (mod n (length rthm-ls)) rthm-ls))

;; *** sc-intern
(defun sc-intern (symbol)
  (if (stringp symbol) symbol (intern (string symbol) :sc)))

;; *** split-list
;;; get a list and a list of numbers which stand for the number of elements in
;;; each sublist of the result. (very lazy implementation)
(defun split-list (ls div-cnt)
  (loop while ls for nr in div-cnt
	collect (loop repeat nr collect (pop ls))))

;; *** distribute-to-minute-layers
;;; get list of something and distribute its contents according to a second list
;;; each number in the second list specifies how many elements to give to the
;;; according layer. slot-name specifies the slot in which to put the lists.
(defun distribute-to-minute-layers (content distribution list-of-minutes
				    layer-number slot-name)
  (let ((layers (get-related-minute-layers list-of-minutes layer-number)))
    (unless (= (length layers) (length distribution))
      (error "unqueal amount of relevant layers and elements in distribution: ~
              ~a ~a" (length layers) (length distribution)))
    (loop for elements in (split-list content distribution)
	  and layer in layers
	  do (setf (slot-value layer slot-name)  elements)))
  t)

;; *** distribute-divs
;;; get list of division-ratios and numbers-of-division and distribute the
;;; division-ratios to the layers of list-of-minutes with the specified number.
(defun distribute-divs (division-ratios number-of-divisions list-of-minutes
			layer-number)
  (distribute-to-minute-layers division-ratios number-of-divisions
			       list-of-minutes layer-number 'division-ratios))

;; *** distribute-states
;;; same as distribute-divs but for states
(defun distribute-states (list-of-states distribution list-of-minutes
			  layer-number)
  (distribute-to-minute-layers list-of-states distribution
			       list-of-minutes layer-number 'states))

;; *** distribute-dynamics
;;; same as distribute-divs but for states
(defun distribute-dynamics (list-of-dynamics distribution list-of-minutes
			    layer-number)
  (distribute-to-minute-layers list-of-dynamics distribution
			       list-of-minutes layer-number 'dynamics))

;; *** distribute-chords
;;; same as distribute-divs but for chords
(defun distribute-chords (list-of-chords distribution list-of-minutes
			    layer-number)
  (distribute-to-minute-layers list-of-chords distribution
			       list-of-minutes layer-number 'chords))

;; *** get-lsim (get-layer-slot-in-minutes)
(defun get-lsim (ls-of-minutes minute layer-number slot-name)
  "get-layer-slot-in-minutes"
  (let ((minute (nth minute ls-of-minutes)))
    (slot-value (car (member layer-number (layers minute) :key 'number))
		slot-name)))

;; *** set-lsim (set-layer-slot-in-minutes)
(defun set-lsim (ls-of-minutes minute layer-number
		 slot-name new-value)
  "set-layer-slot-in-minutes"
  (let ((minute (nth minute ls-of-minutes)))
    (setf (slot-value (car (member layer-number (layers minute) :key 'number))
		      slot-name)
	  new-value)))

;; *** get-st-md-nd
;;; gets a time and a duration. Assumes bar = '(4 4), 60bpm.
;;; returns 3 values:
;;;  - the duration at the beginning, that is needed to fill the first bar
;;;  - the duration in the middle that fills entire bars
;;;  - the duration at the end, that doesn't fill another bar
(defun get-st-md-nd (time dur)
  (let* ((remain (mod (- 4 (mod time 4)) 4))  ; time until first bar is full
	 (st (if (> remain dur) dur remain)) ; time in first, not full, bar
	 (md (* (floor (- dur st) 4) 4))     ; time within full bars
	 (nd (- dur st md)))                 ; time in last, not full, bar
    (values st md nd)))

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

;; *** gen-chords-from-lines
(defun gen-chords-from-lines (length list-of-starting-notes list-of-ambitus
			      dissonance-env variation-env list-of-offsets)
  (let ((result (ml '() length)))
    (loop for st in list-of-starting-notes and amb in list-of-ambitus
	  and ofs in list-of-offsets and i = 0
	  do (dolist (note (gen-melodic-line length st amb dissonance-env
					     variation-env ofs))
	       (push (midi-to-note note) (nth i result))
	       (incf i)))
    result))

;; *** sections
;;; get pairs of times and values, returns a function that receives a time
;;; argument. That function will return the value which time is smaller than
;;; the functions time argument, when there is no later time for which this is
;;; true.
#|
(loop for i from 0 to 10 collect
	  (funcall (sections 0 1 5 ":)" 6.5 7) i))
=> '(1 1 1 1 1 ":)" ":)" 7 7 7 7)
|#
(defun sections (&rest times-and-values)
  (unless (= 0 (mod (length times-and-values) 2))
    (error "uneven number of arguments in #'sections: ~a" times-and-values))
  (lambda (time)
    (loop for x in times-and-values by #'cddr
	  and y in (cdr times-and-values) by #'cddr
	  with result = nil
	  do (if (>= time x) (setf result y) (return result))
	  finally (return result))))

;; *** section-val
#|
(loop for i from 0 to 10 collect
	  (section-val i 0 1 5 ":)" 6.5 7))
=> '(1 1 1 1 1 ":)" ":)" 7 7 7 7)
|#
(defun section-val (time &rest times-and-values)
  (funcall (apply #'sections times-and-values) time))

;; *** pad-lists
;;; if one list is shorter than the other, loop it until is has the same length
(defun pad-lists (ls1 ls2)
  (if (> (length ls1) (length ls2))
      (setf ls2 (loop for i from 0 below (length ls1)
		      collect (nth-mod i ls2)))
      (setf ls1 (loop for i from 0 below (length ls2)
		      collect (nth-mod i ls1))))
  (values ls1 ls2))

;; *** note-for-ins
(defun note-for-ins (instrument chord)
  (when (and (>= (length (string instrument)) 6)
	     (string= (subseq (string instrument) 0 6) "VIOLIN"))
    (setf instrument 'violin))
  (let* ((ins (get-standard-ins instrument))
	 (min (midi-note (lowest-sounding ins)))
	 (max (midi-note (highest-sounding ins)))
	 (new-chord '()))
    (setf new-chord
	  (loop for i in chord when (<= min (note-to-midi i) max) collect i))
    (if new-chord (nth (random (length new-chord)) new-chord) nil)))

;; *** get-dyns
;;; first and second argument are dynamics before and after crescendo/diminuendo
;;; third arugment is dynamic at start of crescendo/diminuendo, might be nil.
;;; fourth argument ist wheter to od crescendo (t) or diminuendo (nil)
(defun get-dyns (dyn-before dyn-after dyn-current &optional (crescendo t))
  (setf dyn-before (intern (string dyn-before) :ly)
	dyn-after (intern (string dyn-after) :ly)
	dyn-current (intern (string dyn-current) :ly))
  (let* ((dyns (if crescendo '(ppp pp p mp mf f ff fff)
		   '(fff ff f mf mp p pp ppp)))
	 (len (length dyns))
	 new-start
	 new-end)
    (flet ((get-target (x &optional (div 2))
	     (let ((i (- len (length (member x dyns)))))
	       (nth (max (min (+ (floor (- len i) div) i) (1- len)) 0) dyns)))
	   (get-some-start (x)
	     (let ((i (- len (length (member x dyns)))))
	       (nth (mod (floor (* i 5.5)) (1- len)) dyns))))
      (if dyn-current
	  (progn (setf new-start dyn-current
		       new-end (get-target new-start)))
	  (progn (setf new-start (get-some-start dyn-before)
		       new-end (get-target new-start 1.5))))
      (values (sc-intern new-start) (sc-intern new-end)))))

;; *** get-dynamic
;;; get the dynamic mark for an event
(defun get-dynamic (event)
  (loop for mark in (marks event)
	for m = (intern (string mark) :sc)
	when (is-dynamic m) do (return m)))

;; *** get-cresc-beg
(defun get-cresc-beg (event)
  (loop for mark in (marks event)
	for m = (intern (string mark) :ly)
	when (find m '(cresc-beg dim-beg)) do (return m)))

;; *** remove-mark
(defun remove-mark (event mark)
  (setf (marks event)
	(remove mark (marks event))))

;; *** dyn=
;;; compare dynamics.
;;; when diff = 1 ->  pp == p, pp == ppp, pp == pp
;;; when diff = 0 ->  pp =/= p, pp =/= ppp, pp == pp
(defun dyn= (dynx dyny &optional (diff 1))
  (setf dynx (intern (string dynx) :ly)
	dyny (intern (string dyny) :ly))
  (let* ((dyns '(pppp ppp pp p mp mf f ff fff ffff))
	 (len (length dyns))
	 ix
	 iy)
    (setf ix (- len (length (member dynx dyns)))
	  iy (- len (length (member dyny dyns))))
    (<= (abs (- ix iy)) diff)))

;; *** add-crescendo-dynamics
;;; if optional-arg crescendo is nil, do diminuendos.
(defun add-crescendo-dynamics (event-list &optional (crescendo t))
  (let ((len (length event-list))
	(cresc-beg (sc-intern (if crescendo 'cresc-beg 'dim-beg)))
	(cresc-end (sc-intern (if crescendo 'cresc-end 'dim-end)))	
	(begs '())
	(ends '()))
    ;; find beginnings and ends of crescendo-marks
    (loop for i from 0 and e in event-list
          do (when (find cresc-beg (marks e)) (push i begs))
	     (when (find cresc-end (marks e)) (push i ends))
	  finally (setf begs (reverse begs)
			ends (reverse ends)))
    ;; find dynamics before and after each crescendo and set new ones
    (loop for beg in begs and end in ends
	  for dyn-st = (get-dynamic (nth beg event-list))
	  for dyn-nd = (get-dynamic (nth end event-list))
	  for cre-st = (get-cresc-beg (nth end event-list))
	  for dyn-before = (loop for i from beg downto 0
				 for dyn = (get-dynamic (nth i event-list))
				 when dyn do (return dyn)
				   finally (return 'p))
	  for dyn-after = (loop for i from end below len
				for dyn = (get-dynamic (nth i event-list))
				when dyn do (return dyn)
				  finally (return 'p))
	  do (multiple-value-bind (st nd)
		 (get-dyns dyn-before dyn-after dyn-st crescendo)
	       (if (dyn= st (get-dynamic (nth (max (1- beg) 0) event-list)))
		   ;; move crescendo start:
		   (progn (remove-mark (nth beg event-list) cresc-beg)
			  (push cresc-beg (marks (nth (max (1- beg) 0)
						      event-list))))
		   ;; don't move:
		   (unless dyn-st (add-mark (nth beg event-list) st)))
	       (if (or (and dyn-nd (not (dyn= dyn-nd nd)))
		       (and cre-st (not (dyn= dyn-nd nd)))
		       (is-rest (nth end event-list)))
		   ;; move crescendo end:
		   (progn (remove-mark (nth end event-list) cresc-end)
			  (add-mark (nth (max (1- end) 0) event-list) nd)
			  (push cresc-end (marks (nth (max (1- end) 0)
						      event-list))))
		   ;; move end but not dynamic:
		   (progn #+nil(remove-mark (nth end event-list) cresc-end)
			  (add-mark (nth end event-list) nd)
			  #+nil(push cresc-end (marks (nth (max (1- end) 0)
						      event-list)))))))
    event-list))

;; *** env-fun1
(defun env-fun1 (breakpoint &optional (exponent 0.3))
  (let ((bp (max 0 (min 100 (round breakpoint)))))
    (append
     (if (= bp 0) '(0 1)
	 (loop for i from 0 to bp
	    collect i collect (expt (/ i bp) exponent)))
     (loop for i from (1+ bp) to 100
	   collect i collect (expt (/ (- 100 i) (- 100 bp)) exponent)))))

;; ** indispensability

;; *** apply-indispensability
(defun apply-indispensability (beats rhythms indispensability-function)
  ""
  (let* ((duration (apply #'+ beats)))
    (loop for rthm in rhythms
       for sum = 0 then (+ sum rthm)
       with beat-sum = 0
       with last-beat-i = 0
       for progress = (rationalize (/ sum duration))
       for current-beat-i = (decider progress beats)
       for current-beat = (nth current-beat-i beats)
       with position = 0
       until (>= sum duration)
       do (unless (= current-beat-i last-beat-i)
	    (incf beat-sum (nth last-beat-i beats))
	    (setf last-beat-i current-beat-i))
       ;; position in current beat:
	 (setf position (- sum beat-sum))
       collect (funcall indispensability-function
			(rationalize (/ position current-beat))))))

;; ** clm-macros

(defmacro wsound (name &body body)
`(with-sound (:header-type clm::mus-riff :samplingq-rate 48000
	      :output (format nil "~a~a~a" +ens-src-dir+ ,name ".wav")
	      :channels 2 :play nil :scaled-to 0.98
	      :force-recomputation nil)
   ,@body))

;; ** notation

;; *** update-transposing-pitches
;;; transpose events opposite of what set-written-pitch-or-chord would do,
;;; so that musescore then "corrects" it.
(defun update-transposing-pitches (events instrument)
  (loop for e in events
	with trans = (transposition-semitones
		      (get-data
		       instrument
		       sc::+slippery-chicken-standard-instrument-palette+))
	for pitch-or-chord = (pitch-or-chord e)
	for is-rest = (is-rest e)
	do (when (equal instrument 'double-bass)
	     (setf trans -12))
	   (unless is-rest
	     (set-pitch-or-chord e (transpose pitch-or-chord (- trans))))))

;; *** split-into-simpler-ratios
;;; returns a list
;;; very simple, could be imprived.
(defun split-into-simpler-ratios (num)
  (setf num (rationalize num))
  (if (and (> (numerator num) 1) (= 0 (mod (denominator num) 2)))
      (let* ((simpler (/ (floor (numerator num) 2) (/ (denominator num) 2))))
	(append (split-into-simpler-ratios simpler)
		(split-into-simpler-ratios (- num simpler))))
      (list num)))

;; *** quantise-for-notation
;;; quantising to some hopefully notatable durations
;;; returns a list of durations which add up to a quantised version of the
;;; original duration. This is a very simple approach, could be improved.
;;; This basically uses the #'nearest function to quantise to a value that
;;; is a normal note, dotted note or tuplet (3 and 5).
(let* ((steps
	 (append 
	  (loop for i from 1 to 6 collect (/ 1 (expt 2 i)))
	  (loop for i from 0 to 3 collect (/ 1 (* 3 (expt 2 i))))
	  (loop for i from 0 to 2 collect (/ (* 5 (expt 2 i))))))
       (possible-divisions
	 (sort
	  (remove-duplicates
	   (loop for i in steps
		 append (loop for time from 0 to 1 by i collect time)))
	  #'<)))
  ;; defun...
  (defun quantise-for-notation (duration &key (tempo 60))
    (unless (> duration 0)
      (error "duration should be greater than 0 but is: ~a" duration))
    (let* ((tempo-mult (/ tempo 240))
	   ;; normalise to tempo = 240 (1 second per 4/4 bar)
	   (normalised-duration (* duration tempo-mult))
	   (full-bars 0)
	   (quantised 0))
      (loop while (> normalised-duration 1)
	    do (incf full-bars)
	       (incf normalised-duration -1))
      (setf quantised (nearest normalised-duration possible-divisions))
      (loop for i in (append (ml 1 full-bars)
			     (split-into-simpler-ratios quantised))
	    unless (= 0 i) collect (/ i tempo-mult)))))

;; *** split-durations-for-bars
;;; returns a list of sublists of durations. These durations can be fit into
;;; bars at tempo but the sublists mark how the original durations belong
;;; together.
(defun split-durations-for-bars (duration-list
				  &optional (time-sig '(4 4)) (tempo 60))
  (let* ((result '())
	 ;; duration of a bar in seconds:
	 (duration-of-bar (* 60 (/ 4 (cadr time-sig) tempo) (car time-sig))))
    (setf duration-of-bar (rationalize duration-of-bar))
    (loop while duration-list
	  with current-sum = 0
	  with was-split
	  for next-dur = (pop duration-list)
	  for next-sum = (+ current-sum next-dur)
	  do (cond ((equal-within-tolerance next-sum duration-of-bar 0.001)
		    (if was-split
			(push (append (pop result) `(,next-dur)) result)
			(push `(,next-dur) result))
		    (setf current-sum 0 was-split nil))
		   ((< next-sum duration-of-bar)
		    (if was-split
			(push (append (pop result) `(,next-dur)) result)
			(push `(,next-dur) result))
		    (setf current-sum next-sum was-split nil))
		   ((> next-sum duration-of-bar)
		    (let ((diff (rationalize (- duration-of-bar current-sum))))
		      (if was-split
			  (push (append (pop result) `(,diff)) result)
			  (push `(,diff) result))
		      (push (rationalize (- next-dur diff)) duration-list)
		      (setf current-sum 0 was-split t)))))
    (reverse result)))

;; *** quantise-sublists-for-notation
;;; quantise-for-notation when already split for bars
(defun quantise-sublists-for-notation (list-of-split-durations &key (tempo 60))
  (loop for bar in list-of-split-durations
	collect (loop for dur in bar
		      append (quantise-for-notation dur :tempo tempo))))

;; *** durations-and-pitches-to-events
;;; Arguments:
;;; - a list of durations in seconds.
;;; - a list of pitches, where pitches in a sublist are a chord and nil is
;;; a rest: '(e5 (c5 g5) nil). 
;;; The number of notes to be notated is always the number of elements in
;;; the duration-list. If pitch-list is short, it is loooped.
;;; Returns:
;;; a list of events that are split, so they fit in a bar with the given time 
;;; signature in the given tempo.
;;; Because events might have to be split into several events that are tied to
;;; each other, the original index of a pitch might not be the same index for
;;; this pitch in the resulting event list. Thus a second return value is a list
;;; with all indices of the original elements in the pitch-list (sublists count
;;; as one).
(defun durations-and-pitches-to-events
    (duration-list pitch-list &optional (time-sig '(4 4)) (tempo 60))
  (unless (>= (length pitch-list) (length duration-list))
    (setf pitch-list
	  (loop for i from 0 below (length duration-list)
		collect (nth-mod i pitch-list))))
  (setf duration-list 
	(quantise-sublists-for-notation
	 (split-durations-for-bars duration-list time-sig tempo)))
  (let* ((diff 0)
	 (result '())
	 (indices-of-attacks '())
	 (duration-of-bar (* 60 (/ 4 (cadr time-sig) tempo) (car time-sig))))
    (setf indices-of-attacks
	  (loop for tied in duration-list with i = 0
		collect i do (incf i (length tied))))
    (setf diff (- duration-of-bar
		  (mod (apply #'+ (flatten duration-list)) duration-of-bar)))
    (when (= diff duration-of-bar) (setf diff 0))
    (setf result
	  (loop for tied in duration-list and pitch in pitch-list
		append (loop for dur in tied and k from 1 with len = (length tied)
			     for is-rest = (not pitch)
			     for tied-from = (unless is-rest (< k len))
			     for tied-to = (unless is-rest (> k 0))
			     do (when (numberp pitch)
				  (setf pitch (midi-to-note pitch)))
			     collect (make-event pitch dur
						 :duration t :tempo tempo
						 :is-tied-from tied-from
						 :is-tied-to tied-to
						 :is-rest is-rest))))
    (unless (equal-within-tolerance diff 0 .01)
      (let ((qdiff (quantise-for-notation diff :tempo tempo)))
	(setf result
	      (append result
		      (loop for diff in qdiff
			    collect (make-event nil diff :is-rest t :duration t
							 :tempo tempo))))))
    (loop for e in result
	  do (when (= -1 (letter-value e))
	       (setf (letter-value e)
		     (round (/ 4 (duration e)))))) ; ? crude aprox
    (values result indices-of-attacks)))

;; *** lists-to-xml
;;; Arguments:
;;; -lists should be a list of lists. These sublists should have this format:
;;; '(player instrument (list-of-durations) (list-of-pitches)
;;;   ((index mark1) (index mark2)))
;;; the list of marks is optional. The index refers to the index of the pitch in
;;; the pitch list at which to put the mark. Marks can be any slippery chicken
;;; mark - for example just a string with text.
;;; Note, that if one of duration or pitch list is shorter than the other, the
;;; shorter one will be looped until it has the same length as the longer one.
;;; -file should be the path to and the filename for the resulting .xml file.
;;;  If file is nil, one will be automatically generated from the load-path of
;;;  this file.
;;;
;;; This is essentially an implementation of this:
;;; https://github.com/mdedwards/slippery-chicken/wiki/How-can-I-'roll-my-own'-slippery-chicken%3F
;;;
;;; EXAMPLES
#|
;; simple example using one instrument:
(lists-to-xml '((player-one piano (4 4 6 2) (e5 nil d5 a5) ((0 "schneller") (2 "pp")))) 
	      nil :tempo 120)

;; same but change time signature:
(lists-to-xml '((player-one piano (4 4 6 2) (e5 nil d5 a5) ((0 "schneller") (2 "pp"))))
	      nil :tempo 120 :time-sig '(3 4))

;; more interesting time-sig:
(lists-to-xml '((player-two flute (6) ((e5 a5)) ((0 "schneller") (2 "pp"))))
	      nil :time-sig '(5 8))

;; complex example, which will produce a corrupted file - because I would rathe
;; have it produce corrupt notation than fail:
(lists-to-xml '((player-one piano (3 3 3.3 4.83 3) (e5 nil d5 a5) ((0 "schneller") (2 "pp")))
		(player-two flute (6) ((e5 a5)) ((0 "schneller") (2 "pp")))
		(player-three bassoon (2.6) (e5) ((0 "schneller") (2 "pp"))))
	      nil :tempo 70 :time-sig '(5 8))
|#
(let ((default-dir (path-from-same-dir)))
  (defun lists-to-xml (lists file &key (time-sig '(4 4)) (tempo 60)
				    title composer)
    ;; maybe set file and title
    (unless title
      (setf title (if file (pathname-name file) "untitled")))
    (unless file
      (setf file (format nil "~a~a~a~a"
			 default-dir "lists-to-xml-" title ".xml")))
    ;; sanity checks:
    (unless (and (listp lists) (loop for ls in lists always (listp ls)))
      (error "lists in lists-to-xml seems to be malformed"))
    (let* (sc list-of-list-of-bars)
      (loop for i in lists
	    ;; split durations so they fit into bars and parse to (tied) events
	    do (multiple-value-bind (events attacks-indices)
		   (durations-and-pitches-to-events
		    ;; instead of giving the tempo argument to
		    ;; #'durations-and-pitches-to-events I normalize to
		    ;; 60 because events do that anyways.
		    (loop for k in (third i) collect (* k (/ tempo 60)))
		    (fourth i) time-sig)
		 ;; loop through transposing instruments and change written
		 ;; this only has to be done when exporting xml (for musescore),
		 ;; as the in-c argument i missing.
		 (when (find (second i) '(b-flat-clarinet double-bass))
		   (update-transposing-pitches events (second i)))
		 ;; add marks to events (using a list of indices of the events
		 ;; before they were split into bars).
		 (loop for m in (fifth i) 
		       when (< (car m) (length attacks-indices))
			 do (add-mark (nth (nth (car m) attacks-indices) events)
			        (sc-intern (cadr m))))
		 ;; arbitrarily set dynamics before and after crescendo
		 (setf events (add-crescendo-dynamics events))
		 (setf events (add-crescendo-dynamics events nil))
		 ;; generate bars
		 (push (loop while events
			     for bar = (make-rthm-seq-bar `(,time-sig))
			     for ate = (fill-with-rhythms bar events)
			     do (setf events (when ate (nthcdr ate events)))
				(consolidate-rests bar :auto-tuplets t)
			     collect bar)
		       list-of-list-of-bars)))
      ;; pad list-of-bars that are shorter than others in list-of-list-of-bars
      (let ((list-of-bars-len (mapcar #'length list-of-list-of-bars)))
	(unless (apply #'= list-of-bars-len)
	  (let* ((max-len (apply #'max list-of-bars-len))
		 (empty-bar (make-rthm-seq-bar `(,time-sig))))
	    (fill-with-rhythms
	     empty-bar `(,(make-event nil (apply #'/ time-sig)
				      :duration t :is-rest t)))
	    (setf list-of-list-of-bars
		  (loop for bars in list-of-list-of-bars
			collect (append bars
					(ml empty-bar
					    (- max-len (length bars)))))))))
      ;; put them in a slippery-chicken object:
      (loop for bars in (reverse list-of-list-of-bars) and i in lists
	    do (setf sc (bars-to-sc bars :sc sc :player (first i)
					 :instrument (second i) :tempo tempo)))
      ;; set composer and title
      (setf (composer sc) composer (title sc) title)
      (check-ties sc t)
      ;; !! this (every 15 bars) shouldn't be hardcoded like this:
      (loop for i from 1 and bar from 1 to (num-bars sc) by 16
	    do (set-rehearsal-letter sc (if (= bar 1) 2 bar) i))
      ;; call write-xml on sc-object
      (write-xml sc :file file))))

;; some dirty work (replacing names in staffs):
(setf (staff-short-name
       (get-data 'b-flat-clarinet
		 sc::+slippery-chicken-standard-instrument-palette+))
      "cl.")

;; EOF helpers.lsp
