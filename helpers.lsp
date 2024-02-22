;; * helpers.lsp

(in-package :ly)

;; *** nth-mod
(defun nth-mod (n rthm-ls)
  (nth (mod n (length rthm-ls)) rthm-ls))

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

;; *** durations-and-pitches-to-events
;;; Arguments: a list of durations in seconds and of pitches, where pitches in
;;; a sublist are a chord and nil is a rest: '(e5 (c5 g5) nil). If one list is
;;; shorter than the other, the shorter one is looped so both have the same
;;; length.
;;; Returns a list of events that are split, so they fit in a bar with the given
;;; time signature in the given tempo.
;;; Because events might have to be split into several events that are tied to
;;; each other, the original index of a pitch might not be the same index for
;;; this pitch in the resulting event list. Thus a second return value is a list
;;; with all indices of the original elements in the pitch-list.
(defun durations-and-pitches-to-events (duration-list pitch-list
					&optional (time-sig '(4 4)) (tempo 60))
  ;; loop the shorter list to get the same length for both:
  (multiple-value-bind (ls1 ls2) (pad-lists duration-list pitch-list)
    (setf duration-list ls1 pitch-list ls2))
  (let* ((events '())
	 (original-events-indices '())
	 ;; duration of a bar in seconds:
	 (duration-of-bar (rationalize (* 60 (/ 4 (cadr time-sig) tempo)
					  (car time-sig)))))
    (loop while (and duration-list pitch-list)
	  with is-tied
	  with current-sum = 0
	  for i from 0
	  for is-rest = (not (car pitch-list)) ;; nil means rest
	  for next-dur = (rationalize (pop duration-list))
	  for next-sum = (+ current-sum next-dur)
	  do (unless is-tied (push i original-events-indices))
	     ;; if bar is full now
	     (cond ((equal-within-tolerance next-sum duration-of-bar 0.001)
		    (push (make-event (pop pitch-list) next-dur
				      :is-tied-to is-tied :duration t
				      :is-rest is-rest :tempo tempo)
			  events)
		    (setf current-sum 0 is-tied nil))
		   ;; if bar is not full now
		   ((< next-sum duration-of-bar)
		    (push (make-event (pop pitch-list) next-dur
				      :is-tied-to is-tied :duration t
				      :is-rest is-rest :tempo tempo)
			  events)
		    (setf current-sum next-sum is-tied nil))
		   ;; if duration is too long for this bar
		   ((> next-sum duration-of-bar)
		    (let ((diff (- duration-of-bar current-sum)))
		      (push (make-event (car pitch-list) diff
					:is-tied-to is-tied :is-tied-from t
					:duration t :is-rest is-rest
					:tempo tempo)
			    events)
		      (push (- next-dur diff) duration-list))
		    (setf current-sum 0 is-tied (unless is-rest t))))
	     ;; fill last bar with rests until full
	  finally (unless (= 0 current-sum)
		    (push (make-event nil (- duration-of-bar current-sum)
				      :duration t :is-rest t :tempo tempo)
			  events)))
    (loop for e in events
	  do (when (= -1 (undotted-value e))
	       (setf (undotted-value e) 1)))
    ;; return events and original indices
    (values (reverse events)
	    (reverse original-events-indices))))

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
;;; -file should be the path to and the filename for the resulting .xml file
;;;
;;; This is essentially an implementation of this:
;;; https://github.com/mdedwards/slippery-chicken/wiki/How-can-I-'roll-my-own'-slippery-chicken%3F
;;;
;;; EXAMPLES
#|
;; simple example using one instrument:
(lists-to-xml '((player-one piano (4 4 6 2) (e5 nil d5 a5) ((0 "schneller") (2 "pp")))) 
	      "/E/code/ensemble/test.xml" :tempo 120)

;; same but change time signature:
(lists-to-xml '((player-one piano (4 4 6 2 1) (e5 nil d5 a5) ((0 "schneller") (2 "pp"))))
	      "/E/code/ensemble/test.xml" :tempo 120 :time-sig '(3 4))

;; complex example:
(lists-to-xml '((player-one piano (3 3 3 4 3) (e5) )
		(player-two flute (6) ((e5 a5)) ((0 "schneller") (2 "pp")))
		(player-three bassoon (2) (e5) ((0 "schneller") (2 "pp"))))
	      "/E/code/ensemble/test1.xml" :tempo 70 :time-sig '(5 8))
|#
(defun lists-to-xml (lists file &key (time-sig '(4 4)) (tempo 60)
				  (title (pathname-name file)) composer)
  (unless (and (listp lists) (loop for ls in lists always (listp ls)))
    (error "lists in lists-to-xml seems to be malformed"))
  (let* (sc list-of-list-of-bars)
    (loop for i in lists
	  ;; split durations so they fit into bars and parse to (tied) events
	  for events = (durations-and-pitches-to-events
			(third i) (fourth i) time-sig tempo)
	  do (multiple-value-bind (events attacks-indices)
		 (durations-and-pitches-to-events
		  (third i) (fourth i) time-sig tempo)
	       ;; add marks to events (using a list of indices of the events
	       ;; before they were split into bars).
	       (loop for m in (fifth i) 
		     when (< (car m) (length attacks-indices))
		       do (add-mark (nth (nth (car m) attacks-indices) events)
			      (cadr m)))
	       ;; generate bars
	       (push (loop while events
			   for bar = (make-rthm-seq-bar `(,time-sig))
			   for ate = (fill-with-rhythms bar events)
			   do (setf events (when ate (nthcdr ate events)))
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
		      collect (append bars (ml empty-bar
					       (- max-len (length bars)))))))))
    ;; put them in a slippery-chicken object:
    (loop for bars in (reverse list-of-list-of-bars) and i in lists
	  do (setf sc (bars-to-sc bars :sc sc :player (first i)
				       :instrument (second i) :tempo tempo)))
    ;; set composer and title
    (setf (composer sc) composer (title sc) title)
    (check-ties sc t)
    ;; call write-xml on sc-object
    (write-xml sc :file file)))

;; EOF helpers.lsp
