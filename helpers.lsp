;; * helpers.lsp

(in-package :ly)

;; *** nth-mod
(defun nth-mod (i rthm-ls)
  (nth (mod i (length rthm-ls)) rthm-ls))

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

;;; TODO marks (HOW?), composer, title etc... as keys
;;; TODO if durations don't fill time-sig perfectly, fill with rests.
;;;
;;; lists should be a list of lists. These sublists should have this format:
;;; '(player instrument (list-of-durations) (list-of-pitches))
;;; file should be the path to and the filename for the resulting .xml file 
(defun lists-to-xml (lists file &optional (time-sig '(4 4)) (tempo 60))
  (unless (and (listp lists) (loop for ls in lists always (listp ls)))
    (error "lists in lists-to-xml seems to be malformed"))
  (let* (sc)
    (loop for i in lists
	  for events = (durations-and-pitches-to-events
			(third i) (fourth i) time-sig tempo)
	  for bars = (loop while events
			   for bar = (make-rthm-seq-bar `(,time-sig))
			   for ate = (fill-with-rhythms bar events)
			   do (setf events (when ate (nthcdr ate events)))
			   collect bar)
	  do (setf sc (bars-to-sc bars :sc sc :player (first i)
				       :instrument (second i) :tempo tempo)))
;;   (add-mark-to-event sc 1 2 'player-two "schneller")
    (write-xml sc :file file)))

;;; durations in seconds
(defun durations-and-pitches-to-events (duration-list pitch-list
					&optional (time-sig '(4 4)) (tempo 60))
  ;; loop the shorter list to get the same length for both:
  (multiple-value-bind (ls1 ls2) (pad-lists duration-list pitch-list)
    (setf duration-list ls1 pitch-list ls2))
  (let* ((events '())
	 ;; duration of a bar in seconds:
	 (duration-of-bar (* 60 (/ 4 (cadr time-sig) tempo) (car time-sig))))
    (loop while (and duration-list pitch-list)
	  with is-tied
	  with current-sum = 0
	  for is-rest = (not (car pitch-list)) ;; nil means rest
	  for next-dur = (pop duration-list)
	  for next-sum = (+ current-sum next-dur)
	  do (cond ((equal-within-tolerance next-sum duration-of-bar 0.001)
		    (push (make-event (pop pitch-list) next-dur
				      :is-tied-to is-tied :duration t
				      :is-rest is-rest :tempo tempo)
			  events)
		    (setf current-sum 0 is-tied nil))
		   ((< next-sum duration-of-bar)
		    (push (make-event (make-pitch (pop pitch-list)) next-dur
				      :is-tied-to is-tied :duration t
				      :is-rest is-rest :tempo tempo)
			  events)
		    (setf current-sum next-sum is-tied nil))
		   ((> next-sum duration-of-bar)
		    (let ((diff (- duration-of-bar current-sum)))
		      (push (make-event (make-pitch (car pitch-list)) diff
					:is-tied-to is-tied :is-tied-from t
					:duration t :is-rest is-rest
					:tempo tempo)
			    events)
		      (push (- next-dur diff) duration-list))
		    (setf current-sum 0 is-tied (unless is-rest t)))))
    (setf events (reverse events))
    ))

;; EOF helpers.lsp
