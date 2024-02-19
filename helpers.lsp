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

;; *** distribute-divs
;;; get list of division-ratios and numbers-of-division and distribute the
;;; division-ratios to the layers of list-of-minutes with the specified number.
(defun distribute-divs (division-ratios number-of-divisions list-of-minutes
			layer-number)
  (let ((layers (get-related-minute-layers list-of-minutes layer-number)))
    (unless (= (length layers) (length number-of-divisions))
      (error "unequal amount of relevant layers and elements in ~
            number-of-division: ~a ~a"
	     (length layers) (length number-of-divisions)))
    (loop for divs in (split-list division-ratios number-of-divisions)
	  and layer in layers
	  do (setf (div-ratios layer) divs)))
  t)

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

#|
(defun notate (lst-of-harmonics file &optional (instrument 'viola))
  (unless (listp lst-of-harmonics) (error "not a list: ~a" lst-of-harmonics))
  (unless (listp (car lst-of-harmonics))
    (setf lst-of-harmonics (list lst-of-harmonics)))
  
  (let* ((events (loop for i in (flatten lst-of-harmonics)
			      collect (parse-to-event i)))
	 (letters (loop for i in lst-of-harmonics
			with n = 2
			collect n
			do (setf n (+ n (length i)))))
	 (len (length events))
	 (sc (make-slippery-chicken
              '+harmonics-to-notation+
              :ensemble `(((ins (,instrument :midi-channel 1))))
              :set-palette '((1 ((c4))))
              :set-map (loop for i from 1 to len collect `(,i (1)))
              :rthm-seq-palette '((1 ((((1 4) q)))))
	      :rehearsal-letters letters
              :rthm-seq-map (loop for i from 1 to len collect `(,i ((ins (1))))))))
    (map-over-events sc 0 nil 'ins
		     #'(lambda (e) (setf (pitch-or-chord e)
				    (pitch-or-chord (pop events)))))
    (write-xml sc :file file)))
|#

;; EOF helpers.lsp
