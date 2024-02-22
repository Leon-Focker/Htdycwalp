;; * score

(in-package :ly)

;; ** todo

;;; discern between players and instrument
;;; actually compose divisions, states and dynamics
;;; compose instruments
;;; interpret-minutes-by-instrument
;;; interpretation functions for each instrument, that are then placed within
;;;  interpret-minutes-by-instrument and interpret-layer.
;;; maybe we don't need to discern between tape and instrument layers?

;; ** divide (generate divisions and states)

;; *** divisions + states + dynamics

;; ;;; number of divisions:
;; (kernel-transitions 11 7 '(1 -2 3))
;; (defparameter *div-cnt* (window-transitions 11 '(2 3 4 7 9 5 2) .5 .5))

;; ;;; division ratios:
;; (defparameter *divisions* (procession (apply #'+ *div-cnt*) '(1 2 3 4 5)))

(let* ((number-of-division-seeds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 '((2 3 4 7 9 5 2)       ; tape
	   (2 3 7 4 9 2 5)       ; tuba+
	   (2 3 7 4 5 3 2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (division-ratios-seed '(1 2 3 4 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (states-seed '(1 2 3 4 5 6 7 8))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (dynamics-seed '(0 1 2 3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (loop for seed in number-of-division-seeds and i in (minutes-layer-numbers)
	for nr-of-divs = (window-transitions (length (access-minutes)) seed .5)
	for divs = (procession (apply #'+ nr-of-divs) division-ratios-seed)
	for states = (procession (apply #'+ nr-of-divs) states-seed)
	for dynamics = (procession (apply #'+ nr-of-divs) dynamics-seed)
	do (distribute-divs divs nr-of-divs (access-minutes) i)
	   (distribute-states states nr-of-divs (access-minutes) i)
	   (distribute-dynamics dynamics nr-of-divs (access-minutes) i)))

;; *** instruments

;;; very simple distribution for now:
(let ((strings '(violin viola cello double-bass)) ;; violin2
      (woodwinds '(flute oboe b-flat-clarinet bassoon))
      (brass '(french-horn c-trumpet bass-trombone tuba))
      (perc 'percussion))
  (loop for minute in (access-minutes)
	and i in (fibonacci-transitions (length (access-minutes)) '(3 2 1))
	do (loop for layer in (layers minute)
		 do (when (= (number layer) 1) (setf (instruments layer) strings))
		    (when (= (number layer) 2) (setf (instruments layer) woodwinds))
		    (when (= (number layer) 3) (setf (instruments layer) brass))
		    (when (= (number layer) i) (push perc (instruments layer))))))

;; VISUALIZE MINUTES
(visualize-minutes (access-minutes) '(0 1 2 3 111) "/E/code/ensemble/test_wts" 1/4 nil)

(lists-to-xml (interpret-layer (nth 1 (layers (nth 8 (access-minutes)))))
	      "/E/code/ensemble/test1.xml")
(lists-to-xml (interpret-layer (nth 2 (layers (nth 8 (access-minutes)))))
	      "/E/code/ensemble/test2.xml")
(lists-to-xml (interpret-layer (nth 3 (layers (nth 8 (access-minutes)))))
	      "/E/code/ensemble/test3.xml")

;; update start-times of all layers just to be sure:
(loop for i in (access-minutes) do (update-layer-start-times i))

;; ** ...and conquer (interpret the minute objects)

;; interpret layers returns a function that can then be filled with arguments
;;  to form a sections like function.

;; EOF score.lsp

