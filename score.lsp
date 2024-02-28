;; * score

(in-package :ly)

;; ** todo

;;; actually compose divisions and states
;;; interpretation functions for each instrument, that are then placed within
;;;  interpret-layer-by-instrument.
;;; maybe we don't need to discern between tape and instrument layers? as tape
;;;  is just another instrument?
;;; what about crescendi at the end? add extra minute (with rests) to not lose them?
;;; idea: double-bass is a solo-instrument and strings are sometimes coupled
;;;  with ww or bass.
;;; respect lowest and highest notes for each instrument.
;;; should interpret-layer-by-instrument be able to see other layers in minute?
;;;  this way it knows wheter it is solo etc.

;; ** divide (generate divisions and states)

;; *** divisions + states + dynamics

(let* ((number-of-division-seeds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 '((2 3 7 4 9 2 5)		; tape
	   (2 3 4 7 5 9 2)		; strings
	   (2 3 2 7 5 4 9)		; ww
	   (2 3 7 4 5 3 2)))		; brass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (division-ratios-seed '(1 2 3 4 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (states-seeds '((1 2 3 4 5 6 7 8)
		       (1 7 2 3 4 5 6 8)
		       (1 7 2 3 4 5 6 8)
		       (1 7 2 3 4 5 6 8)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (dynamics-seed '(0 1 4 2 3 5)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (loop with nr-of-mins = (length (access-minutes))
	for dseed in number-of-division-seeds and sseed in states-seeds
	for i in (minutes-layer-numbers)
	for nr-of-divs = (window-transitions nr-of-mins dseed .5)
	for divs = '() for states = '() for dynamics = '()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;	REPLACE
	do (case i
	     (0)
	     (1 (setf (cdr (nthcdr (- nr-of-mins 2) nr-of-divs)) '(2))) ; last=2
	     (2)
	     (3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   (setf divs (procession (apply #'+ nr-of-divs) division-ratios-seed)
		 states (procession (apply #'+ nr-of-divs) sseed)
		 dynamics (procession (apply #'+ nr-of-divs) dynamics-seed))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   (distribute-divs divs nr-of-divs (access-minutes) i)
	   (distribute-states states nr-of-divs (access-minutes) i)
	   (distribute-dynamics dynamics nr-of-divs (access-minutes) i)))

;; *** replace more

;;; replace division-ratios in layer 2 and 3 with the ratios of 1
(flet ((helper (x) (get-lsim (access-minutes) 7 x 'division-ratios)))
  (when (apply #'= (loop for i from 1 to 3 collect (length (helper i))))
    (set-lsim (access-minutes) 7 2 'division-ratios (helper 1))
    (set-lsim (access-minutes) 7 3 'division-ratios (helper 1))))
;;; set beginning of minute 0 and 1 to ff in tape:
(setf (car (dynamics (find 0 (layers (nth 0 (access-minutes))) :key 'number)))5)
(setf (car (dynamics (find 0 (layers (nth 1 (access-minutes))) :key 'number)))5)

;;; DYNAMICS
;;; copy dynamics from tape to brass:
(set-related-dynamics (access-minutes) 0 3
		      '((0 0) (1 1) (2 2) (3 3) (4 4) (5 5)))
;;; invert dynamics from tape to strings:
(set-related-dynamics (access-minutes) 0 1
		      '((0 5) (1 4) (2 1) (3 0) (4 1) (5 3)) nil)

;; *** instruments

;;; very simple distribution for now:
(let ((strings '(violin-1 violin-2 viola cello double-bass))
      (woodwinds '(flute oboe b-flat-clarinet bassoon))
      (brass '(c-trumpet french-horn tuba))
      (tromb 'bass-trombone)
      (perc 'percussion))
  (loop for minute in (access-minutes)
	and fib1 in (fibonacci-transitions (length (access-minutes)) '(3 2 1))
	and fib2 in (procession (length (access-minutes)) '(3 3 2 2))
	do (loop for layer in (layers minute)
		 do (when (= (number layer) 1) (setf (instruments layer) strings))
		    (when (= (number layer) 2) (setf (instruments layer) woodwinds))
		    (when (= (number layer) 3) (setf (instruments layer) brass))
		    (when (= (number layer) fib1) (push perc (instruments layer)))
		    (when (= (number layer) fib2) (push tromb (instruments layer))))))

;; VISUALIZE MINUTES
;(visualize-minutes (access-minutes) '(3 2 1 0 111) "/E/code/ensemble/test1" 1 nil t)

;; update start-times of all layers just to be sure:
(loop for i in (access-minutes) do (update-layer-start-times i))

;; ** ...and conquer (interpret the minute objects)

#|
(lists-to-xml (interpret-layer (nth 1 (layers (nth 9 (access-minutes)))))
	      "/E/code/ensemble/test1.xml")
(lists-to-xml (interpret-layer (nth 2 (layers (nth 9 (access-minutes)))))
	      "/E/code/ensemble/test2.xml")
(lists-to-xml (interpret-layer (nth 3 (layers (nth 9 (access-minutes)))))
	      "/E/code/ensemble/test3.xml")
|#

;; write entire score
(lists-to-xml (interpret-minutes (access-minutes))
	      "/E/code/ensemble/test.xml")

(lists-to-xml (interpret-minutes (access-minutes) '(tuba percussion double-bass flute computer))
	      "/E/code/ensemble/test2.xml")

(lists-to-xml (interpret-minutes (subseq (access-minutes) 2 4) '(tuba))
	      "/E/code/ensemble/test1.xml")

;; idea: interpret layers returns a function that can then be filled with arguments
;;  to form a sections like function.

;; EOF score.lsp

