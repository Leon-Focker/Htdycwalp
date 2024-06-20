;; * score

(in-package :ly)

;; (re-)load minutes
(load (compile-file (probe-file (format nil "~aminutes.lsp" +ens-src-dir+))))

;; ** todo

;;; what about crescendi at the end? add extra minute (with rests) to not lose them?
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
       (dynamics-seed '(0 1 4 0 2 3 5)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (loop with nr-of-mins = (length (access-minutes))
	for dseed in number-of-division-seeds and sseed in states-seeds
	for i in (minutes-layer-numbers)
	for nr-of-divs = (window-transitions nr-of-mins dseed .5)
	for nr-of-items = (apply #'+ nr-of-divs)
	for divs = '() and states = '() and dynamics = '() and chords = '()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;	REPLACE
	do (case i
	     (0)
	     (1 (setf (cdr (nthcdr (- nr-of-mins 2) nr-of-divs)) '(2))) ; last=2
	     (2)
	     (3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   (setf divs (procession nr-of-items division-ratios-seed)
		 states (procession nr-of-items sseed)
		 dynamics (procession nr-of-items dynamics-seed))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   (distribute-divs divs nr-of-divs (access-minutes) i)
	   (distribute-states states nr-of-divs (access-minutes) i)
	   (distribute-dynamics dynamics nr-of-divs (access-minutes) i)
	   (distribute-chords chords nr-of-divs (access-minutes) i)))

;; *** chords

(let* ((all-chords '(((c2 g2 d3 b3 gf4 d5 g5) ; chords, starting with consonant
		      (c2 g2 d3 b3 gf4 d5 a5)
		      (c2 g2 d3 c4 gf4 d5 a5)
		      (c2 g2 d3 c4 g4 d5 b5)
		      (c2 g2 d3 c4 g4 d5 bf5)
		      (c2 g2 d3 c4 g4 ef5 bf5)
		      (c2 g2 e3 b3 gf4 b4 b5)
		      (c2 g2 e3 b3 gf4 c4 bf5)
		      (c2 g2 ef3 bf3 d4 a4 c4 bf5))
		     ;; medi
		     ((c2 g2 d3 b3 fs4 g4 d5 g5 d6)
		      (c2 g2 d3 b3 fs4 a4 e5 gs5 ds6)
		      (c2 g2 d3 b3 fs4 gs4 cs5 g5 d6)
		      (c2 g2 d3 cs4 fs4 a4 e5 g5 d6)
		      (c2 g2 d3 cs4 fs4 g4 d5 fs5 d5)
		      (c2 g2 d3 fs3 cs4 fs4 as4 f5)
		      (c2 g2 ds3 as3 f4 c5 fs5)
		      (c2 g2 ds3 as3 f4 cs5 fs5 c6)
		      (c2 g2 ds3 b3 c4 fs4 cs5 a5))
		     ;; dissonant
		     ((c2 g2 d3 gf3 b3 df4 f4 bf4 e5 b5)
		      (c2 g2 d3 gf3 b3 df4 f4 bf4 ef5 a5)
		      (c2 g2 d3 gf3 bf3 df4 e4 a4 ef5 af5)
		      (c2 g2 d3 f3 a3 b4 e4 bf4 gf5 af5))))
       (transpose-chords
	 (kernel-transitions 11 '(10 9 8 7 6 5 4 3 2 1 0) '(3 -3))))
  (loop for minute in (access-minutes) and offset in transpose-chords
	with last-i = 0
	for max-nr = 0
	do (flet ((tr (x) (+ (note-to-midi x) offset)))
	     (loop for layer in (layers minute)
		   for nr = (length (states layer))
		   do (setf
		       max-nr
		       (max max-nr nr)
		       (chords layer)
		       (loop repeat nr for i from last-i
			     collect
			     (list
			      (mapcar #'tr (nth-mod i (nth 0 all-chords)))
			      (mapcar #'tr (nth-mod i (nth 1 all-chords)))
			      (mapcar #'tr (nth-mod i (nth 2 all-chords))))))))
	   (incf last-i max-nr)))

;; *** replace more

;;; replace division-ratios in layer 2 and 3 with the ratios of 1
(flet ((helper (x) (get-lsim (access-minutes) 7 x 'division-ratios)))
  (when (apply #'= (loop for i from 1 to 3 collect (length (helper i))))
    (set-lsim (access-minutes) 7 2 'division-ratios (helper 1))
    (set-lsim (access-minutes) 7 3 'division-ratios (helper 1))))

;;; DYNAMICS

;;; set beginning of minute 0 and 1 to ff in tape:
(setf (car (dynamics (find 0 (layers (nth 0 (access-minutes))) :key 'number)))5)
(setf (car (dynamics (find 0 (layers (nth 1 (access-minutes))) :key 'number)))5)
;;; copy dynamics from tape to brass (crescendo = rest):
(set-related-dynamics (access-minutes) 0 3
		      '((0 0) (1 0) (2 2) (3 3) (4 4) (5 5)))
;;; invert dynamics from brass to strings:
(set-related-dynamics (access-minutes) 3 1
		      '((0 5) (1 4) (2 1) (3 0) (4 1) (5 3)) nil)
;;; set second dynamic of minute 0 and 1 to cresc in strings:
(setf (cadr (dynamics (find 1 (layers (nth 0 (access-minutes))) :key 'number)))1)
(setf (cadr (dynamics (find 1 (layers (nth 1 (access-minutes))) :key 'number)))1)

;; *** instruments

;;; very simple distribution:
(let ((strings '(violin-1 violin-2 viola cello double-bass))
      (woodwinds '(flute oboe b-flat-clarinet bassoon))
      (brass '(c-trumpet french-horn tuBa))
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

;; update start-times of all layers just to be sure:
(loop for i in (access-minutes) do (update-layer-start-times i))

;; ** ...and conquer (interpret the minute objects)

;; *** visualize minutes
;; (visualize-minutes (access-minutes) '(3 2 1 0 111)
;; 		   (format nil "~a~a" +ens-src-dir+ "test.png") 1 nil)

;; *** write entire score
;; (lists-to-xml (interpret-minutes (access-minutes))
;; 	      (format nil "~a~a" +ens-src-dir+ "Score_from_Lisp.xml")
;; 	      :tempo 60
;; 	      :time-sig '(4 4)
;; 	      :print-rehearsal-letters t
;; 	      :set-new-dynamics t)

;; *** generate patterns and alternative versions

;; **** get rhythmic material for minute 4

#|
;; different combinations of i-div and divisor to generate patterns:
(set-lsim (access-minutes) 3 1 'divisor 3)
(set-lsim (access-minutes) 3 1 'i-div 6)
(lists-to-xml (interpret-minutes (subseq (access-minutes) 3 4)
				 '(violin-1 violin-2 viola cello double-bass))
	      (format nil "~a~a" +ens-src-dir+ "minute_4_3_6.xml")
	      :tempo 90)
|#

;; **** patterns minute 9 ...
;; ... were notated by hand

;; **** get rhythmic material for last minute

;; interpret everything as state 3 (morphing rhythms)
;; moving threshold...

;; (set-lsim (access-minutes) 10 1 'divisor 8)
;; (set-lsim (access-minutes) 10 1 'i-div 20)
;; (set-lsim (access-minutes) 10 1 'states
;; 	  (ml 3 (length (get-lsim (access-minutes) 10 1 'states))))
;; (set-lsim (access-minutes) 10 2 'divisor 6)
;; (set-lsim (access-minutes) 10 2 'i-div 13)
;; (set-lsim (access-minutes) 10 2 'states
;; 	  (ml 3 (length (get-lsim (access-minutes) 10 2 'states))))
;; (set-lsim (access-minutes) 10 3 'divisor 6)
;; (set-lsim (access-minutes) 10 3 'i-div 7)
;; (set-lsim (access-minutes) 10 3 'states
;; 	  (ml 3 (length (get-lsim (access-minutes) 10 3 'states))))

;; (lists-to-xml (append (interpret-layer (third (layers (nth 10 (access-minutes)))))
;; 		      (interpret-layer (fourth (layers (nth 10 (access-minutes)))))
;; 		      (interpret-layer (second (layers (nth 10 (access-minutes))))))
;; 	      (format nil "~a~a" +ens-src-dir+ "patters_last_minute.xml")
;; 	      :tempo 60)

;; EOF score.lsp
