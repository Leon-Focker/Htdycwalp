;; * chords.lsp

;;; not so pretty file to generate strings of notes. Similar generations can
;;; then be layered as chords.

(in-package :ly)

;;; when compiling, SBCL complains about a function being undefined when it was
;;; defined in a let (like #'access-intervalls). So let's avoid that warning:
(declaim (ftype (function) access-intervals))

;;; this is only a hash-table because I wanted to learn how to use them. There
;;; is no need for this to be a hash-table.
(let ((intervals (make-hash-table)))
  (defun access-intervals ()
    intervals))

(defun set-intervals (key ls)
  (setf (gethash key (access-intervals)) ls))

(set-intervals 0 '(0 12 -12))
(set-intervals 1 '(7 -5))
(set-intervals 2 '(-7 5))
(set-intervals 3 '(4 -4 9 -9))
(set-intervals 4 '(3 -3 8 -8))
(set-intervals 5 '(1 -1 2 -2))
(set-intervals 6 '(10 -10 11 -11))
(set-intervals 7 '(6 -6))

;; length (of sequence)
;; first-note (integer - midi value)
;; abmitus (list of lowest, highest note)
;; dissonance-env (from 0 to 1)
;; variation-env (how many intervals are possible) (from 0 to 1)
;; offset: if 1, the first note gets treated like its the second, and so on.
;;  setting this to a multiple of length is useful to get varations of a line
(defun gen-melodic-line (length first-note ambitus dissonance-env variation-env
			 &optional (offset 0))
  (unless (integerp first-note)
    (error "first-note is not an integer but: ~a" first-note))
  (loop for i from 0 below length
	for progress = (/ i length)
	with next = first-note
	with last = first-note
	collect
	(let* ((dissonance (interpolate progress dissonance-env))
	       (variation (round (interpolate progress variation-env)))
	       (dissonance-keys (loop for i from (max 0 (round (- dissonance (/ variation 2))))
					to (min 7 (round (+ dissonance (/ variation 2))))
				      collect i))
	       (intervals (loop for i in dissonance-keys append (gethash i (access-intervals))))
	       (interval-cnt (length intervals)))
	  (prog1 next
	    (labels ((helper (cnt)
		       (setf next (+ last (nth (mod cnt interval-cnt) intervals)))
		       (unless (or (<= (first ambitus) next (second ambitus))
				   (= (- cnt (length intervals) offset) i))
			 (helper (1+ cnt)))))
	      (helper (+ i offset)))
	    (setf last next)))))

;; make it a function!
(defun gen-bassline-permutations
    (first-notes ambitus-list
     max-len voices dissonance-env variation-env permutation file
     &optional incr-channel)
  (let* ((pitches '())
	 (durations '(1))
	 (start-times '())
	 (channel 0))
    ;; we do this to use the perks of re-order!
    (setf permutation (re-order (loop for i from 0 below max-len collect i)
				permutation))
    (setf pitches
	  (flatten 
	   (loop for n in permutation do (incf n)
		 collect
		 (loop for i from 0 below voices
		       append
		       (gen-melodic-line n (nth i first-notes)
					 (nth i ambitus-list)
					 dissonance-env
					 variation-env (* n i))))))
    (setf start-times
	  (flatten
	   (loop for n in permutation with start = 0
		 collect (loop repeat voices
			       with ls = (loop for i from start to (+ start n)
					       collect i)
			       append ls)
		 do (incf start (1+ n)))))
    ;; incf channel for each voice
    (when incr-channel
      (setf channel
	    (loop for n in permutation
		  do (incf n)
		  append (loop for i from 0 below voices
			       append (ml (mod i 16) n)))))
    (lists-to-midi pitches durations start-times
		   :file file
		   :channel channel)))

;; ** three chords:

#|
(let* ((max-len 11)
       (dissonance-env '(0 1       .3 2  .45 2  .5 6  .8 1  1 4))
       (variation-env '(0 1  .2 0  .3 2  .4 1       .6 1    1 5))
       (permutation '(0 1 2 3 4 5 6 7 8 9 10 11)))
  (gen-bassline-permutations '(74 75 76 78) '((62 76) (50 64) (38 52) (26 30)) max-len 4
			     dissonance-env variation-env permutation
			     (format nil "~a~a"
				     +ens-src-dir+
				     "bassline_permutation_chord_11.mid"))
  (gen-bassline-permutations '(65 68 70 71 72) '((26 96) (26 96) (26 96) (26 96) (26 96)) max-len 5
			     dissonance-env variation-env permutation
			     (format nil "~a~a"
				     +ens-src-dir+
				     "bassline_permutation_chord_22.mid"))
  (gen-bassline-permutations '(67 69 61) '((50 90) (50 90) (50 90)) max-len 3
			     dissonance-env variation-env permutation
			     (format nil "~a~a"
				     +ens-src-dir+
				     "bassline_permutation_chord_33.mid")))
|#

;; chords for minute 4:
#|
(gen-bassline-permutations '(75 76 79 80 75) '((40 80) (50 79) (59 85) (73 92) (40 80)) 8 5
			   '(0 1  .3  6  .7 3 1 1)
			   '(0 5 .4 2  1 0)
			   nil
			   (format nil "~a~a" +ens-src-dir+ "chords_min_4.1.mid"))

(gen-bassline-permutations '(74 75 79 80 75) '((40 80) (50 79) (59 85) (73 92) (40 80)) 8 5
			   '(0 1  .3  6  .7 3 1 1)
			   '(0 5 .4 2  1 0)
			   nil
			   (format nil "~a~a" +ens-src-dir+ "chords_min_4.2.mid"))
|#

;; drifting melodies for brass in minute 8, to be reversed:
#|
(let ((instruments '(c-trumpet french-horn bass-trombone tuba))
      (ambitus '()))
  (setf ambitus
	(loop for ins in instruments
	      for i = (my-get-standard-ins ins)
	      collect (list (midi-note (lowest-sounding i))
			    (midi-note (highest-sounding i)))))
  (gen-bassline-permutations (ml 59 (length instruments)) ambitus 12 (length instruments)
			     '(0 0   1 7)
			     '(0 0   1 3)
			     nil
			     (format nil "~a~a" +ens-src-dir+ "drifting_min_8.mid")))
|#


;; drifting melodies for minute 5
;; patterns starting after one another, trying to find together, chords go out.
;; starting with 
#|
(let ((instruments '(violin viola cello flute french-horn bass-trombone))
      (ambitus '()))
  (setf ambitus
	(loop for ins in instruments
	      for i = (my-get-standard-ins ins)
	      collect (list (midi-note (lowest-sounding i))
			    (midi-note (highest-sounding i)))))
  (gen-bassline-permutations (ml 69 (length instruments)) ambitus 12 (length instruments)
			     '(0 0   1 7)
			     '(0 0   1 3)
			     nil
			     (format nil "~a~a" +ens-src-dir+ "drifting_min_5.mid")
			     t))
|#

;; EOF chords.lsp
