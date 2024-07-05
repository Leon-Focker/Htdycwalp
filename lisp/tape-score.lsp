;; * tape-score

(in-package :ly)

(load (format nil "~a~a" +ens-src-dir+ "score.lsp"))

;; ** minute 1
;; simple, steady pulse train
;; rthm = 1/13, rqq rhythm with 13 equal divisions
;; going slower through the indisp-function to get kind of iso-rhythmic
;; displacements.
(wsound "minute_1"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (first (access-minutes)))))
      (declare (ignore start-times score-rhythm score-srt score-amp
		       score-time-mult))
      (fplay 0 60
	     (sound (nth 6 sound-list))
	     (rhythm 1/13)
	     (indisp-fun (funcall score-indisp))
	     (srt .5)
	     (duration .01)
	     (tim-mult (- 5 (* line 2.5)))
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1)))))
	     (amp (funcall (sections 0  (dry-wet 0.9 amp-val (* line 2))
				     30 amp-val)
			   time))
	     (out-channels 1)
	     (degree 0)))))

;; ** minute 2
;; simple, steady pulse train
;; rthm = 1/13, rqq rhythm with 13 equal divisions
;; going slower through the indisp-function to get kind of iso-rhythmic
;; displacements.
(wsound "minute_2"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (second (access-minutes)))))
      (declare (ignore start-times score-rhythm score-srt score-amp score-time-mult))
      (fplay 0 60
	     (sound (nth 6 sound-list))
	     (rhythm 1/13)
	     (indisp-fun (funcall score-indisp))
	     (srt (interpolate line '(0 .5  1 1.3) :warn nil))
	     (duration .01)
	     (tim-mult (- 5 (* line 2.5))(- 5 (* line 2.5)))
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1)))))
	     (amp (funcall (sections 0  .9
				     (* 8/13 60) amp-val)
			   time))
	     (out-channels 1)
	     (degree 0)))))

;; ** minute 3

;; solos with percussion, break in between.
(wsound "minute_3"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (third (access-minutes)))))
      (declare (ignore start-times score-rhythm))
      (fplay 0 60
	     (sound (nth 6 sound-list))
	     (rhythm 1/8)
	     (indisp-fun (funcall score-indisp 'time time))
	     (duration .01)
	     (time-mult (funcall (funcall score-time-mult time) 'time time))
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time time-mult) 1)))))
	     (srt (funcall (funcall score-srt time) 'amp-val amp-val 'line line))
	     (amp (dry-wet (* (interpolate (min time 60) score-amp) amp-val)
			   .9
			   (max (- 1 (* line 3)) 0)))
	     (out-channels 1)
	     (degree 0)))))

;; ** minute 4

;; simple, steady pulse with rthm = 1/13
(wsound "minute_4_sides"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (fplay 0 20
	   (sound (nth 6 sound-list))
	   (rhythm 1/13)
	   (srt .5)
	   (duration .01)
	   (amp 0.9)
	   (degree 0 90))))

;; simple, steady pulse train
;; rthm = 1/13, rqq rhythm with 13 units divided by 3 5 2 3.
;; the srt is slowly going up and the soundfile is rhythmically changed.
;; DIVIDED in two parts: fast and slow pulses, switching between them is PDs job
;; part two is elongated in the DAW
(loop for part from 1 to 2 do
  (wsound (format nil "minute_4_~a_mid" part)
    (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
      (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			    score-amp score-time-mult)
	  (interpret-tape (first (layers (fourth (access-minutes)))))
	(declare (ignore start-times score-srt score-amp score-time-mult))
	(fplay 0 60
	       ;; don't use dynamics, would be a kind of cresc anyways
	       ;; (dynamics (interpolate (min time 60) score-amp))
	       (indisp-fun (funcall score-indisp 'time time))
	       (sound (nth (case (funcall indisp-fun (* time 4/13))
			     ((0 1 2) 0)
			     ((3 4 5 6 7) 2)
			     ((8 9) 4)
			     (t 6))
			   sound-list))
	       ;; (rhythm (funcall (funcall score-rhythm time) 'line line))
	       (rhythm (funcall (funcall score-rhythm (if (= part 1) 0 55)) 'line line))
	       (srt (interpolate (expt line 2) '(0 .5  1 2) :warn nil))
	       (duration .01)
	       (amp (* 1/13 (1+ (funcall indisp-fun (mod time 1)))))
	       (degree 45))))))

;; ** minute 5

;; pulses with changing rhythm, depending on indisp-fun * .25
;; haas effect on second channel.
;; sounds are chosen in the same order, when to chose a new sound is determined
;; by indisp-fun * .2
;; start is the loudest sample in the soundfile
;; srt goes up from .5 to 8

;; using samp0 to resample and adjust for tempo 64
(wsound "minute_5"
  (resample 64/60
    (let* ((sound-list (reverse (data (getf *soundfiles* :noise))))
	   (sound-list1 (reverse (data (getf *soundfiles* :percussive))))
	   (cnt 0))
      (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			    score-amp score-time-mult)
	  (interpret-tape (first (layers (fifth (access-minutes)))))
	(declare (ignore score-rhythm score-srt score-amp))
	(fplay 0 60
	       (indisp-fun (funcall score-indisp 'time time))
	       (rhythm (case (funcall indisp-fun (* time .25))
			 ((0 1 2) 1/13)
			 ((3 4 5 6 7) .1)
			 ((8 9) 1/12)
			 (t .25))
		       (* rhythm 1.001)
		       (case (funcall indisp-fun (* time .25))
			 ((0 1) (+ 18/13))
			 ((2 3 4 5 6 7) 2/13)
			 ((8 9) 1/13)
			 (t 15/13))
		       (case (funcall indisp-fun (* time .25))
			 ((0 1) (+ 17/13))
			 ((2 3 4 5 6 7) 1/13)
			 ((8 9) 2/13)
			 (t 16/13)))
	       (stub (case (funcall indisp-fun (* time 1/5)) ;; change sound?
		       ((0 1 2 3) (incf cnt)))
		     ;; "quantise" time to 1/8th of a second:
		     (when (or (<= 13 time 24) (<= 44 time 48))
		       (setf rhythm (+ 1/8 (- (/ (ceiling (* time 8)) 8) time))
			     rhythm2 (+ 1/8 (- (/ (ceiling (* time2 8)) 8) time2)))))
	       (sound (nth-mod cnt (if (or (<= 24 time 34) (<= 44 time)) sound-list1 sound-list)))
	       (start (/ (peak-index sound) 48000))
	       (amp-mult (if (<= 44 time) (/ 1 (peak sound)) 1))	     

	       (srt-mod (section-val time
				     (nth-mod 0 start-times) 1
				     (nth-mod 1 start-times) 2
				     (nth-mod 2 start-times) .8
				     (nth-mod 3 start-times) 1))
	       (srt (interpolate (* line srt-mod) '(0 .5  1 8) :warn nil)
		    srt
		    5)
	       (duration 0.008)
	       (time-mult (funcall (funcall score-time-mult time)))
	       (amp (if (or (<= 44 time 48) (> time 58))
			1
			(* 1/13 (1+ (funcall indisp-fun (mod (* time time-mult) 1)))
			   (/ 1 (peak sound))))
		    amp
		    (if (<= time3 48) amp 0))
	       (degree 0 90 30 60))))))


;; PD version in two parts:
(loop for part from 1 to 2 do
  (wsound (format nil "minute_5_~a" part)
    (resample 64/60 '(0 1 2) '(0 120 240)
      (let* ((sound-list (reverse (data (getf *soundfiles* :noise))))
	     (sound-list1 (reverse (data (getf *soundfiles* :percussive))))
	     (cnt 0))
	(multiple-value-bind (start-times score-indisp score-rhythm score-srt
			      score-amp score-time-mult)
	    (interpret-tape (first (layers (fifth (access-minutes)))))
	  (declare (ignore score-rhythm score-srt score-amp))
	  (fplay 0 (if (= part 2) 80 60)
		 (indisp-fun (funcall score-indisp 'time time))
		 (rhythm (case (funcall indisp-fun (* time .25))
			   ((0 1 2) 1/13)
			   ((3 4 5 6 7) .1)
			   ((8 9) 1/12)
			   (t .25))
			 (* rhythm 1.001)
			 (case (funcall indisp-fun (* time .25))
			   ((0 1) (+ 18/13))
			   ((2 3 4 5 6 7) 2/13)
			   ((8 9) 1/13)
			   (t 15/13))
			 (case (funcall indisp-fun (* time .25))
			   ((0 1) (+ 17/13))
			   ((2 3 4 5 6 7) 1/13)
			   ((8 9) 2/13)
			   (t 16/13)))
		 (stub (case (funcall indisp-fun (* time 1/5)) ;; change sound?
			 ((0 1 2 3) (incf cnt)))
		       ;; "quantise" time to 1/8th of a second:
		       (when (= part 2) ;;(or (<= 13 time 24) (<= 44 time 48))
			 (setf rhythm (+ 1/8 (- (/ (ceiling (* time 8)) 8) time))
			       rhythm2 (+ 1/8 (- (/ (ceiling (* time2 8)) 8) time2)))))
		 (sound (nth-mod cnt (if (or (<= 24 time 34) (<= 44 time) (= part 2))
					 sound-list1 sound-list)))
		 (start (/ (peak-index sound) 48000))
		 (amp-mult (if (<= 44 time) (/ 1 (peak sound)) 1))	     

		 (srt-mod (section-val time
				       (nth-mod 0 start-times) 1
				       (nth-mod 1 start-times) 2
				       (nth-mod 2 start-times) .8
				       (nth-mod 3 start-times) 1))
		 (srt (interpolate (* line srt-mod) '(0 .5  1 8) :warn nil)
		      srt
		      5)
		 (duration 0.008)
		 (time-mult (funcall (funcall score-time-mult time)))
		 (amp (if (or (<= 44 time 48) (> time 58) (= part 2))
		      	  1
		      	  (* 1/13 (1+ (funcall indisp-fun (mod (* time time-mult) 1)))
		      	     (/ 1 (peak sound))))
		      amp
		      (if (<= time3 48) amp 0))
		 (out-channels 3)
		 (degree 0 180 120 240)))))))

(unpack_3chan_file "minute_5_1")
(unpack_3chan_file "minute_5_2")

;; ** minute 6

;; original version, no longer needed
#|
(wsound "minute_6"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise))))
	 (sound-list1 (reverse (data (getf *soundfiles* :percussive)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (sixth (access-minutes)))))
      (declare (ignore start-times score-time-mult))
      (fplay 0 60
	     (dynamics (interpolate (min time 60) score-amp))
	     (score-rthm (funcall (funcall score-rhythm time) 'line line)
			 (funcall (funcall score-rhythm time2) 'line line)
			 (funcall (funcall score-rhythm time3) 'line line)
			 (funcall (funcall score-rhythm time4) 'line line))
	     (rhythm (section-val time
				  0 score-rthm
				  20 1/26
				  40 (* 3/13 (mirrors (abs (sin (* time 1/2))) .2 1)))
		     rhythm
		     rhythm
		     score-rthm4)
	     (indisp-fun (funcall score-indisp 'time time))
	     (sound (section-val time
				 0 (nth 6 sound-list)
				 40 (nth 6 sound-list1)))
	     (duration (section-val time
				    0  .01
				    40 .02)
		       duration
		       (section-val time3
				    0  .01
				    40 .02))
	     (tim-mult (section-val time
				    0 (- 5 (* line 2.5))
				    20 line))
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1))))
		      amp-val
		      (- 1 amp-val)
		      (1- (* 1/13 (1+ (funcall indisp-fun (mod (* time4 tim-mult) 1))))))
	     (srt (if (<= time 40)
		      (funcall (funcall score-srt time) 'amp-val amp-val 'line line)
		      1)
		  (if (<= time2 40) 2 25)
		  (if (<= time3 40) 4 24)
		  1)
	     (start (section-val time
				 0 0
				 40 (* (- line 2/3) .2))
		    start
		    (section-val time3
				 0 0
				 40 (* (- line 2/3) .2)))
	     (amp (section-val time
			       0 (* amp-val dynamics)
			       20 0
			       24 (dry-wet (* amp-val dynamics) 0 (/ (- time 20) 20))
			       44 (* amp-val dynamics .0007))
		  amp
		  (section-val time
			       0 (* .07 amp-val3 (- 1 dynamics))
			       20 0
			       24 (dry-wet (* .07 amp-val3 (- 1 dynamics)) 0 (/ (- time3 20) 20))
			       40 (* .07 amp-val3 (- 1 dynamics) .0007))
		  (section-val time
			       0 0
			       24 (- 1 (* amp3 20))
			       40 0))
	     (out-channels 3)
	     (degree 120 120 240 0)))))
|#

;; *** minute 6 part 1

;;; different rhythms and sounds
(wsound "minute_6_1"
  (let* ((sound-list1 (reverse (data (getf *soundfiles* :percussive)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (sixth (access-minutes)))))
      (declare (ignore start-times score-time-mult))
      (fplay 0 80
	     (dynamics (interpolate (min time 60) score-amp))
	     (rhythm (if (< time 4)
			 .25
		       (funcall (funcall score-rhythm (cond ((< 36 time 48) 30)
							    ((> time 50) (+ time 8))
							    (t time)))
				'line line))
		     rhythm
		     rhythm)
	     (indisp-fun (funcall score-indisp 'time time))
	     (sound (nth 6 sound-list1))
	     (duration 0.1)
	     (tim-mult (- 5 (* line 2.5)))
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1))))
		      amp-val
		      (- 1 amp-val))
	     (srt (funcall (funcall score-srt time) 'amp-val amp-val 'line line)
		  2
 		  1)
	     (amp (if (> time 36) (* 1 amp-val (- 1 dynamics)) (* amp-val dynamics))
		  amp
		  (* 1 amp-val3 (- 1 dynamics))) 
	     (out-channels 3)
	     (degree 0 240 120)))))

(unpack_3chan_file "minute_6_1")

;; *** minute 6 part 2

;;; background noise
(wsound "minute_6_2"
  (let* (;(sound-list (reverse (data (getf *soundfiles* :noise))))
	 (sound-list1 (reverse (data (getf *soundfiles* :percussive)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (sixth (access-minutes)))))
      (declare (ignore start-times score-time-mult score-srt))
      (fplay 0 80
	     (dynamics (interpolate (min time 60) score-amp))
	     (rhythm (* 3/13 (mirrors (abs (sin (* time 1/2))) .2 1)))
	     (indisp-fun (funcall score-indisp 'time time))
	     (sound (nth 6 sound-list1))
	     (duration .2)
	     (tim-mult line)
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1))))
		      amp-val
		      (- 1 amp-val))
	     (srt 23 25 24 1)
	     (start (* (- (mirrors line 0 4/5) 2/3) .2))
	     (amp (* amp-val dynamics .0007)
		  amp
		  (* .07 amp-val3 (- 1 dynamics) .007)
		  0)
	     (out-channels 3)
	     (degree 240 120 0)))))

(unpack_3chan_file "minute_6_2")

;; *** minute 6 part 3

#|
(wsound "minute_6_3"
  (let* (;(sound-list (reverse (data (getf *soundfiles* :noise))))
	 (sound-list1 (reverse (data (getf *soundfiles* :percussive)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (sixth (access-minutes)))))
      (declare (ignore start-times score-time-mult score-srt))
      (fplay 0 80
	     (dynamics (interpolate (min time 60) score-amp))
	     (rhythm (* 3/13 (mirrors (abs (sin (* time 1/2))) .2 1)))
	     (indisp-fun (funcall score-indisp 'time time))
	     (sound (nth 6 sound-list1))
	     (duration .2)
	     (tim-mult line)
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1))))
		      amp-val
		      (- 1 amp-val))
	     (srt 1 25 24 1)
	     (start (* (- (mirrors line 0 4/5) 2/3) .2))
	     (amp (* amp-val .0007)
		  amp
		  (* .07 amp-val3 .007))
	     (out-channels 3)
	     (degree 240 120 0)))))

(unpack_3chan_file "minute_6_3")
|#

;; *** minute 6 part 4

;;; rhythmic pattern that dissovles 

(wsound "minute_6_4_1"
  (let* ((sound-list1 (reverse (data (getf *soundfiles* :percussive))))
	 (pattern '(0 0 1 1 1 1 0 0 0 1 0 1 1 0 0 1)))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
 	(interpret-tape (first (layers (sixth (access-minutes)))))
      (declare (ignore start-times score-time-mult score-indisp score-rhythm))
      (fplay 0 80
	     (dynamics (interpolate (min time 60) score-amp))
	     (rhythm .125 .25 (if (< time 30) .125 (* .125 (1+ (* (- line .5) .1)))))
	     (indisp-fun (lambda (x) (nth-mod (round x) pattern)))
	     (sound (nth 6 sound-list1))
	     (duration 0.1)
	     (tim-mult (- 5 (* line 2.5)))
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1)))))
	     (srt 2
		  (mirrors (funcall (funcall score-srt time) 'amp-val amp-val 'line line) .25 1)
 		  1)
	     (amp (funcall indisp-fun (* time (/ 1 rhythm)))
		  (funcall indisp-fun (* time2 (/ 1 rhythm2)))
		  (funcall indisp-fun (* time3 (/ 1 rhythm3)))) 
	     (out-channels 3)
	     (degree 120 0 240)))))

(unpack_3chan_file "minute_6_4_1")

;;; minute_6_4_1" but with clicks
(wsound "minute_6_4_2"
  (let* ((sound-list1 (reverse (data (getf *soundfiles* :percussive))))
	 (sound-list (reverse (data (getf *soundfiles* :noise))))
	 (pattern '(0 0 1 1 1 1 0 0 0 1 0 1 1 0 0 1)))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (sixth (access-minutes)))))
      (declare (ignore start-times score-time-mult score-indisp score-rhythm))
      (fplay 0 80
	     (dynamics (interpolate (min time 60) score-amp))
	     (rhythm .25 .125 (if (< time 30) .25 (* .25 (1+ (* (- line .5) .1)))))
	     (indisp-fun (lambda (x) (nth-mod (round x) pattern)))
	     (sound (nth 6 sound-list))
	     (duration 0.01)
	     (tim-mult (- 5 (* line 2.5)))
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1)))))
	     (srt .5
		  (funcall (funcall score-srt time) 'amp-val amp-val 'line line)
 		  1)
	     (amp (funcall indisp-fun (* time (/ 1 rhythm)))
		  (funcall indisp-fun (* time2 (/ 1 rhythm2)))
		  (funcall indisp-fun (* time3 (/ 1 rhythm3)))) 
	     (out-channels 3)
	     (degree 120 0 240)))))

(unpack_3chan_file "minute_6_4_2")

;; *** minute 6 part 5

;;; rhythmic pattern that dissovles faster than 6_4_1
(wsound "minute_6_5_1"
  (let* ((sound-list1 (reverse (data (getf *soundfiles* :percussive))))
	 (pattern '(0 0 1 1 1 1 0 0 0 1 0 1 1 0 0 1)))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (sixth (access-minutes)))))
      (declare (ignore start-times score-time-mult score-indisp score-rhythm
		       score-srt))
      (fplay 0 60
	     (dynamics (interpolate (min time 60) score-amp))
	     (rhythm .125
		     (if (< time 5) .25 (* .25 (1+ (* (- line 5/60) .1))))
		     (if (< time 5) .125 (* .125 (1+ (* (- line 5/60) .15)))))
	     (indisp-fun (lambda (x) (nth-mod (round x) pattern)))
	     (sound (nth 6 sound-list1))
	     (duration 0.1)
	     (tim-mult (- 5 (* line 2.5)))
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1)))))
	     (srt (mirrors (sin (* line 10)) 1 3)
		  (mirrors (sin (* line2 36)) .25 1)
 		  (mirrors (sin (* line3 25)) .75 1.5))
	     (amp (funcall indisp-fun (* time (/ 1 rhythm)))
		  (funcall indisp-fun (* time2 (/ 1 rhythm2)))
		  (funcall indisp-fun (* time3 (/ 1 rhythm3)))) 
	     (out-channels 3)
	     (degree 120 0 240)))))

(unpack_3chan_file "minute_6_5_1")

;;; minute_6_5_1" but with clicks
(wsound "minute_6_5_2"
  (let* ((sound-list1 (reverse (data (getf *soundfiles* :percussive))))
	 (sound-list (reverse (data (getf *soundfiles* :noise))))
	 (pattern '(0 0 1 1 1 1 0 0 0 1 0 1 1 0 0 1)))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (sixth (access-minutes)))))
      (declare (ignore start-times score-time-mult score-indisp score-rhythm))
      (fplay 0 60
	     (dynamics (interpolate (min time 60) score-amp))
	     (rhythm (if (< time 5) .25 (* .25 (1+ (* (- line 5/60) .2))))
		     (if (< time 5) .125 (* .125 (1+ (* (- line 5/60) .09))))
		     .25)
	     (indisp-fun (lambda (x) (nth-mod (round x) pattern)))
	     (sound (nth 6 sound-list))
	     (duration 0.01)
	     (tim-mult (- 5 (* line 2.5)))
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1)))))
	     (srt .5
		  (funcall (funcall score-srt time) 'amp-val amp-val 'line line)
 		  1)
	     (amp (funcall indisp-fun (* time (/ 1 rhythm)))
		  (funcall indisp-fun (* time2 (/ 1 rhythm2)))
		  (funcall indisp-fun (* time3 (/ 1 rhythm3)))) 
	     (out-channels 3)
	     (degree 120 0 240)))))

(unpack_3chan_file "minute_6_5_2")

#|
(wsound "minute_6_5"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise))))
	 (sound-list1 (reverse (data (getf *soundfiles* :percussive)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (sixth (access-minutes)))))
      (declare (ignore start-times score-time-mult))
      (fplay 0 60
	     (magic 10 13 16)
	     (test (< (mod time magic) 2.5)
		   (< (mod time2 magic2) 2)
		   (< (mod time3 magic3) 3))
	     (duration .01)
	     (srt 0.5)
	     (sound (if test (nth 6 sound-list) (nth 6 sound-list1))
		    (if test2 (nth 6 sound-list) (nth 6 sound-list1))
		    (if test3 (nth 6 sound-list) (nth 6 sound-list1)))
	     (rhythm (if test 1/13 1/6)
		     (if test2 1/13 1/6)
		     (if test3 1/13 1/6))
	     (amp (if test 1 1/600)
		  (if test2 1 1/600)
		  (if test3 1 1/600))
	     (out-channels 3)
	     (degree 0 120 240)))))
|#

(wsound "minute_6_5_3"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise))))
	 (sound-list1 (reverse (data (getf *soundfiles* :percussive)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (sixth (access-minutes)))))
      (declare (ignore start-times score-time-mult score-indisp score-rhythm
			 score-srt score-amp))
      (fplay 0 60
	     (magic 10 13 16)
	     (test (< (mod time magic) 2.5)
		   (< (mod time2 magic2) 2)
		   (< (mod time3 magic3) 3))
	     (duration .01)
	     (srt 0.5)
	     (sound (nth 6 sound-list))
	     (rhythm 1/13 1/13 1/13)
	     (amp (if test 1 0)
		  (if test2 1 0)
		  (if test3 1 0))
	     (out-channels 3)
	     (degree 0 120 240)))))

(unpack_3chan_file "minute_6_5_3")

;; ** minute 7

;; rthm = 1/13, rqq rhythm with 13 divisions
;; difference in amplitude is increased, until smaller amp values go into the negative
(wsound "minute_7_sides"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise))))
	 (end-time 60))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (seventh (access-minutes)))))
      (declare (ignore start-times score-rhythm score-srt score-time-mult))
      (fplay 0 end-time
	     (sound (nth 6 sound-list))
	     (rhythm 1/13 3/13)
	     (indisp-fun (funcall score-indisp 'time time))
	     (srt .5)
	     (duration .01)
	     (dynamics (interpolate (min time 60) score-amp)
		       (interpolate (min time2 60) score-amp))
	     (amp (* (- 1 (* (1+ (funcall indisp-fun (mod time 1)))
			     (+ .05 (* (min (* line (/ end-time 60)) 1) .125))))
		     dynamics)
		  (* (- 1 (* (1+ (funcall indisp-fun (mod time2 1)))
			     (+ .05 (* (min (* line2 (/ end-time 60)) 1) .125))))
		     dynamics))
	     (degree 0 90)))))

;; mostly the same as above but altered to add a mess on mid channel.
(wsound "minute_7_mid"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise))))
	 (end-time 80))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (seventh (access-minutes)))))
      (declare (ignore start-times score-rhythm score-srt score-time-mult score-amp))
      (fplay 0 end-time
	     (sound (nth 6 sound-list))
	     (rhythm 1/12 1/14 1/20) ;;(rhythm 2/13 4/13 3/26)
	     (indisp-fun (funcall score-indisp 'time time))
	     (srt .5)
	     (duration .008)
	     (dynamics (min (* line (/ end-time 60)) 1)
		       (max (min (- (* (min (* line2 (/ end-time 60)) 1) 1.5) .5) 1) 0)
		       (max (min (- (* (min (* line3 (/ end-time 60)) 1) 2) .7) 1) 0))
	     (amp (* (- 1 (* (1+ (funcall indisp-fun (mod time 1)))
			     (+ .05 (* (min (* line (/ end-time 60)) 1) .125))))
		     dynamics)
		  (* (* (1+ (funcall indisp-fun (mod time2 1)))
			(+ .05 (* (min (* line2 (/ end-time 60)) 1) .125)))
		     dynamics2)
		  (* (- 1 (* (1+ (funcall indisp-fun (mod time2 1)))
			     (+ .05 (* (min (* line3 (/ end-time 60)) 1) .125))))
		     dynamics3))
	     (out-channels 1)
	     (degree 0)))))

;; ** minute 8

;; start with rthm = 1/13 and an rqq rhythm with 13 divisions
;; left channel accelerates (sides) while right channel decelerates (mid)
(wsounds "minute_8" 
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (eighth (access-minutes)))))
      (declare (ignore start-times score-rhythm score-srt score-time-mult
		       score-amp))
      (fplay 0 90
	     (sound (nth 6 sound-list))
	     (rhythm (* (/ 1 13) (- 1 (* .5 line)))
		     (* (/ 1 13) (+ 1 (* 1 line))))
	     (indisp-fun (funcall score-indisp 'time time))
	     (srt .5)
	     (duration .01)
	     (tim-mult (+ 1 (* line 5)))
	     ;; do the volume automation manually in PD
	     ;; (dynamics (interpolate (min time 60) score-amp)
	     ;; 	       (interpolate (min time2 60) score-amp))
	     (dynamics 1 1)
	     (amp (* (/ 1 (1+ (funcall indisp-fun (mod time 1)))) dynamics)
		  (* (/ 1 (1+ (funcall indisp-fun (mod time2 1)))) dynamics2))
	     (out-channels 3)
	     (degree 180 0)))))

(unpack_3chan_file "minute_8")

;; ** minute 9

(loop for part from 0 to 1 do
  (wsound (format nil "minute_9_~a" part)
    (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
      (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			    score-amp score-time-mult)
	  (interpret-tape (first (layers (ninth (access-minutes)))))
	(declare (ignore score-time-mult score-srt score-amp))
	(fplay 0 90
	       (sound (nth 6 sound-list))
	       (duration .01)
	       ;; start second voice 4.5 beats later, etc.
	       (time 0 (* 4.5 60/72) (* 9.7 60/72))
	       (sect (section-val time
				  (nth-mod 0 start-times)
				  0
				  (nth-mod 1 start-times)
				  1
				  (nth-mod 2 start-times)
				  2
				  (+ (nth-mod 3 start-times) 10)
				  3))
	       (test (if (or (= sect 1) (= sect 3)) 0 1))
	       (mute-voice (if (= part 0) test (abs (1- test))))
	       (rhythm (cond
			 ;; first sect
			 ((= sect 0)
			  1)
			 ;; second sect 
			 ((= sect 1)
			  (funcall (funcall score-rhythm time)
				   'line line))
			 ;; quantising to 60 bpm (later adjust to 72)
			 ((= sect 2)
			  (* (+ 60/72 (- (/ (ceiling (* time 72/60)) 72/60) time))
			     72/60))
			 ;; fourth sect
			 ((= sect 3)
			  (funcall (funcall score-rhythm time)
				   'line line))))
	     
	       ;; adjust for tempo 72
	       (stub (setf rhythm  (* rhythm  60/72))
		     (when (< 30 time2 32) (setf time2 (+ time2 300/72)))
		     (when (< 36 time3 38) (setf time3 (+ time3 100/72))))
	       (indisp-fun (funcall score-indisp 'time time))
	       (tim-mult (- 5 (* line 2.5)))
	       (amp (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1)))
		       mute-voice))
	       (out-channels 3)
	       (degree 0 120 240))))))

(unpack_3chan_file "minute_9_0")
(unpack_3chan_file "minute_9_1")

;; ** minute 10

;; no minute 10 anymore

#+nil(wsound "minute_10"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (tenth (access-minutes)))))
      (fplay 0 60
	     (sound (nth 6 sound-list))
	     (rhythm (funcall (funcall score-rhythm time) 'line line))
	     (indisp-fun (funcall score-indisp 'time time))
	     (duration .01)
	     (tim-mult (- 5 (* line 2.5)))
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1)))))
	     (srt (funcall (funcall score-srt time) 'amp-val amp-val 'line line))
	     (amp (* (interpolate time score-amp) amp-val))
	     (degree 45)))))

;; ** minute 11

(wsound "minute_11"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (nth 10 (access-minutes)))))
	(declare (ignore start-times score-amp score-time-mult score-srt
			 score-rhythm))
	(fplay 0 80
	     (sound (nth 6 sound-list))
	     (duration .01)
	     (rhythm (* 1/20 (1+ (* line 13))))
	     (indisp-fun (funcall score-indisp 'time time))
	     (tim-mult (- 5 (* line .5)))
	     (amp (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1)))))
	     (degree 45)))))


;;; reverse minute 11
(wsound "minute_11_reverse"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (nth 10 (access-minutes)))))
	(declare (ignore start-times score-amp score-time-mult score-srt
			 score-rhythm))
	(fplay 0 80
	     (sound (nth 6 sound-list))
	     (duration .01)
	     (rhythm (* 1/20 (1+ (* (- 1 line) 13))))
	     (indisp-fun (funcall score-indisp 'time time))
	     (tim-mult (- 5 (* (- 1 line) .5)))
	     (amp (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1)))))
	     (degree 45)))))

;; EOF tape-score.lsp
