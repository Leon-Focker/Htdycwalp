;; * tape-score

(in-package :ly)

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
	     (degree 45)))))

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
	     (degree 45)))))

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
	     (degree 45)))))

;; ** minute 4

;; simple, steady pulse train
;; rthm = 1/13, rqq rhythm with 13 units divided by 3 5 2 3.
;; the srt is slowly going up and the soundfile is rhythmically changed.
(wsound "minute_4"
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
	     (rhythm (funcall (funcall score-rhythm time) 'line line))
	     (srt (interpolate line '(0 .5  1 2) :warn nil))
	     (duration .01)
	     (amp (* 1/13 (1+ (funcall indisp-fun (mod time 1)))))
	     (degree 45)))))

;; ** minute 5

;; pulses with changing rhythm, depending on indisp-fun * .25
;; haas effect on second channel.
;; sounds are chosen in the same order, when to chose a new sound is determined
;; by indisp-fun * .2
;; start is the loudest sample in the soundfile
;; srt goes up from .5 to 8

;; 
(wsound "minute_5"
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
	     (duration 0.01)
	     (time-mult (funcall (funcall score-time-mult time)))
	     (amp (if (or (<= 44 time 48) (> time 58))
		      1
		      (* 1/13 (1+ (funcall indisp-fun (mod (* time time-mult) 1)))
			 (/ 1 (peak sound))))
		  amp
		  (if (<= time3 48) amp 0))
	     (degree 0 90 30 60)))))

;; ** minute 6

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
			       44 (* amp-val dynamics .07))
		  amp
		  (section-val time
			       0 (* .07 amp-val3 (- 1 dynamics))
			       20 0
			       24 (dry-wet (* .07 amp-val3 (- 1 dynamics)) 0 (/ (- time3 20) 20))
			       40 (* .07 amp-val3 (- 1 dynamics) .07))
		  (section-val time
			       0 0
			       24 (- 1 (* amp3 20))
			       40 0))
	     (degree 0 0 90 45)))))

;; ** minute 7

;; rthm = 1/13, rqq rhythm with 13 divisions
;; difference in amplitude is increased, until smaller amp values go into the negative
(wsound "minute_7"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (seventh (access-minutes)))))
      (setf *test* score-indisp)
      (fplay 0 60
	    (sound (nth 6 sound-list))
	    (rhythm 1/13 3/13)
	    (indisp-fun (funcall score-indisp 'time time))
	    (srt .5)
	    (duration .01)
	    (tim-mult (+ 1 (* line 5)))
	    (dynamics (interpolate (min time 60) score-amp)
		      (interpolate (min time2 60) score-amp))
	    (amp (* (- 1 (* (1+ (funcall indisp-fun (mod time 1))) (+ .05 (* line .125))))
		    dynamics)
		 (* (- 1 (* (1+ (funcall indisp-fun (mod time2 1))) (+ .05 (* line2 .125))))
		    dynamics))
	    (degree 0 90)))))

;; ** minute 8

;; start with rthm = 1/13 and an rqq rhythm with 13 divisions
;; left channel accelerates (sides) while right channel decelerates (mid)
(wsound "minute_8"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (eighth (access-minutes)))))
      (fplay 0 60
	     (sound (nth 6 sound-list))
	     (rhythm (* (/ 1 13) (- 1 (* .5 line)))
		     (* (/ 1 13) (+ 1 (* 1 line))))
	     (indisp-fun (funcall score-indisp 'time time))
	     (srt .5)
	     (duration .01)
	     (tim-mult (+ 1 (* line 5)))
	     (dynamics (interpolate (min time 60) score-amp)
		      (interpolate (min time2 60) score-amp))
	     (amp (* (/ 1 (1+ (funcall indisp-fun (mod time 1)))) dynamics)
		  (* (/ 1 (1+ (funcall indisp-fun (mod time2 1)))) dynamics2))
	     (degree 0 90)))))

;; ** minute 9

(wsound "minute_9"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (ninth (access-minutes)))))
      (fplay 0 60
	     (sound (nth 6 sound-list))
	     (rhythm (funcall (funcall score-rhythm time) 'line line))
	     (indisp-fun (funcall score-indisp 'time time))
	     (indisp-fun (lambda (x) x))
	     (duration .01)
	     (tim-mult (- 5 (* line 2.5)))
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1)))))
	     (srt (funcall (funcall score-srt time) 'amp-val amp-val 'line line))
	     (amp (* (interpolate time score-amp) amp-val))
	     (degree 45)))))

;; ** minute 10

(wsound "minute_10"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (start-times score-indisp score-rhythm score-srt
			  score-amp score-time-mult)
	(interpret-tape (first (layers (tenth (access-minutes)))))
      (fplay 0 60
	     (sound (nth 6 sound-list))
	     (rhythm (funcall (funcall score-rhythm time) 'line line))
	     (indisp-fun (funcall score-indisp 'time time))
	     (indisp-fun (lambda (x) x))
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
      (fplay 0 60
	     (sound (nth 6 sound-list))
	     (rhythm (funcall (funcall score-rhythm time) 'line line))
	     (indisp-fun (funcall score-indisp 'time time))
	     (indisp-fun (lambda (x) x))
	     (duration .01)
	     (tim-mult (- 5 (* line 2.5)))
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1)))))
	     (srt (funcall (funcall score-srt time) 'amp-val amp-val 'line line))
	     (amp (* (interpolate time score-amp) amp-val))
	     (degree 45)))))

;; EOF tape-score.lsp
