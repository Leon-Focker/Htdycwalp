;; * tape-score

(in-package :ly)

;; ** minute 1
;; simple, steady pulse train
;; rthm = 1/13, rqq rhythm with 13 equal divisions
;; going slower through the indisp-function to get kind of iso-rhythmic
;; displacements.
(wsound "minute_1"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (score-indisp score-rhythm score-srt score-amp
			  score-time-mult)
	(interpret-tape (first (layers (first (access-minutes)))))
      (declare (ignore score-rhythm score-srt score-amp score-time-mult))
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
    (multiple-value-bind (score-indisp score-rhythm score-srt score-amp
			  score-time-mult)
	(interpret-tape (first (layers (second (access-minutes)))))
      (declare (ignore score-rhythm score-srt score-amp score-time-mult))
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
    (multiple-value-bind (score-indisp score-rhythm score-srt score-amp score-time-mult)
	(interpret-tape (first (layers (third (access-minutes)))))
      (declare (ignore score-rhythm))
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
(wsound "minute_4_Test"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (score-indisp score-rhythm score-srt score-amp
			  score-time-mult)
	(interpret-tape (first (layers (fourth (access-minutes)))))
      (fplay 0 60
	     ;; don't use dynamics, would be a kind of cresc anyways
	     ;; (dynamics (interpolate (min time 60) score-amp))
	     (indisp-fun (funcall score-indisp))
	     (sound (nth (case (funcall indisp-fun (* time 1/3))
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
;; sounds are chosen in the same order, when is determined by indisp-fun * .2
;; start is the loudest sample in the soundfile
;; srt goes up from .5 o 8
;; amp is indisp-fun * 1
(wsound "minute_5"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise))))
	 (cnt 0))
    (multiple-value-bind (score-indisp score-rhythm score-srt score-amp
			  score-time-mult)
	(interpret-tape (first (layers (fifth (access-minutes)))))
      (fplay 0 60
	     (indisp-fun (funcall score-indisp))
	     (stub (case (funcall indisp-fun (* time 1/5))
		     ((0 1 2 3) (incf cnt))))
	     (sound (nth-mod cnt sound-list))
	     (start (/ (peak-index sound) 48000))
	     (rhythm (case (funcall indisp-fun (* time .25))
		       ((0 1 2) 1/13)
		       ((3 4 5 6 7) .1)
		       ((8 9) 1/12)
		       (t .25))
		     (* rhythm 1.001))
	     (srt (interpolate line '(0 .5  1 8) :warn nil))
	     (duration .01)
	     (amp (* 1/13 (1+ (funcall indisp-fun (mod time 1)))))
	     (degree 0 90)))))

;; ** minute 6

(wsound "minute_6"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (score-indisp score-rhythm score-srt score-amp
			  score-time-mult)
	(interpret-tape (first (layers (sixth (access-minutes)))))
      (fplay 0 60
	     (dynamics (interpolate (min time 60) score-amp))
	     (sound (nth 6 sound-list))
	     (rhythm (funcall (funcall score-rhythm time) 'line line))
	     (indisp-fun (funcall score-indisp 'time time))
	     (indisp-fun (lambda (x) x))
	     (duration .01)
	     (tim-mult (- 5 (* line 2.5)))
	     (amp-val (* 1/13 (1+ (funcall indisp-fun (mod (* time tim-mult) 1)))))
	     (srt (funcall (funcall score-srt time) 'amp-val amp-val 'line line))
	     (amp (* (* (interpolate time score-amp) amp-val) dynamics))
	     (degree 45)))))

;; ** minute 7

;; rthm = 1/13, rqq rhythm with 13 divisions
;; difference in amplitude is increased, until smaller amp values go into the negative
(wsound "minute_7"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
    (multiple-value-bind (score-indisp score-rhythm score-srt score-amp score-time-mult)
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
    (multiple-value-bind (score-indisp score-rhythm score-srt score-amp score-time-mult)
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
    (multiple-value-bind (score-indisp score-rhythm score-srt score-amp score-time-mult)
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
    (multiple-value-bind (score-indisp score-rhythm score-srt score-amp score-time-mult)
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
    (multiple-value-bind (score-indisp score-rhythm score-srt score-amp score-time-mult)
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
