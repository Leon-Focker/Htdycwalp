;; * tape-score

(in-package :ly)

;; ** trying to use indispensability:

;; make a structure:
(defparameter *indisp*
  (data (make-fractal-structure '(2 3 2)
				'((1 ((3 3)))
				  (2 ((3 1)))
				  (3 ((2 1 2))))
				'((2 2)
				  (3 3)
				  (1 2))
				:duration 20
				:type 'compartmentalise
				:smallest .1)))

(defparameter *indisp-amp*
  (loop for i from 0 to 2 collect
       (apply-indispensability (nth 3 *indisp*) (nth i *indisp*)
			       (rqq-to-indispensability-function
				'(1 ((2 (1 1)) (3 (1 1 1)) (2 (1 1))))))))

(defparameter *indisp-amp*
  (loop for i from 0 to 2 collect
       (apply-indispensability (nth 3 *indisp*) (nth i *indisp*)
			       (rqq-to-indispensability-function
				'(3 ((1 (1 1 1)) (1 (1 1 1)) (1 (1 1 1))))))))

;; without indisp amps:
(with-sound (:header-type clm::mus-riff :samplingq-rate 48000
	     :output (format nil "~a~a" +ens-src-dir+ "indisp_test.wav")
	     :channels 2 :play nil :scaled-to 0.98
	     :force-recomputation nil)
  (let* ((sound-list (reverse (data *noise*))))
    (fplay 0 20
	   (sound (nth 6 sound-list))
	   (rhythm (nth-mod i (nth 3 *indisp*))
		   (nth-mod i (nth 2 *indisp*))
		   (nth-mod i (nth 1 *indisp*)))
	   (srt .5 1 2)
	   (duration .01 .02 .01)
	   (amp .8 .3 .2)
	   (degree 45 0 90))))

;; with indisp-amps:
(with-sound (:header-type clm::mus-riff :samplingq-rate 48000
	     :output (format nil "~a~a" +ens-src-dir+ "indisp_test!2.wav")
	     :channels 2 :play nil :scaled-to 0.98
	     :force-recomputation nil)
  (let* ((sound-list (reverse (data *noise*))))
    (fplay 0 20
	   (sound (nth 6 sound-list))
	   (rhythm (nth-mod i (nth 3 *indisp*))
		   (nth-mod i (nth 2 *indisp*))
		   (nth-mod i (nth 1 *indisp*))
		   (nth-mod i (nth 0 *indisp*)))
	   (srt .5 1 2 4)
	   (duration .01 .02 .01 .0025)
	   (amp 1
		(/ 1 (1+ (nth-mod i (nth 2 *indisp-amp*))))
		(/ 1 (1+ (nth-mod i (nth 1 *indisp-amp*))))
		(/ 1 (1+ (nth-mod i (nth 0 *indisp-amp*)))))
	   (degree 45 0 90 45))))

;; pulse, no rhythm
(with-sound (:header-type clm::mus-riff :samplingq-rate 48000
	     :output (format nil "~a~a" +ens-src-dir+ "pulse_train.wav")
	     :channels 2 :play nil :scaled-to 0.98
	     :force-recomputation nil)
  (let* ((sound-list (reverse (data *noise*))))
    (fplay 0 20
	   (sound (nth 6 sound-list))
	   (rthm .1) 
	   (rhythm rthm
		   rthm)
	   (indisp-fun (rqq-to-indispensability-function
			'(1 ((2 (1 1)) (3 (1 2)) (2 (1 1 1 1))))))
	   (srt .5 .5)
	   (duration .01 .01)
	   (tim-mult (+ 1 (* line 5))
		     (+ 1 (* line2 4)))
	   (amp (funcall indisp-fun (mod (* time tim-mult) 1))
		(funcall indisp-fun (mod (* time2 tim-mult2) 1)))
	   (degree 0 90))))

(with-sound (:header-type clm::mus-riff :samplingq-rate 48000
	     :output (format nil "~a~a" +ens-src-dir+ "pulse_train_acc.wav")
	     :channels 2 :play nil :scaled-to 0.98
	     :force-recomputation nil)
  (let* ((sound-list (reverse (data *noise*))))
    (fplay 0 20
	   (sound (nth 6 sound-list))
	   (rthm (/ .05 (+ 1 line))) ;; try .1 and .05
	   (rhythm rthm
		   rthm)
	   (indisp-fun (rqq-to-indispensability-function
			'(1 ((2 (1 1)) (3 (1 2)) (2 (1 1 1 1))))))
	   (srt .5 .5)
	   (duration .01 .01)
	   (tim-mult (+ 1 (* line 5))
		     (+ 1 (* line2 4)))
	   (amp (funcall indisp-fun (mod (* time tim-mult) 1))
		(funcall indisp-fun (mod (* time2 tim-mult2) 1)))
	   (degree 0 90))))

(with-sound (:header-type clm::mus-riff :samplingq-rate 48000
	     :output (format nil "~a~a" +ens-src-dir+ "pulse_train_mono.wav")
	     :channels 2 :play nil :scaled-to 0.98
	     :force-recomputation nil)
  (let* ((sound-list (reverse (data *noise*))))
    (fplay 0 20
	   (sound (nth 6 sound-list))
	   (rthm .1)
	   (rhythm rthm
		   rthm)
	   (indisp-fun (rqq-to-indispensability-function
			'(1 ((2 (1 1)) (3 (1 2)) (2 (1 1 1 1))))))
	   (srt .5 2)
	   (duration .01 .01)
	   (tim-mult (+ 1 (* line 5))
		     (+ 1 (* line2 4)))
	   (amp (funcall indisp-fun (mod (* time tim-mult) 1))
		(funcall indisp-fun (mod (* time2 tim-mult2) 1)))
	   (degree 45))))

(with-sound (:header-type clm::mus-riff :samplingq-rate 48000
	     :output (format nil "~a~a" +ens-src-dir+ "pulse_train_mono2.wav")
	     :channels 2 :play nil :scaled-to 0.98
	     :force-recomputation nil)
  (let* ((sound-list (reverse (data *noise*))))
    (fplay 0 20
	   (sound (nth 6 sound-list))
	   (rthm .1)
	   (rhythm rthm
		   (/ rthm 2))
	   (indisp-fun (rqq-to-indispensability-function
			'(1 ((2 (1 1)) (3 (1 2)) (2 (1 1 1 1))))))
	   (srt .5 .5)
	   (duration .01 .01)
	   (tim-mult (+ 1 (* line 5))
		     (+ 1 (* line2 4)))
	   (amp (funcall indisp-fun (mod (* time tim-mult) 1))
		(funcall indisp-fun (mod (* time2 tim-mult2) 1)))
	   (degree 45))))

;; EOF tape-score.lsp
