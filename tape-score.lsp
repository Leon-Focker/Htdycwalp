;; * tape-score

(in-package :ly)

;; ** minute 1

;;; simple impulse train with indispensability amplitudes:
(wsound "minute_1_t"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
     (fplay 0 60
	    (sound (nth 6 sound-list))
	    (rhythm .1)
	    (indisp-fun (rqq-to-indispensability-function
			 '(2 (1 (1 (1 1)))) t))
	    (srt .5)
	    (duration .01)
	    (tim-mult (+ 1 (* line 5)))
	    (amp (* (funcall (sections 0 1.6
				       10 (if (= 0 (mod i 2))
					      (funcall indisp-fun (mod (* time tim-mult) 1)) 2)
				       22.85 (funcall indisp-fun (mod (* time tim-mult) 1))
				       56.6 2)
			     time)
		    0.5 ))
	    (degree 45))))

(wsound "minute_2_t"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
     (fplay 0 60
	    (sound (nth 6 sound-list))
	    (rhythm .1)
	    (indisp-fun (rqq-to-indispensability-function
			 '(2 (1 (2 (1 1)))) t))
	    (srt .5)
	    (duration .01)
	    (tim-mult (+ 1 (* line 5)))
	    (amp (funcall indisp-fun (mod (* time tim-mult) 1)))
	    (degree 45))))

;; ** minute 2
;; ** minute 3
;; ** minute 4
;; ** minute 5
;; ** minute 6
;; ** minute 7
;; ** minute 8
;; ** minute 9
;; ** minute 10
;; ** minute 11

;; EOF tape-score.lsp
