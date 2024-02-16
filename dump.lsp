

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


;; * screaches

(wsound "screach_6_st"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
     (fplay 0 3
	    (sound (nth 6 sound-list))
	    (start (+ 0.2 line) (+ 0.21 (* line .099)))
	    (rhythm (interpolate line '(0 .015  1 .03) :warn nil))
	    (indisp-fun (rqq-to-indispensability-function
			  '(13 (3 (3 (1 1 1)) (3 (2 1)) (4 (1 1 1))))))
	    (srt (* .5 (+ (* line 2) 1)))
	    (duration (interpolate line '(0 .05  1 .03) :warn nil))
	    (tim-mult (+ 1 (* line 5)))
	    (amp (* (- 1 (* .1 (funcall indisp-fun (mod (* time tim-mult) 1)))) .25))
	    (degree 0 90))))

(wsound "screach_1_st-"
  (let* ((sound-list (reverse (data (getf *soundfiles* :noise)))))
     (fplay 0 3
	    (sound (nth 1 sound-list))
	    (start (+ 0.2 line) (+ 0.21 (* line .099)))
	    (rhythm (interpolate line '(0 .015  1 .03) :warn nil))
	    (indisp-fun (rqq-to-indispensability-function
			  '(13 (3 (3 (1 1 1)) (3 (2 1)) (4 (1 1 1))))))
	    (srt (* .5 (+ (* line 2) 1)))
	    (duration (interpolate line '(0 .05  1 .03) :warn nil))
	    (tim-mult (+ 1 (* line 5)))
	    (amp (* (- 1 (* .1 (funcall indisp-fun (mod (* time tim-mult) 1)))) .25))
	    (degree 0 90))))
