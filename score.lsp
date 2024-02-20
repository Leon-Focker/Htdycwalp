;; * score

(in-package :ly)

;; ** divide (generate divisions and states)

;; ;;; number of divisions:
;; (kernel-transitions 11 7 '(1 -2 3))
;; (defparameter *div-cnt* (window-transitions 11 '(2 3 4 7 9 5 2) .5 .5))

;; ;;; division ratios:
;; (defparameter *divisions* (procession (apply #'+ *div-cnt*) '(1 2 3 4 5)))

(let* ((number-of-division-seeds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 '((2 3 4 7 9 5 2)
	   (2 3 7 4 9 2 5)
	   (2 3 7 4 5 3 2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (division-ratios-seed '(1 2 3 4 5)))
  (loop for seed in number-of-division-seeds and i from 0
	for nr-of-divs = (window-transitions (length (access-minutes)) seed .5)
	for divs = (procession (apply #'+ nr-of-divs) division-ratios-seed)
	do (distribute-divs divs nr-of-divs (access-minutes) i)))

;; VISUALIZE MINUTES
(visualize-minutes (access-minutes) '(0 1 2 3) "/E/code/ensemble/test_wts" 1/4 nil)

;; update start-times of all layers just to be sure:
(loop for i in (access-minutes) do (update-layer-start-times i))

;; ** ...and conquer (handle the minute objects)

;; interpret layers returns a function that can then be filled with arguments
;;  to form a sections like function.


;; EOF score.lsp

