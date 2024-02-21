;; * show-minutes

;;; This is a modified version of ly::show-structure.
;;; #'show-minutes is not very nice but it does its job

(ql:quickload :imago)

(in-package :imago)

(defun show-minutes (list-of-minutes list-of-layer-numbers path-and-filename
		     &optional (size-factor 1) (specify-time-and-layer t))
  (let* ((h (floor (* 100  size-factor)))
	 (w (floor (* 1000 size-factor)))
	 (durations (loop for nr in list-of-layer-numbers
			  for durs = (ly::get-all-related-durations
				      list-of-minutes nr 'error)
			  collect (mapcar
				   (lambda (x) (sc::rescale x 0 (apply #'+ durs) 0 1))
				   durs)))
	 (states (loop for nr in list-of-layer-numbers
		       for layers = (ly::get-related-minute-layers
				     list-of-minutes nr 'error)
		       collect (loop for i in layers append (ly::states i))))
	 (nr-of-states (apply #'max (ly::flatten states)))
	 (length (length durations))
	 (height (* h length))
	 (array (make-array `(,height ,w)))
	 (n 0)
	 (sublist '())
	 (ssublist '())
	 (sublength 0)
	 (index 0)
	 (color 0)
	 (element 0)
	 (name (format nil "~a~a" path-and-filename ".png")))
    (loop for y from 0 to (1- height) do
	 (setf n (floor y h))
	 (setf sublist (nth n durations))
	 (setf ssublist (nth n states))
	 (setf sublength (length sublist))
	 (loop for x from 0 to (1- w) do
	      (setf index (sc::decider (/ x w) sublist))
	      (setf element (nth index sublist))
	      (setf color (make-color (floor (* (/ (nth index ssublist) nr-of-states) 255)) ;; state
				       ;; different layers
				      (if specify-time-and-layer (floor (* (/ n length) 100)) 100)
				      ;; time
				      (if specify-time-and-layer (floor (* (/ index sublength) 200)) 100)))
	      (setf (aref array y x) color)))
    (write-png
     (make-instance 'rgb-image
		    :pixels array)
     name)
    name))

;; EOF show-minutes.lsp
