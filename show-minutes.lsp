;; * show-minutes

;; This is a modified version of ly::show-structure

(ql:quickload :imago)

(in-package :imago)

(defun show-minutes (list-of-minutes list-of-layer-numbers path-and-filename
		     &optional (size-factor 1))
  (let* ((h (* 100 size-factor))
	 (w (* 1000 size-factor))
	 (ls (loop for nr in list-of-layer-numbers
		   for durs = (ly::get-all-related-durations list-of-minutes nr 'error)
		   collect (mapcar
			    (lambda (x) (sc::rescale x 0 (apply #'+ durs) 0 1))
			    durs)))
	 (length (length ls))
	 (height (* h length))
	 (array (make-array `(,height ,w)))
	 (n 0)
	 (sublist '())
	 (sublength 0)
	 (index 0)
	 (color 0)
	 (element 0)
	 (name (format nil "~a~a" path-and-filename ".png")))
    (loop for y from 0 to (1- height) do
	 (setf n (floor y h))
	 (setf sublist (nth n ls))
	 (setf sublength (length sublist))
	 (loop for x from 0 to (1- w) do
	      (setf index (sc::decider (/ x w) sublist))
	      (setf element (nth index sublist))
	      (setf color (make-color (floor (* (expt (- 1 (/ element 1)) 15) 255))
				      (floor (* (/ n length) 100))
				      (floor (* (/ index sublength) 200))))
	      (setf (aref array y x) color)))
    (write-png
     (make-instance 'rgb-image
		    :pixels array)
     name)
    name))

(in-package :layers)

;; EOF show-minutes.lsp
