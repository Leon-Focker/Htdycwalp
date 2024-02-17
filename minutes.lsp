;; * minutes

(in-package :ly)

;; ** classes

;;; one minute of the entire piece with mainly structural information
(defclass minute ()
  ((id :accessor id :initarg :id :initform nil)
   ;; start time of the minute in seconds
   (start-time :accessor start-time :type number :initarg :start-time
	       :initform 0)
   ;; list of tape-minutes and instrument-minutes
   (layers :accessor layers :type list :initarg :layers
	       :initform '())))

(defclass minute-layer ()
  ((id :accessor id :initarg :id :initform nil)
   ;; start time of the minute in seconds
   (start-time :accessor start-time :type number :initarg :start-time
	       :initform 0)
   (number :accessor number :type integer :initarg :number :initform 0)
   ;; ratios by which each minute will be divided
   ;; the duration for each division is = (* (/ 60 sum-of-ratios) ratio)
   (division-ratios :accessor div-ratios :initarg :div-ratios :initform '(1))))

(defclass tape-layer (minute-layer)
  ())
    
(defclass instrument-layer (minute-layer)
  ())

;; ** print objects

(defmethod print-object ((mn minute) stream)
  (format stream "<MINUTE ~a>" (id mn)))

(defmethod print-object ((mnl minute-layer) stream)
  (format stream "<MINUTE-LAYER ~a>" (id mnl)))

;; ** methods

(defmethod update-layer-start-times ((mn minute))
  (let ((time (start-time mn)))
    (loop for layer in (layers mn)
	  do (setf (start-time layer) time))
    mn))

(defmethod get-section-durations ((mnl minute-layer))
  (let* ((ratios (div-ratios mnl))
	 (sum (apply #'+ ratios)))
    (loop for ratio in ratios collect (* (/ 60 sum) ratio))))
  
(defmethod get-section-durations ((mn minute))
  (loop for layer in (layers mn)
	collect (get-section-durations layer)))

;; ** make

(defun make-minute (id start-time &optional (layers '()))
  (make-instance 'minute :id id :start-time start-time :layers layers))

(defun make-tape-layer (id-uniquifier number start-time
			&optional (div-ratios '(1)))
  (unless (integerp number) (error "number must be an integer, not ~a" number))
  (make-instance 'tape-layer
		 :id (intern (format nil "tape-layer-~a" id-uniquifier) :ly)
		 :number number
		 :start-time start-time
		 :div-ratios div-ratios))

(defun make-instrument-layer (id-uniquifier number start-time
			      &optional (div-ratios '(1)))
  (unless (integerp number) (error "number must be an integer, not ~a" number))
  (make-instance 'instrument-layer
		 :id (intern (format nil "instr-layer-~a" id-uniquifier) :ly)
		 :number number
		 :start-time start-time
		 :div-ratios div-ratios))

;; ** operations on lists of minutes

(defun get-related-minute-layers (list-of-minutes layer-number
				  &optional (error-fun #'warn))
  (loop with flag
	for minute in list-of-minutes
	for layer = (car (member layer-number (layers minute) :key 'number))
	unless layer do (setf flag t)
	  when layer collect layer into result
	    finally (when flag
		      (funcall error-fun "not all minutes had a layer with ~
                                          this number: ~a" layer-number))
		    (return result)))

(defun get-all-related-durations (list-of-minutes layer-number
				  &optional (error-fun #'warn))
  (loop for layer in (get-related-minute-layers list-of-minutes
						layer-number error-fun)
	    append (get-section-durations layer)))

;; ** visualize



;; ** minutes

;;; Michael Edwards challenged me to not use a single global variable, so I 
;;; guess I'm going to use closures:
(let ((minutes '()))
  ;; minutes holds all 11 minute objects, which are filled with tape and
  ;; instrument-layers
  (setf minutes
	(loop for i from 0 to 10
	      for time = (* i 60)
	      for tape-layer = (make-tape-layer 0 0 time)
	      for instrument-layers = '()
	      do (setf instrument-layers
		       (loop for k from 1 to 3 ; 1-3 to discern them from tape
			     collect (make-instrument-layer k k time)))
	      collect (make-minute i time
				   (cons tape-layer instrument-layers))))
  ;; closure to access minutes everywhere:
  (defun access-minutes ()
    minutes)
  (defun (setf access-minutes) (new-value)
    (setf minutes new-value)))

;; EOF mintues.lsp
