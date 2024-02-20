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
   (division-ratios :accessor div-ratios :initarg :div-ratios :initform '(1))
   ;; every division of the minute will have a state, which is a number
   ;; depending on the type of layer this number can be something like static,
   ;; isorhythmic etc.
   (states :accessor states :initarg :states :initform '(0))))

(defclass tape-layer (minute-layer)
  ())
    
(defclass instrument-layer (minute-layer)
  ((instruments :accessor instruments :type list :initarg :instruments
		:initform '())))

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

;; 
(defmethod interpret-layer ((tl tape-layer)
			    &rest keyargs &key &allow-other-keys)
  )

;; (defun test (nr)
;;   (lambda (&rest args) (unless (= nr (length args)) (warn "wrong nummber of arguments: ~a" (length args)))))


(defmethod interpret-layer ((il instrument-layer)
			    &rest keyargs &key &allow-other-keys)
  )

;; ** make

(defun make-minute (id start-time &optional (layers '()))
  (make-instance 'minute :id id :start-time start-time :layers layers))

(defun make-tape-layer (id-uniquifier number start-time
			&optional (div-ratios '(1)) (states '(0)))
  (unless (integerp number) (error "number must be an integer, not ~a" number))
  (make-instance 'tape-layer
		 :id (intern (format nil "tape-layer-~a" id-uniquifier) :ly)
		 :number number
		 :start-time start-time
		 :div-ratios div-ratios
		 :states states))

(defun make-instrument-layer (id-uniquifier number start-time
			      &optional (div-ratios '(1)) (states '(0)))
  (unless (integerp number) (error "number must be an integer, not ~a" number))
  (make-instance 'instrument-layer
		 :id (intern (format nil "instr-layer-~a" id-uniquifier) :ly)
		 :number number
		 :start-time start-time
		 :div-ratios div-ratios
		 :states states))

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

(defun visualize-minutes (list-of-minutes list-of-layer-numbers
			  path-and-filename
			  &optional (size-factor 1) (specify-time-and-layer t))
  (format t "~&visualizing minutes...")
  (imago::show-minutes list-of-minutes list-of-layer-numbers path-and-filename
		       size-factor specify-time-and-layer))

;; ** minutes

;;; Michael Edwards challenged me to not use a single global variable, so I 
;;; guess I'm going to use closures:
;;; minutes holds all 11 minute objects, which are filled with tape and
;;; instrument-layers.
;;; number-of-divisions stores the total number of parts in each layer.
;;; lnrs is a plist with the ids/numbers for the layers. First is tape layer,
;;; the rest are instrument-layers.
(let ((minutes '())
      (number-of-divisions '())
      (l-nrs '(0 1 2 3)))
  ;; init minutes:
  (setf minutes
	(loop for i from 0 to 10
	      for time = (* i 60)
	      for tape-layer = (make-tape-layer (car l-nrs) (car l-nrs) time)
	      for instrument-layers = '()
	      do (setf instrument-layers
		       (loop for k in (cdr l-nrs) ; net numbers for layers (1-3)
			     collect (make-instrument-layer k k time)))
	      collect (make-minute i time
				   (cons tape-layer instrument-layers))))
  ;; init number-of-divisions:
  (loop for nr in (reverse l-nrs)
	for layers = (get-related-minute-layers minutes nr #'error)
	for len = (length (loop for i in layers append (div-ratios i)))
	do (push-key-value nr len number-of-divisions))
  ;; closures to access minutes and co everywhere:
  (defun access-minutes () minutes)
  (defun (setf access-minutes) (new-value) (setf minutes new-value))
  (defun minutes-nod () number-of-divisions)
  (defun (setf minutes-nod) (new-value) (setf number-of-divisions new-value))
  (defun minutes-layer-numbers () l-nrs))

;; add undivided layers for reference when visualizing, number 111
(loop for minute in (access-minutes) and i from 0
      do (setf (layers minute)
	       (append (layers minute) `(,(make-tape-layer 111 111 (* i 60))))))

;; EOF mintues.lsp
