;; * Soundfiles
;;; collect and analyse soundfiles, store them in ly::stored-file-list objects.
;;; This is an easy way of handling soundfiles. Look into the layers-package for
;;; more documentation.

(in-package :ly)

(defparameter *soundfiles* '())

;; ** distorted

(setf-key-value *soundfiles*
		:distorted (make-stored-file-list 'distorted nil))
(setf-key-value *soundfiles*
		:distorted-txt
		(format nil "~a~a" +ens-src-dir+ "distorted.txt"))
(unless (probe-file (getf *soundfiles* :distorted-txt))
  (setf *re-analyse-soundfiles* t))

(when *re-analyse-soundfiles*
  (folder-to-stored-file-list
   (getf *soundfiles* :distorted)
   "/E/Keks_Feedback/samples/distorted/"
   :analyse t
   :auto-map nil
   :auto-scale-mapping nil
   :remap nil
   ;;:fft-size 4096
   :f1 #'(lambda (sf) (/ (log (/ (+ (dominant-frequency sf)
				    (centroid sf))
				 2))
			 12000))
   :f2 #'(lambda (sf) (* (expt (transient sf) 0.7)
			 0.6))
   :f3 #'(lambda (sf) (- 1 (expt (smoothness sf)
				 0.5))))
  (store-in-text-file (getf *soundfiles* :distorted)
		      (getf *soundfiles* :distorted-txt)))

(unless *re-analyse-soundfiles*
  (setf-key-value *soundfiles* :distorted
		  (load-from-file (getf *soundfiles* :distorted-txt))))

;; ** noise

(setf-key-value *soundfiles*
		:noise (make-stored-file-list 'noise nil))
(setf-key-value *soundfiles*
		:noise-txt (format nil "~a~a" +ens-src-dir+ "noise.txt"))
(unless (probe-file (getf *soundfiles* :noise-txt))
  (setf *re-analyse-soundfiles* t))

(when *re-analyse-soundfiles*
  (folder-to-stored-file-list
   (getf *soundfiles* :noise)
   "/E/Keks_Feedback/samples/noise/"
   :analyse t
   :auto-map nil
   :auto-scale-mapping nil
   :remap nil
   ;;:fft-size 4096
   :f1 #'(lambda (sf) (/ (log (/ (+ (dominant-frequency sf)
				    (centroid sf))
				 2))
			 12000))
   :f2 #'(lambda (sf) (* (expt (transient sf) 0.7)
			 0.6))
   :f3 #'(lambda (sf) (- 1 (expt (smoothness sf)
				 0.5))))
  (store-in-text-file (getf *soundfiles* :noise)
		      (getf *soundfiles* :noise-txt)))

(unless *re-analyse-soundfiles*
  (setf-key-value *soundfiles* :noise
		  (load-from-file (getf *soundfiles* :noise-txt))))

;; ** percussive
(setf-key-value *soundfiles*
		:percussive (make-stored-file-list 'percussive nil))
(setf-key-value *soundfiles*
		:percussive-txt
		(format nil "~a~a" +ens-src-dir+ "percussive.txt"))
(unless (probe-file (getf *soundfiles* :percussive-txt))
  (setf *re-analyse-soundfiles* t))
(when *re-analyse-soundfiles*
  (folder-to-stored-file-list
   (getf *soundfiles* :percussive)
   "/E/Keks_Feedback/samples/percussive/polished/"
   :auto-map t
   :auto-scale-mapping t
   :remap t
   ;;:fft-size 4096
   :f1 #'(lambda (sf) (/ (log (/ (+ (dominant-frequency sf)
				    (centroid sf))
				 2))
			 12000))
   :f2 #'(lambda (sf) (* (expt (transient sf) 0.7)
			 0.6))
   :f3 #'(lambda (sf) (- 1 (expt (smoothness sf)
				 0.5))))
  (store-in-text-file (getf *soundfiles* :percussive)
		      (getf *soundfiles* :percussive-txt)))

(unless *re-analyse-soundfiles*
  (setf-key-value *soundfiles*
		  :percussive
		  (load-from-file (getf *soundfiles* :percussive-txt))))

;; EOF soundfiles.lsp
