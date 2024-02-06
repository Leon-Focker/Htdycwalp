;; * Soundfiles
;;; collect and analyse soundfiles, store them in ly::stored-file-list objects.
;;; This is an easy way of handling soundfiles. Look into the layers-package for
;;; more documentation.

(in-package :ly)

;; ** distorted

(defparameter *distorted* (make-stored-file-list 'distorted nil))
(defparameter *distorted-txt* (format nil "~a~a" +ens-src-dir+ "distorted.txt"))
(unless (probe-file *distorted-txt*) (setf *re-analyse-soundfiles* t))
(when *re-analyse-soundfiles*
  (folder-to-stored-file-list
   *distorted*
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
  (store-in-text-file *distorted* *distorted-txt*))

(unless *re-analyse-soundfiles*
  (setf *distorted* (load-from-file *distorted-txt*)))

;; ** noise
(defparameter *noise* (make-stored-file-list 'noise nil))
(defparameter *noise-txt* (format nil "~a~a" +ens-src-dir+ "noise.txt"))
(unless (probe-file *noise-txt*) (setf *re-analyse-soundfiles* t))
(when *re-analyse-soundfiles*
  (folder-to-stored-file-list
   *noise*
   "/E/Keks_Feedback/samples/noise/"
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
  (store-in-text-file *noise* *noise-txt*))

(unless *re-analyse-soundfiles*
  (setf *noise* (load-from-file *noise-txt*)))

;; ** percussive
(defparameter *percussive* (make-stored-file-list 'percussive nil))
(defparameter *percussive-txt* (format nil "~a~a" +ens-src-dir+ "percussive.txt"))
(unless (probe-file *percussive-txt*) (setf *re-analyse-soundfiles* t))
(when *re-analyse-soundfiles*
  (folder-to-stored-file-list
   *percussive*
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
  (store-in-text-file *percussive* *percussive-txt*))

(unless *re-analyse-soundfiles*
  (setf *percussive* (load-from-file *percussive-txt*)))

;; EOF soundfiles.lsp
