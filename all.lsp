;; * ensemble

;;; constant pulse of clicks, only their volume and timbre make the rhythm
;;; - using the layers indispensebility function and morphing the velocity lists
;;; then modulate speed of pulse and time signature synchronous with indispensibility
;;; modulate speed asynchronous to indispensibility

;; ** package

(ql:quickload 'layers)

(in-package :ly)

(defconstant +ens-src-dir+ (path-from-same-dir))

(import '(clm::with-sound
	  clm::with-mix
	  clm::sound-let
	  clm::*CLM-MIX-CALLS*
	  clm::*CLM-MIX-OPTIONS*
	  clm::add-sound))

;; ** globals

;; set this to true, if you want to re-analyse all soundfiles, even if their
;; analysis data was saved to a text-file.
(defparameter *re-analyse-soundfiles* nil)
;; for the generation of spatial audio files with reaper:
(set-sc-config 'reaper-files-for-windows t)

;; ** load
;; load files:
(dolist (file '("helpers.lsp"
		"soundfiles.lsp"
		"show-minutes.lsp"
		"minutes.lsp"
		;; score.lsp
		;;"tape-score.lsp"
		))
  (load (probe-file (format nil "~a~a" +ens-src-dir+ file))))

;; YAY :)
(format t "~&done loading!")

;; EOF all.lsp
