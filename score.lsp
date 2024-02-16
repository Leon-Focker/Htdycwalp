;; * score

(in-package :ly)

(let ((happenings '()))
  ;; collect the start of each 1 minute section:
  (loop for i from 0 to 11 do (push (* 60 i) happenings))
  
  (sort happenings #'<))

;; EOF score.lsp
