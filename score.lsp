;; * score

(in-package :ly)

;; ** divide (generate divisions and states)

;; update start-times of all layers just to be sure:
(loop for i in (access-minutes) do (update-layer-start-times i))

;; ** ...and conquer (handle the minute objects)

;; interpret layers returns a function that can then be filled with arguments
;;  to form a sections like function.


;; EOF score.lsp

