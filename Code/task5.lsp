;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(load "task5.lsp")
;;;(in-package #:vecto)
;;;(learning-function 0.5 0.5)
;;;(learning-function 1.5 0.5)
;;;(learning-function 2.5 0.5)
;;;----------------------------------EXTRA THINGS TO TEST ARE THE SUB-FUNCTIONS--------------------------------------
;;;

(ql:quickload "vecto")
(in-package #:vecto)

(load "task4.lsp")

;;create a function that calculates the scaling factor that the learning of a given weight vector decreases by
(defun learning-function(dist current-time &aux scaling-factor)
	(setf scaling-factor (coerce (exp (/ (* (- 1.0) (expt dist 2.0)) 0.15)) 'float))
	(setf scaling-factor (/ scaling-factor (+ (* current-time 4.0) 1.0)))
	scaling-factor
)