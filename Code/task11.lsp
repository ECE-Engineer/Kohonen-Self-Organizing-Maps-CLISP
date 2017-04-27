;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(load "task11.lsp")
;;;(in-package #:vecto)
;;;(setf temp-list cluster-list)
;;;(setf temp1 (nth (random (length temp-list)) temp-list))
;;;(setf temp-list (remove temp1 temp-list))
;;;(setf temp2 (nth (random (length temp-list)) temp-list))
;;;(paint-checkerboard temp1 temp2)
;;;
;;;----------------------------------EXTRA THINGS TO TEST ARE THE SUB-FUNCTIONS--------------------------------------
;;;(let (temp1 temp2) (setf temp1 (list (list 50 250 50) (list 150 250 150) (list 0 100 0))) (setf temp2 (list (list 250 150 250) (list 96 8 80) (list 219 87 195))) (paint-checkerboard temp1 temp2))
;;;

(load "task10.lsp")
(ql:quickload "vecto")
(in-package #:vecto)

;;create a method that takes 2 RGB colored clusters to create a checkerboard with those colors
(defun paint-checkerboard (cluster1 cluster2 &aux checkerboard-canvas)
	(setf checkerboard-canvas (make-array (list 50 50) :initial-element '(0 0 0)))
	(dotimes (i 50)
		(dotimes (j 50)
			(if (= (mod (+ i j) 2) 0)
				(setf (aref checkerboard-canvas i j) (nth (random (length cluster1)) cluster1))
				(setf (aref checkerboard-canvas i j) (nth (random (length cluster2)) cluster2))
			)
		)
	)
	;paint the canvas
	(paint-board-canvas checkerboard-canvas (concatenate 'string "../Visuals/Extra/" (write-to-string 'checkerboard-canvas) ".png"))
)