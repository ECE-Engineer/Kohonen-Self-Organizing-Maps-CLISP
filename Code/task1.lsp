;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(load "task1.lsp")
;;;(setf map (init-empty-array 16))
;;;(print-array map)
;;;(setf map (init-array map))
;;;(print-array map)
;;;----------------------------------EXTRA THINGS TO TEST ARE THE SUB-FUNCTIONS--------------------------------------
;;;(rand-weight-vector)



;;create a method to make an empty n by n array
(defun init-empty-array (num)
	(make-array (list num num))
)

;;create a method to print out the n by n array
(defun print-array (arrayN)
	(dotimes (i (car (array-dimensions arrayN)))
		(dotimes (j (cadr (array-dimensions arrayN)))
			(cond
				((eq (aref arrayN i j) nil) (format t "~A" "- "))
				(t (format t "~A~A" (aref arrayN i j) " "))
			)
		)
		(terpri)
	)
)

;;create a method to create an RGB weight vector whereas the R, B, and G values are selected at random
(defun rand-weight-vector()
	(list (random 256) (random 256) (random 256))
)

;;create a method to "FILL" the n by n array RANDOMLY
(defun init-array (arrayN)
	(dotimes (i (car (array-dimensions arrayN)))
		(dotimes (j (cadr (array-dimensions arrayN)))
			(setf (aref arrayN i j) (rand-weight-vector))
		)
	)
	arrayN
)