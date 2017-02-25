;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(setf map (create-empty-map 16))
;;;(visualize map)
;;;(setf map (init-map map))
;;;(visualize map)
;;;----------------------------------EXTRA THINGS TO TEST ARE THE SUB-FUNCTIONS--------------------------------------
;;;(rand-weight-vector)
;;;
;;;
;;;

;;create a method to create a RGB weight vector, whereas the R, B, and G values are selected at random
(defun rand-weight-vector()
	(list (random 256) (random 256) (random 256))
)

;;create a method to create a list of n squared elements
(defun create-empty-map(num)
	(make-list-empty (* num num))
)

;;create a method to construct the n squared length list
(defun make-list-empty(num)
	(cond
		((= num 0) '())
		(t (cons '() (make-list-empty (- num 1))))
	)
)

;;create a method to create randomly assign RGB weight vectors to the map
(defun init-map(lst)
	(make-list-full (length lst))
)

;;create a method to construct the n squared length list
(defun make-list-full(num)
	(cond
		((= num 0) '())
		(t (cons (rand-weight-vector) (make-list-full (- num 1))))
	)
)

;;create a method to visualize the map as a n by n grid of elements
(defun visualize(lst)
	(cond
		((null (car lst)) (print-empty-grid (length lst) (sqrt (length lst))))
		(t (print-full-grid lst 0 (sqrt (length lst))))
	)
)

;;print an empty n by n grid
(defun print-empty-grid(num const)
	(cond
		((< num 0) nil)
		((= (mod num const) 0)
			(if (< num (* const const))
				(progn
					(format t "~A" "- ")
					(terpri)
					(print-empty-grid (- num 1) const)
				)
				(progn
					(print-empty-grid (- num 1) const)
				)
			)
		)
		(t 
			(format t "~A" "- ")
			(print-empty-grid (- num 1) const)
		)
	)
)

;;print an full n by n grid
(defun print-full-grid(lst num const)
	(cond
		((> (+ num 1) (length lst)) nil)
		((= (mod (+ num 1) const) 0)
			(if (> (+ num 1) 0)
				(progn
					(format t "~A" (concatenate 'string (write-to-string (nth num lst)) " "))
					(terpri)
					(print-full-grid lst (+ num 1) const)
				)
				(progn
					(format t "~A" (concatenate 'string (write-to-string (nth num lst)) " "))
					(print-full-grid lst (+ num 1) const)
				)
			)
		)
		(t 
			(format t "~A" (concatenate 'string (write-to-string (nth num lst)) " "))
			(print-full-grid lst (+ num 1) const)
		)
	)
)