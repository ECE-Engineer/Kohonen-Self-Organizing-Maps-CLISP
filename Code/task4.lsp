;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(load "task4.lsp")
;;;(in-package #:vecto)
;;;(setf map (init-screen 'RANDOM))
;;;(setf sample (rand-weight-vector))
;;;(setf winner-vect (find-winner map sample 'ED))
;;;(get-down-scaled-vect map winner-vect)
;;;(get-neighbors map winner-vect 0.1)
;;;(setf winner-vect (find-winner map sample 'CS))
;;;(get-down-scaled-vect map winner-vect)
;;;(get-neighbors map winner-vect 0.1)
;;;(setf winner-vect (find-winner map sample 'PCC))
;;;(get-down-scaled-vect map winner-vect)
;;;(get-neighbors map winner-vect 0.1)
;;;(setf map (new-screen-layout 'CORNER))
;;;(setf sample (rand-weight-vector))
;;;(setf winner-vect (find-winner map sample 'ED))
;;;(get-neighbors map winner-vect 0.1)
;;;(setf winner-vect (find-winner map sample 'CS))
;;;(get-neighbors map winner-vect 0.1)
;;;(setf winner-vect (find-winner map sample 'PCC))
;;;(get-neighbors map winner-vect 0.1)
;;;(setf map (new-screen-layout 'CENTER))
;;;(setf sample (rand-weight-vector))
;;;(setf winner-vect (find-winner map sample 'ED))
;;;(get-neighbors map winner-vect 0.1)
;;;(setf winner-vect (find-winner map sample 'CS))
;;;(get-neighbors map winner-vect 0.1)
;;;(setf winner-vect (find-winner map sample 'PCC))
;;;(get-neighbors map winner-vect 0.1)
;;;----------------------------------EXTRA THINGS TO TEST ARE THE SUB-FUNCTIONS--------------------------------------
;;;(print-all-neighbors map winner-vect)

(ql:quickload "vecto")
(in-package #:vecto)

(load "task3.lsp")

*read-default-float-format*
(defconstant *RADIUS* 60)

;;create a method to get scaled down corresponding winning weight vector
(defun get-down-scaled-vect (vector-array vect &aux pos vec)
	*read-default-float-format*
	(block find-block
		(dotimes (i (car (array-dimensions vector-array)))
			(dotimes (j (cadr (array-dimensions vector-array)))
				(if (eq (aref vector-array i j) vect)
					(setf pos (list i j))
				)
			)
		)
	)
	(setf vec (aref vector-weights (car pos) (cadr pos)))
	(list (coerce (nth 0 vec) 'float) (coerce (nth 1 vec) 'float) (coerce (nth 2 vec) 'float))
)

;;create a method to determine the neighbors of the winning weight vector_image
(defun get-neighbors (vector-array winning-weight-vector time &aux radius2 outer-vect center-vect normalized-dist neighbor-vect temp)
	*read-default-float-format*
	(setf radius2 (round (/ (* *RADIUS* (- 1 time)) 2)))
	(setf neighbor-vect '())
	
	(loop for i from (- radius2) to radius2 do
		(loop for j from (- radius2) to radius2 do
			(setf temp (get-down-scaled-vect vector-array winning-weight-vector))
			(if (and (>= (+ i (nth 1 temp)) 0) (< (+ i (nth 1 temp)) (cadr (array-dimensions vector-array))) (>= (+ j (nth 0 temp)) 0) (< (+ j (nth 0 temp)) (car (array-dimensions vector-array))))
				(setf neighbor-vect (snoc (aref vector-array (round (+ (nth 0 temp) j)) (round (+ (nth 1 temp) i))) neighbor-vect))
			)
		)
	)
	neighbor-vect
)

;;create a method to print the neighbors with a radius from 0 to 100 with a step size of 1/100
(defun print-all-neighbors (vector-array winning-weight-vector &aux max-count counter inc temp)
	*read-default-float-format*
	(setf max-count 100)
	(setf counter 0)
	(setf inc (/ 1 max-count))
	
	(loop while (< counter max-count) do
		(setf temp (get-neighbors vector-array winning-weight-vector counter))
		(if (not (null temp))
			(write temp)
			(terpri)
		)
		(setf counter (+ counter inc))
	)
)