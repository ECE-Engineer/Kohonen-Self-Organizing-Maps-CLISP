;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(load "task6.lsp")
;;;(in-package #:vecto)
;;;(setf map (init-screen 'RANDOM))
;;;(init-vector-samples)
;;;(setf sample (get-random-vector))
;;;(setf winner-vect (find-winner vector-weights sample 'ED))
;;;(scale-neighbors winner-vect sample 0.1)
;;;vector-weights
;;;----------------------------------EXTRA THINGS TO TEST ARE THE SUB-FUNCTIONS--------------------------------------
;;;

(ql:quickload "vecto")
(in-package #:vecto)

(load "task5.lsp")

*read-default-float-format*





;;create a method to find the location of a vector in the vector-weights array
(defun get-pos (vect &aux pos-vect)
	*read-default-float-format*
	(block find-block
		(dotimes (i 50)
			(dotimes (j 50)
				(if (eq (aref vector-weights i j) vect)
					(progn
						(setf pos-vect (list i j))
						(return-from find-block)
					)
				)
			)
		)
	)
	pos-vect
)

;;create a method to scale the neighbors of the winning weight
(defun scale-neighbors (winning-weight-vector sample-weight-vector time &aux radius2 outer-vect center-vect normalized-dist distance time-update temp1 temp2 temp3 position-vector)
	*read-default-float-format*
	(setf radius2 (coerce (round (/ (* *RADIUS* (- 1.0 time)) 2.0)) 'float))
	(setf outer-vect (list (coerce radius2 'float) (coerce radius2 'float) 0.0))
	(setf center-vect (list 0.0 0.0 0.0))
	(setf normalized-dist (get-dist center-vect outer-vect))
	
	(setf position-vector (get-pos winning-weight-vector))
	
	(loop for i from (- radius2) to radius2 do
		(loop for j from (- radius2) to radius2 do
			(if (and (>= (+ i (nth 1 position-vector)) 0) (< (+ i (nth 1 position-vector)) (cadr (array-dimensions vector-weights))) (>= (+ j (nth 0 position-vector)) 0) (< (+ j (nth 0 position-vector)) (car (array-dimensions vector-weights))))
				(progn
				
					;get distance from center point and normalize
					(setf outer-vect (list (coerce j 'float) (coerce i 'float) 0.0))
					(setf distance (get-dist outer-vect center-vect))
					(setf distance (/ distance normalized-dist))
					
					;get the scaling factor learning decreases by
					(setf time-update (learning-function distance time))
					
					;scale the vector by the appropriate amount
					(setf temp1 (multiply sample-weight-vector time-update))
					(setf temp2 (aref vector-weights (floor (coerce (+ (nth 0 position-vector) j) 'float)) (floor (coerce (+ (nth 1 position-vector) i) 'float))))
					(setf temp3 (multiply temp2 (- 1.0 time-update)))
					;adjust the vector in the array
					(setf (aref vector-weights (floor (coerce (+ (nth 0 position-vector) j) 'float)) (floor (coerce (+ (nth 1 position-vector) i) 'float))) (add-components temp1 temp3))
				)
			)
		)
	)
)

;;create a method to scale a vector by a certain factor
(defun add-components (vect1 vect2)
	*read-default-float-format*
	(list (coerce (+ (nth 0 vect1) (nth 0 vect2)) 'float) (coerce (+ (nth 1 vect1) (nth 1 vect2)) 'float) (coerce (+ (nth 2 vect1) (nth 2 vect2)) 'float))
)

;;create a method to scale a vector by a certain factor
(defun add (vect scalar)
	*read-default-float-format*
	(list (coerce (+ (nth 0 vect) scalar) 'float) (coerce (+ (nth 1 vect) scalar) 'float) (coerce (+ (nth 2 vect) scalar) 'float))
)

;;create a method to scale a vector by a certain factor
(defun multiply (vect scalar)
	*read-default-float-format*
	(list (coerce (* (nth 0 vect) scalar) 'float) (coerce (* (nth 1 vect) scalar) 'float) (coerce (* (nth 2 vect) scalar) 'float))
)