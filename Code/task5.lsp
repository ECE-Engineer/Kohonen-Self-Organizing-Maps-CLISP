;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(load "task5.lsp")
;;;(in-package #:vecto)
;;;(start 'RANDOM 'ED)
;;;(start 'CORNER 'ED)
;;;(start 'CENTER 'ED)
;;;(start 'RANDOM 'CS)
;;;(start 'CORNER 'CS)
;;;(start 'CENTER 'CS)
;;;(start 'RANDOM 'PCC)
;;;(start 'CORNER 'PCC)
;;;(start 'CENTER 'PCC)
;;;----------------------------------EXTRA THINGS TO TEST ARE THE SUB-FUNCTIONS--------------------------------------
;;;

(ql:quickload "vecto")
(in-package #:vecto)

(load "task4.lsp")
(load "task9.lsp")

*read-default-float-format*
(setf state 0)
(setf map (list 0.0 0.0 0.0))
(defconstant *MAX_ITERATIONS* 500)
(setf som-time 0.0)
(setf time-inc (/ 1.0 *MAX_ITERATIONS*))

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
					
					;get the scaling factor
					(setf time-update (coerce (exp (/ (* (- 1.0) (expt distance 2.0)) 0.15)) 'float))
					;get the scalar factor learning decreases by
					(setf time-update (/ time-update (+ (* time 4.0) 1.0)))
					
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

;;create a method to start the program
(defun start (init-choice simularity-metric-choice)
	(cond
		((= state 0)
			(setf state 1)
			(setf map (init-screen init-choice))
			(generic-init init-choice)
			(generic-run simularity-metric-choice init-choice)
		)
		(t
			(new-screen-layout init-choice)
			(generic-run simularity-metric-choice init-choice)
		)
	)
)

;;create a method that resets the program
(defun reset (init-choice)
	;(init-vector-samples)
	(setf som-time 0.0)
	(setf time-inc (/ 1.0 *MAX_ITERATIONS*))
	;paint the initial screen
	(paint-canvas map (concatenate 'string "../Visuals/SOM_Frames/" (write-to-string 'initialized) "and" (write-to-string init-choice) ".png"))
)

;;create a method that performs simple initializations
(defun generic-init (init-choice)
	(reset init-choice)
)

;;create a method to execute a generic run of the program and return the screen when it is done
(defun generic-run (simularity-metric init-choice &aux sample-vect winner-vect)
	*read-default-float-format*
	(block infinite-loop
		(loop
			do
				(print som-time)
				(terpri)
				(if (< som-time 0.98)
					(progn
						(setf sample-vect (get-random-vector))
						(setf winner-vect (find-winner vector-weights sample-vect simularity-metric))
						(scale-neighbors winner-vect sample-vect som-time)
						(setf som-time (+ som-time time-inc))
					)
				)
				(if (>= som-time 0.98)
					(progn
						(setf som-time 0.98)
					)
				)
				;update the screen
				(set-rgb)
				(update-rgb)
				(to-float color-buffer)
				;paint each frame of the screen
				(paint-canvas map (concatenate 'string "../Visuals/SOM_Frames/" (write-to-string (round (* som-time 1000))) ".png"))
				(if (= som-time 0.98)
					(progn
						(print "exiting!!!")
						(terpri)
						(return-from infinite-loop)
					)
				)
		)
	)
	;paint the final screen
	(paint-canvas map (concatenate 'string "../Visuals/SOM_Frames/" (write-to-string 'finished) "and" (write-to-string simularity-metric) "and" (write-to-string init-choice) ".png"))
)