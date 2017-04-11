;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(load "task2.lsp")
;;;(in-package #:vecto)
;;;(setf map (init-default-array 50))
;;;(print-array map)
;;;(init-screen 'RANDOM)
;;;(init-screen 'CORNER)
;;;(init-screen 'CENTER)
;;;----------------------------------EXTRA THINGS TO TEST ARE THE SUB-FUNCTIONS--------------------------------------
;;;(setf vec1 (rand-weight-vector))
;;;(setf vec2 (rand-weight-vector))
;;;(subtract vec2 vec1)
;;;(setf vec1 (set-vector (nth 0 (rand-weight-vector)) (nth 1 (rand-weight-vector)) (nth 2 (rand-weight-vector))))
;;;(get-dist vec2 vec1)
;;;(setf vec2 (get-random-vector))
;;;(get-dist vec2 vec1)

(ql:quickload "vecto")
(in-package #:vecto)

(load "task1.lsp")

(setf vector-weights (make-array (list 50 50) :initial-element '(0 0 0)))
(setf color-buffer (make-array (list 50 50) :initial-element '(0 0 0)))
(setf vector-samples (make-array (list 50) :initial-element '(0 0 0)))
(defconstant *MAX-SAMPLE-POINTS* 50)

(setf rgb-palette (make-array (list 256) :initial-element '(0 0 0)))
(setf current-palette (make-array (list 256) :initial-element '(0 0 0)))
(setf rbg-table (make-array (list 6 6 6) :initial-element '(0 0 0)))



;;create a method to select a random sample from the map
(defun get-random-vector ()
	(aref vector-samples (multiple-value-bind (q r) (floor (random *MAX-SAMPLE-POINTS*)) q))
)

;;create a method to set the current color palette
(defun set-rgb ()
	*read-default-float-format*
	(dotimes (i 256);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;problem might be here------its a 1D array(car (array-dimensions current-palette))
		(setf (aref current-palette i) (aref rgb-palette i))
	)
)

;;create a method to initialize all the vector samples
(defun init-vector-samples ()
	*read-default-float-format*
	(dotimes (i *MAX-SAMPLE-POINTS*)
		(setf (aref vector-samples i) (list (multiple-value-bind (q r) (floor (random 6)) q) (multiple-value-bind (q r) (floor (random 6)) q) (multiple-value-bind (q r) (floor (random 6)) q)))
	)
)

;;create a method to return a weight vector with its contents rounded
(defun round-vector (vector1)
	(list (round (nth 0 vector1)) (round (nth 1 vector1)) (round (nth 2 vector1)))
)

;;create a method to update the screen
(defun update-rgb (&aux temp-vec)
	*read-default-float-format*
	(setf temp-vec '(0 0 0))
	
	(dotimes (i (car (array-dimensions vector-weights)))
		(dotimes (j (cadr (array-dimensions vector-weights)))
			(setf temp-vec (round-vector (aref vector-weights j i)));;;;;;;;;;;;;;;;;;either something is wrong here or THE VECTOR WEIGHTS ARE NOT WHAT THEY SHOULD BE!!!
			(setf (aref color-buffer j i) (aref current-palette (aref rbg-table (nth 0 temp-vec) (nth 1 temp-vec) (nth 2 temp-vec))));;;;;;;;;;;;;;;;;;;;;;;;here!;;;;;;;;;;;;;;;;;;;;;;;yes it is but where???!!!
		)
	)
)

;;create a method to make a default n by n array
(defun init-default-array (num)
	(make-array (list num num) :initial-element '(0 0 0))
)

;;create a method to make an array that has red, green, blue, and black radiate from the corners
(defun init-corners-array (&aux height-multiplier width-multiplier)
	*read-default-float-format*
	
	(dotimes (i (car (array-dimensions vector-weights)))
		(setf height-multiplier (coerce (* (/ i (car (array-dimensions vector-weights))) 5.0) 'float))
		(dotimes (j (cadr (array-dimensions vector-weights)))
			(setf width-multiplier (coerce (/ j (cadr (array-dimensions vector-weights))) 'float))
			(setf (aref vector-weights i j) (list (* (- 1.0 width-multiplier) height-multiplier) (* width-multiplier height-multiplier) (* (abs width-multiplier) (- 5.0 height-multiplier))))
		)
	)
)

;;create a method to calculate the difference between 2 vectors
(defun subtract (reference-vector actual-vector &aux temp-vector)
	*read-default-float-format*
	(setf temp-vector (list 0.0 0.0 0.0))
	(setf temp-vector (list (- (nth 0 reference-vector) (nth 0 actual-vector)) (- (nth 1 reference-vector) (nth 1 actual-vector)) (- (nth 2 reference-vector) (nth 2 actual-vector))))
	temp-vector
)

;;create a method to set a vector
(defun set-vector (red-value green-value blue-value)
	*read-default-float-format*
	(list (coerce red-value 'float) (coerce green-value 'float) (coerce blue-value 'float))
)

;;create a method to calculate the Euclidean distance
(defun get-dist (reference-vector actual-vector &aux temp-vector)
	*read-default-float-format*
	
	(setf temp-vector (list 0.0 0.0 0.0))
	(setf temp-vector (subtract reference-vector actual-vector))
	(setf temp-vector (set-vector (* (nth 0 temp-vector) (nth 0 temp-vector)) (* (nth 1 temp-vector) (nth 1 temp-vector)) (* (nth 2 temp-vector) (nth 2 temp-vector))))
	(coerce (sqrt (+ (nth 0 temp-vector) (nth 1 temp-vector) (nth 2 temp-vector))) 'float)
)

;;create a method to make an array that has red, green, and blue form a circle around the center
(defun init-equidistant-circles-array (&aux center outer max-dist theta1 theta2 theta3 height2 height4 width2 red-center green-center blue-center)
	*read-default-float-format*
	
	(setf center (list (coerce (car (array-dimensions vector-weights)) 'float) (coerce (cadr (array-dimensions vector-weights)) 'float) 0.0))
	(setf outer (list 0.0 0.0 0.0))
	
	(setf max-dist (/ (coerce (get-dist center outer) 'float) 5.0))
	(setf theta1 (* 90.0 (coerce (/ PI 180.0) 'float)))
	(setf theta2 (* 210.0 (coerce (/ PI 180.0) 'float)))
	(setf theta3 (* 330.0 (coerce (/ PI 180.0) 'float)))
	(setf height2 (coerce (/ (car (array-dimensions vector-weights)) 2) 'float))
	(setf height4 (coerce (/ (car (array-dimensions vector-weights)) 4) 'float))
	(setf width2 (coerce (/ (cadr (array-dimensions vector-weights)) 2) 'float))
	
	(setf red-center (list (coerce (* (cos theta1) height4) 'float) (coerce (* (sin theta1) height4) 'float) 0.0))
	(setf green-center (list (coerce (* (cos theta2) height4) 'float) (coerce (* (sin theta2) height4) 'float) 0.0))
	(setf blue-center (list (coerce (* (cos theta3) height4) 'float) (coerce (* (sin theta3) height4) 'float) 0.0))
	
	(setf red-center (set-vector (+ (nth 0 red-center) width2) (+ (nth 1 red-center) height2) 0.0))
	(setf green-center (set-vector (+ (nth 0 green-center) width2) (+ (nth 1 green-center) height2) 0.0))
	(setf blue-center (set-vector (+ (nth 0 blue-center) width2) (+ (nth 1 blue-center) height2) 0.0))
	
	(dotimes (i (car (array-dimensions vector-weights)))
		(dotimes (j (cadr (array-dimensions vector-weights)))
			(setf outer (set-vector (coerce j 'float) (coerce i 'float) 0.0))
			
			(setf (aref vector-weights i j) (list (/ (get-dist outer red-center) max-dist) (/ (get-dist outer green-center) max-dist) (/ (get-dist outer blue-center) max-dist)))
		)
	)
)

;;create a method to "FILL" the n by n array RANDOMLY
(defun init-random-array ()
	*read-default-float-format*
	(dotimes (i (car (array-dimensions vector-weights)))
		(dotimes (j (cadr (array-dimensions vector-weights)))
			(setf (aref vector-weights j i) (list (coerce (/ (random 500) 100) 'float) (coerce (/ (random 500) 100) 'float) (coerce (/ (random 500) 100) 'float)))
		)
	)
)

;;create a method to make all the values in the array to floating-point-numbers
(defun to-float (arrayN &aux temp)
	*read-default-float-format*
	(dotimes (i (car (array-dimensions vector-weights)))
		(dotimes (j (cadr (array-dimensions vector-weights)))
			(setf temp (list (coerce (nth 0 (aref arrayN i j)) 'float) (coerce (nth 1 (aref arrayN i j)) 'float) (coerce (nth 2 (aref arrayN i j)) 'float)))
			(setf (aref arrayN i j) temp)
		)
	)
)

;;create a method that sets the screen colors
(defun init-screen (init-choice);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	*read-default-float-format*
	(setf col-loop 0)
	(dotimes (i (nth 0 (array-dimensions rbg-table)))
		(dotimes (j (nth 1 (array-dimensions rbg-table)))
			(dotimes (k (nth 2 (array-dimensions rbg-table)));;;;;;;;;;;;;;;;;;;;;;;;;;;;;issue might be with these!!!!!
				(setf (aref rbg-table i j k) col-loop)
				(setf (aref rgb-palette (+ (* i 36) (* j 6) k)) (list (* i (/ 256 5)) (* j (/ 256 5)) (* k (/ 256 5))))
				(setf col-loop (+ col-loop 1))
			)
		)
	)
	(dotimes (i (car (array-dimensions vector-weights)))
		(dotimes (j (cadr (array-dimensions vector-weights)))
			(setf (aref vector-weights j i) '(0 0 0))
			(setf (aref color-buffer j i) '(0 0 0))
		)
	)
	(dotimes (i *MAX-SAMPLE-POINTS*)
		(setf (aref vector-samples i) '(0 0 0))
	)
	(set-rgb)
	(init-vector-samples)
	(cond
		((eq init-choice 'RANDOM)
			(init-random-array)
			(set-rgb)
			(update-rgb)
			(to-float color-buffer)
			color-buffer
		)
		(
			(eq init-choice 'CORNER)
			(init-corners-array)
			(set-rgb)
			(update-rgb)
			(to-float color-buffer)
			color-buffer
		)
		(
			(eq init-choice 'CENTER)
			(init-equidistant-circles-array)
			(set-rgb)
			(update-rgb)
			(to-float color-buffer)
			color-buffer
		)
	)
)

;;create a method that picks a new screen layout
(defun new-screen-layout (init-choice)
	(cond
		((eq init-choice 'RANDOM)
			(set-rgb)
			(update-rgb)
			(to-float color-buffer)
			color-buffer
		)
		(
			(eq init-choice 'CORNER) (init-corners-array)
			(set-rgb)
			(update-rgb)
			(to-float color-buffer)
			color-buffer
		)
		(
			(eq init-choice 'CENTER) (init-equidistant-circles-array)
			(set-rgb)
			(update-rgb)
			(to-float color-buffer)
			color-buffer
		)
	)
)