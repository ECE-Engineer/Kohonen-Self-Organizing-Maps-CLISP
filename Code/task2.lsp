;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(load "task2.lsp")
;;;(setf map (init-default-array 16))
;;;(print-array map)
;;;(setf map (init-random-array map))
;;;(print-array map)
;;;(setf map (init-default-array 16))
;;;(setf map (init-corners-array map))
;;;(print-array map)
;;;(setf map (init-default-array 16))
;;;(setf map (init-equidistant-circles-array map))
;;;(print-array map)
;;;----------------------------------EXTRA THINGS TO TEST ARE THE SUB-FUNCTIONS--------------------------------------
;;;(setf vec1 (rand-weight-vector))
;;;(setf vec2 (rand-weight-vector))
;;;(subtract vec2 vec1)
;;;(setf vec1 (set-vector (nth 0 (rand-weight-vector)) (nth 1 (rand-weight-vector)) (nth 2 (rand-weight-vector))))
;;;(get-dist vec2 vec1)



(load "task1.lsp")

;;create a method to make a default n by n array
(defun init-default-array (num)
	(make-array (list num num) :initial-element '(0 0 0))
)

;;create a method to make an array that has red, green, blue, and black radiate from the corners
(defun init-corners-array (arrayN &aux height-multiplier width-multiplier)
	*read-default-float-format*
	
	(dotimes (i (car (array-dimensions arrayN)))
		(setf height-multiplier (* (/ i (car (array-dimensions arrayN))) 5.0))
		(dotimes (j (cadr (array-dimensions arrayN)))
			(setf width-multiplier (/ j (cadr (array-dimensions arrayN))))
			(setf (aref arrayN i j) (list (* (- 1.0 width-multiplier) height-multiplier) (* width-multiplier height-multiplier) (* (abs width-multiplier) (- 5.0 height-multiplier))))
		)
	)
	arrayN
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
	(list red-value green-value blue-value)
)

;;create a method to calculate the Euclidean distance
(defun get-dist (reference-vector actual-vector &aux temp-vector)
	*read-default-float-format*
	
	(setf temp-vector (list 0.0 0.0 0.0))
	(setf temp-vector (subtract reference-vector actual-vector))
	(setf temp-vector (set-vector (* (nth 0 temp-vector) (nth 0 temp-vector)) (* (nth 1 temp-vector) (nth 1 temp-vector)) (* (nth 2 temp-vector) (nth 2 temp-vector))))
	(sqrt (+ (nth 0 temp-vector) (nth 1 temp-vector) (nth 2 temp-vector)))
)

;;create a method to make an array that has red, green, and blue form a circle around the center
(defun init-equidistant-circles-array (arrayN &aux center outer max-dist theta1 theta2 theta3 height2 height4 width2 red-center green-center blue-center)
	*read-default-float-format*
	
	(setf center (list (car (array-dimensions arrayN)) (cadr (array-dimensions arrayN)) 0.0))
	(setf outer (list 0.0 0.0 0.0))
	
	(setf max-dist (/ (get-dist center outer) 5.0))
	(setf theta1 (* 90.0 (/ PI 180.0)))
	(setf theta2 (* 210.0 (/ PI 180.0)))
	(setf theta3 (* 330.0 (/ PI 180.0)))
	(setf height2 (/ (car (array-dimensions arrayN)) 2))
	(setf height4 (/ (car (array-dimensions arrayN)) 4))
	(setf width2 (/ (cadr (array-dimensions arrayN)) 2))
	
	(setf red-center (list (* (cos theta1) height4) (* (sin theta1) height4) 0.0))
	(setf green-center (list (* (cos theta2) height4) (* (sin theta2) height4) 0.0))
	(setf blue-center (list (* (cos theta3) height4) (* (sin theta3) height4) 0.0))
	
	(setf red-center (set-vector (+ (nth 0 red-center) width2) (+ (nth 1 red-center) height2) 0.0))
	(setf green-center (set-vector (+ (nth 0 green-center) width2) (+ (nth 1 green-center) height2) 0.0))
	(setf blue-center (set-vector (+ (nth 0 blue-center) width2) (+ (nth 1 blue-center) height2) 0.0))
	
	(dotimes (i (car (array-dimensions arrayN)))
		(dotimes (j (cadr (array-dimensions arrayN)))
			(setf outer (set-vector j i 0.0))
			
			(setf (aref arrayN i j) (list (/ (get-dist outer red-center) max-dist) (/ (get-dist outer green-center) max-dist) (/ (get-dist outer blue-center) max-dist)))
		)
	)
	arrayN
)