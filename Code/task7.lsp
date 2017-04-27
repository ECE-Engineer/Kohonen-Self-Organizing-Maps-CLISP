;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(load "task7.lsp")
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

(load "task6.lsp")
(ql:quickload "vecto")
(in-package #:vecto)
(defconstant *INTEGER-BIT-SIZE* 30)
*read-default-float-format*

;;create a function that shifts x a specific amount of bits left
(defun shift-left (x bits)
	(logand (ash x bits) (1- (ash 1 *INTEGER-BIT-SIZE*)))
)

;;create a function that shifts x a specific amount of bits right
(defun shift-right (x bits)
	(logand (ash x (- bits)) (1- (ash 1 *INTEGER-BIT-SIZE*)))
)

;;create a function to paint the map in color with each weight vector represented as 4 by 4 pixels
(defun paint-canvas (arrayN file)
	(defconstant *X-SIZE* 15)
	*read-default-float-format*
	(with-canvas (:width 200 :height 200)
		(dotimes (i (car (array-dimensions vector-weights)))
			(dotimes (j (cadr (array-dimensions vector-weights)))
				(set-rgb-fill (/ (nth 0 (aref arrayN i j)) 256) (/ (nth 1 (aref arrayN i j)) 256) (/ (nth 2 (aref arrayN i j)) 256))
				(rectangle (+ (shift-left j 2) *X-SIZE*) (shift-left i 2) 4 4);
				(fill-path)
			)
		)
		(save-png file)
	)
)

;;create a function to paint the map in color with each weight vector represented as 4 by 4 pixels
(defun paint-board-canvas (arrayN file)
	(defconstant *X-SIZE* 15)
	*read-default-float-format*
	(with-canvas (:width 200 :height 200)
		(dotimes (i 50)
			(dotimes (j 50)
				(set-rgb-fill (/ (nth 0 (aref arrayN i j)) 256) (/ (nth 1 (aref arrayN i j)) 256) (/ (nth 2 (aref arrayN i j)) 256))
				(rectangle (+ (shift-left j 2) *X-SIZE*) (shift-left i 2) 4 4)
				(fill-path)
			)
		)
		(save-png file)
	)
)

;;create a method to start the program
(defun start (init-choice simularity-metric-choice)
	*read-default-float-format*
	(cond
		((= state 0)
			(setf state 1)
			(setf map (init-screen-bw init-choice))
			(generic-init init-choice)
			(generic-run simularity-metric-choice init-choice)
		)
		(t
			(setf som-time 0.0)
			(setf time-inc (/ 1.0 *MAX_ITERATIONS*))
			(dotimes (i 256)
				(setf (aref black-white-palette i) (list i i i))
			)
			(init-screen-bw init-choice)
			(generic-run simularity-metric-choice init-choice)
		)
	)
)

;;create a method that resets the program
(defun reset (init-choice)
	;uncomment the below command to get a new set of vector sample vectors
	;(init-vector-samples)
	(setf som-time 0.0)
	(setf time-inc (/ 1.0 *MAX_ITERATIONS*))
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
				(if (< som-time 0.899937)
					(progn
						(setf sample-vect (get-random-vector))
						(setf winner-vect (find-winner vector-weights sample-vect simularity-metric))
						(scale-neighbors winner-vect sample-vect som-time)
						(setf som-time (+ som-time time-inc))
					)
				)
				(if (>= som-time 0.899937)
					(progn
						(setf som-time 0.899937)
					)
				)
				;update the screen
				(update-rgb)
				(to-float color-buffer)
				
				(if (= som-time 0.899937)
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

;;create a method that sets the screen colors
(defun init-screen-bw (init-choice)
	*read-default-float-format*
	
	(dotimes (i 256)
		(setf (aref black-white-palette i) (list i i i))
	)
	
	(init-screen init-choice)
)