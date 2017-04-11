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

(load "task9.lsp")

*read-default-float-format*
(setf state 0)
(setf map (list 0.0 0.0 0.0))
(defconstant *MAX_ITERATIONS* 500)
(setf som-time 0.0)
(setf time-inc (/ 1.0 *MAX_ITERATIONS*))

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