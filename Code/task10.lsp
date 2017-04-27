;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(load "task10.lsp")
;;;(in-package #:vecto)
;;;(start-gif 'RANDOM 'ED)
;;;(start-gif 'CORNER 'ED)
;;;(start-gif 'CENTER 'ED)
;;;(start-gif 'RANDOM 'CS)
;;;(start-gif 'CORNER 'CS)
;;;(start-gif 'CENTER 'CS)
;;;(start-gif 'RANDOM 'PCC)
;;;(start-gif 'CORNER 'PCC)
;;;(start-gif 'CENTER 'PCC)
;;;
;;;----------------------------------EXTRA THINGS TO TEST ARE THE SUB-FUNCTIONS--------------------------------------
;;;

(load "task9.lsp")
(ql:quickload "vecto")
(in-package #:vecto)
*read-default-float-format*

;;create a method to start the program
(defun start-gif (init-choice simularity-metric-choice)
	*read-default-float-format*
	(cond
		((= state 0)
			(setf state 1)
			(setf map (init-screen-bw init-choice))
			(generic-init init-choice)
			(generic-run-gif simularity-metric-choice init-choice)
		)
		(t
			(setf som-time 0.0)
			(setf time-inc (/ 1.0 *MAX_ITERATIONS*))
			(dotimes (i 256)
				(setf (aref black-white-palette i) (list i i i))
			)
			(init-screen init-choice)
			(generic-run-gif simularity-metric-choice init-choice)
		)
	)
)

;;create a method to execute a generic run of the program and return the screen when it is done
(defun generic-run-gif (simularity-metric init-choice &aux sample-vect winner-vect)
	;paint the initial screen
	(paint-canvas map (concatenate 'string "../Visuals/SOM_Frames/" (write-to-string 'initialized) "and" (write-to-string init-choice) ".png"))
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
				
				;paint each frame of the screen
				(paint-canvas map (concatenate 'string "../Visuals/SOM_Frames/" (write-to-string (round (* som-time 1000000000))) ".png"))
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