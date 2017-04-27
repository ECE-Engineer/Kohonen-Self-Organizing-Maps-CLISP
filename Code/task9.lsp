;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(load "task9.lsp")
;;;(in-package #:vecto)
;;;(start-bw 'RANDOM 'ED)
;;;(start-bw 'CORNER 'ED)
;;;(start-bw 'CENTER 'ED)
;;;(start-bw 'RANDOM 'CS)
;;;(start-bw 'CORNER 'CS)
;;;(start-bw 'CENTER 'CS)
;;;(start-bw 'RANDOM 'PCC)
;;;(start-bw 'CORNER 'PCC)
;;;(start-bw 'CENTER 'PCC)
;;;----------------------------------EXTRA THINGS TO TEST ARE THE SUB-FUNCTIONS--------------------------------------
;;;(init-screen-bw 'CORNER)
;;;(set-bw-palette)
;;;(update-bw-palette)
;;;(to-float color-buffer)
;;;(paint-canvas color-buffer "../Visuals/color.png")

(load "task8.lsp")
(ql:quickload "vecto")
(in-package #:vecto)
(defconstant *NUMBER_OF_NEIGHBORS* 3)
*read-default-float-format*

;;create a method to start the program
(defun start-bw (init-choice simularity-metric-choice)
	(cond
		((= state 0)
			(setf state 1)
			(setf map (init-screen-bw init-choice))
			(set-bw-palette)
			(generic-init init-choice)
			(generic-run-bw simularity-metric-choice init-choice)
		)
		(t
			(new-screen-layout init-choice)
			(generic-run-bw simularity-metric-choice init-choice)
		)
	)
)

;;create a method to execute a generic run of the program and return the screen when it is done
(defun generic-run-bw (simularity-metric init-choice &aux sample-vect winner-vect)
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
	
	(update-bw-palette)
	(to-float color-buffer)
	;paint the final screen
	(paint-canvas map (concatenate 'string "../Visuals/SOM_Frames/" (write-to-string 'finished) "and" (write-to-string simularity-metric) "and" (write-to-string init-choice) ".png"))
)

;;create a method to set the black & white palette of the map
(defun set-bw-palette ()
	*read-default-float-format*
	(dotimes (i 256)
		(setf (aref current-palette i) (aref black-white-palette i))
	)
)

;;create a method to update the black & white palette of the map
(defun update-bw-palette (&aux temp-array max-dist center-vect ave total temp-vec)
	*read-default-float-format*
	
	(setf temp-vec '(0 0 0))
	
	(dotimes (i (car (array-dimensions vector-weights)))
		(dotimes (j (cadr (array-dimensions vector-weights)))
			(setf temp-vec (round-vector (aref vector-weights j i)))
			(setf (aref color-buffer j i) (aref current-palette (aref rbg-table (nth 0 temp-vec) (nth 1 temp-vec) (nth 2 temp-vec))))
		)
	)
	
	
	
	(setf temp-array (make-array (list 50 50) :initial-element '(0 0 0)))
	(setf max-dist 0.0)
	
	(dotimes (i (car (array-dimensions vector-weights)))
		(dotimes (j (cadr (array-dimensions vector-weights)))
			(setf center-vect (list (coerce (nth 0 (aref vector-weights j i)) 'float) (coerce (nth 1 (aref vector-weights j i)) 'float) (coerce (nth 2 (aref vector-weights j i)) 'float)))
			(setf ave 0.0)
			(setf total 0.0)
			
			(loop for k from (- *NUMBER_OF_NEIGHBORS*) to *NUMBER_OF_NEIGHBORS* do
				(loop for z from (- *NUMBER_OF_NEIGHBORS*) to *NUMBER_OF_NEIGHBORS* do
					(if (and (>= (+ i k) 0) (< (+ i k) 50) (>= (+ j z) 0) (< (+ j z) 50))
						(progn
							(setf total (+ total (get-dist (aref vector-weights (+ j z) (+ i k)) center-vect)))
							(setf ave (coerce (+ ave 1.0) 'float))
						)
					)
				)
			)
			
			(setf total (coerce (/ total (- ave 1.0)) 'float))
			(if (> total max-dist) (setf max-dist total))
			
			(setf (aref temp-array j i) total)
		)
	)
	
	(setf total 0.0)
	(dotimes (i (car (array-dimensions vector-weights)))
		(dotimes (j (cadr (array-dimensions vector-weights)))
			(setf (aref vector-weights j i) (aref current-palette (round (- 255 (* (round (/ (aref temp-array j i) max-dist)) 255.0)))))
			
			(setf total (+ total (aref temp-array j i)))
		)
	)
	total
)