;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(load "task9.lsp")
;;;(in-package #:vecto)
;;;(setf map (init-screen 'RANDOM))
;;;(paint-canvas map "../Visuals/random.png")
;;;(setf map (new-screen-layout 'CORNER))
;;;(paint-canvas map "../Visuals/corner.png")
;;;(setf map (new-screen-layout 'CENTER))
;;;(paint-canvas map "../Visuals/center.png")
;;;(setf vect (rand-weight-vector))
;;;(setf map (fill-array map vect))
;;;(paint-canvas map "../Visuals/color.png")
;;;----------------------------------EXTRA THINGS TO TEST ARE THE SUB-FUNCTIONS--------------------------------------

;(load "task3.lsp")
(ql:quickload "vecto")
(in-package #:vecto)

(defconstant *INTEGER-BIT-SIZE* 30)

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