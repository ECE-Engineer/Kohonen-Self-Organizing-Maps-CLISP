;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(load "task12.lsp")
;;;(in-package #:vecto)
;;;(start 'RANDOM 'ED)
;;;(whatis '(255 0 0))
;;;red
;;;(write-to-file)
;;;----------------------------------EXTRA THINGS TO TEST ARE THE SUB-FUNCTIONS--------------------------------------
;;;(load "task12.lsp")
;;;(in-package #:vecto)
;;;(setf map (init-screen-bw 'RANDOM))
;;;(whatis '(255 0 0))
;;;red
;;;(write-to-file)

(load "task11.lsp")
(ql:quickload "vecto")
(in-package #:vecto)

;;create a function that writes the clusters to a text file (one cluster per line)
(defun write-to-file (&aux)
	(with-open-file (stream "clusters.txt" :direction :output :if-does-not-exist :create)
		(dotimes (i (length cluster-list))
			(if (not (null (nth i cluster-list)))
				(progn
					(format stream (write-to-string (nth i cluster-list)))
					(if (< i (- (length cluster-list) 1))
						(terpri stream)
					)
				)
			)
		)
	)
)