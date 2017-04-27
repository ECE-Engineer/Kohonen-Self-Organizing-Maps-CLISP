;;;;-------------------------------------------EXAMPLE COMMANDS BELOW-------------------------------------------------------
;;;(load "task8.lsp")
;;;(in-package #:vecto)
;;;(start 'RANDOM 'ED)
;;;(whatis '(255 0 0))
;;;red
;;;(whatis '(0 255 0))
;;;green
;;;(whatis '(0 0 255))
;;;blue
;;;(whatis '(255 0 255))
;;;pink
;;;(whatis '(255 255 0))
;;;yellow
;;;(whatis '(255 128 0))
;;;orange
;;;(whatis '(128 128 128))
;;;gray
;;;(whatis '(255 255 255))
;;;white
;;;(whatis '(0 0 0))
;;;black
;;;(whatis '(128 0 255))
;;;purple
;;;(whatis '(255 0 128))
;;;bright_pink
;;;(whatis '(0 255 255))
;;;light_blue
;;;(whatis '(0 64 255))
;;;bright_blue
;;;(whatis '(0 128 255))
;;;pale_blue
;;;(whatis '(153 0 0))
;;;red
;;;(whatis '(200 0 0))
;;;red
;;;(whatis '(100 0 0))
;;;red
;;;(whatis '(10 0 0))
;;;black
;;;(whatis '(10 5 15))
;;;black
;;;(whatis '(0 200 0))
;;;green
;;;(whatis '(0 150 0))
;;;green
;;;(whatis '(0 85 0))
;;;green
;;;(whatis '(93 85 127))
;;;gray
;;;(whatis '(93 85 127))
;;;gray
;;;(whatis '(211 202 255))
;;;black
;;;----------------------------------EXTRA THINGS TO TEST ARE THE SUB-FUNCTIONS--------------------------------------
;;;(load "task8.lsp")
;;;(in-package #:vecto)
;;;(setf map (init-screen-bw 'RANDOM))
;;;(whatis '(255 0 0))
;;;red

(load "task7.lsp")
(ql:quickload "vecto")
(in-package #:vecto)
*read-default-float-format*
(setf cluster-list '())
(defconstant *MAX_TRIALS* 100)
(setf status 'uncomplete)
(setf centroids '())
(setf label-list '())

;;create a method to get scaled up corresponding winning weight vector
(defun get-up-scaled-vect (vect &aux pos vec)
	*read-default-float-format*
	(block find-block
		(dotimes (i 50)
			(dotimes (j 50)
				(if (eq (aref vector-weights i j) vect)
					(setf pos (list i j))
				)
			)
		)
	)
	(setf vec (aref color-buffer (car pos) (cadr pos)))
	(list (coerce (nth 0 vec) 'float) (coerce (nth 1 vec) 'float) (coerce (nth 2 vec) 'float))
)

;;create a method that will make a list of the BMUs in the data set from the given sample set
(defun get-all-BMUs (&aux lst)
	(setf lst '())
	(dotimes (i (car (array-dimensions vector-samples)))
		(setf lst (snoc (get-up-scaled-vect (find-winner vector-weights (aref vector-samples i) 'ED)) lst))
	)
	lst
)

;;create a method to assign RGB vectors to clusters
(defun assign-to-clusters (lst &aux similar-pair previous-value current-value counter)
	(dotimes (i 50)
		(dotimes (j 50)
			(setf similar-pair '())
			(setf previous-value 0)
			(setf current-value 0)
			(setf counter 0)
			(dotimes (k (length lst))
				(cond
					((and (= k 0))
						(setf previous-value (get-dist (nth k lst) (aref color-buffer i j)))
						(setf similar-pair (snoc k similar-pair))
						(setf counter (+ counter 1))
					)
					(t
						(setf current-value (get-dist (nth k lst) (aref color-buffer i j)))
						(if (< current-value previous-value)
							(progn
								(setf similar-pair (remove (nth (- counter 1) similar-pair) similar-pair))
								(setf similar-pair (snoc k similar-pair))
								(setf previous-value current-value)
							)
						)
					)
				)
			)
			;assign the point to the winning cluster
			(setf (nth (nth 0 similar-pair) cluster-list) (snoc (aref color-buffer i j) (nth (nth 0 similar-pair) cluster-list)))
		)
	)
)

;;create a method to calculate the new centroids for the clusters
(defun create-centroids (all-clusters centroids-list &aux temp-lst sum-vec)
	*read-default-float-format*
	(dotimes (i (length all-clusters))
		(cond
			;check to see if the centroid can be updated
			((null (nth i all-clusters)) (setf temp-lst (snoc (nth i centroids-list) temp-lst)))
			(t
				(setf sum-vec '(0.0 0.0 0.0))
				(dotimes (j (length (nth i all-clusters)))
					(setf sum-vec (add-components (nth j (nth i all-clusters)) sum-vec))
				)
				(setf temp-lst (snoc (divide sum-vec (length (nth i all-clusters))) temp-lst))
			)
		)
	)
	temp-lst
)

;;create a method that performs k-means to create clusters using the RGB samples list
(defun k-means (&aux BMU-list new-centroids update-flag)
	*read-default-float-format*
	;clear the clusters
	(setf cluster-list '())
	;create K clusters
	(dotimes (i (car (array-dimensions vector-samples)))
		(setf cluster-list (snoc '() cluster-list))
	)
	;initialize the centroids
	(setf BMU-list (get-all-BMUs))
	;assign the RGB vectors to initial clusters
	(assign-to-clusters BMU-list)
	;start the iteration
	(block cluster-loop
		(dotimes (i *MAX_TRIALS*)
			;create the new centroids
			(setf new-centroids (create-centroids cluster-list BMU-list))
			;reset convergence flag
			(setf update-flag 'true)
			;check for convergence
			(block loop1
				(dotimes (j (length new-centroids))
					(if (not (eq (nth j new-centroids) (nth j BMU-list)))
						(progn
							(setf update-flag nil)
							(return-from loop1)
						)
					)
				)
			)
			(if (eq update-flag 'true)
				(return-from cluster-loop)
			)
			;update the centroids
			(setf BMU-list new-centroids)
			;empty the clusters
			(setf cluster-list '())
			;create K clusters
			(dotimes (i (car (array-dimensions vector-samples)))
				(setf cluster-list (snoc '() cluster-list))
			)
			;re-assign the data to the approporiate clusters
			(assign-to-clusters BMU-list)
		)
	)
	BMU-list
)

;;create a method that will search through all the clusters to find the cluster best associated with the vector specified
(defun cluster-search (vect centroids-list labels &aux best-cluster-number)
	;find the best matching cluster
	(dotimes (i (length centroids-list))
		(if (= i 0)
			(progn
				(setf past-value (get-dist vect (nth i centroids-list)))
				(setf best-cluster-number i)
			)
			(progn
				(if (< (get-dist vect (nth i centroids-list)) past-value)
					(progn
						(setf past-value (get-dist vect (nth i centroids-list)))
						(setf best-cluster-number i)
					)
				)
			)
		)
	)
	best-cluster-number
)

;;create a method that will prompt the user to label the clustered data
(defun whatis (vect &aux response label cluster-number)
	(if (eq status 'uncomplete)
		(progn
			;use k-means clustering
			(setf centroids (k-means))
			;create a list to contain all the labels for the corresponding clusters
			(dotimes (i (length centroids))
				(setf label-list (snoc nil label-list))
			)
			(setf status 'complete)
		)
	)
	
	;check to see if the best corresponding cluster to the RGB vector is labeled
	(setf cluster-number (cluster-search vect centroids label-list))
	(setf response (nth cluster-number label-list))
	(cond
		((not (null response)) response)
		(t
			;prompt the user for a label for the cluster
			(print "This Cluster Currently Has No Label. Please Give It A Label: ")
			(setf label (read))
			(terpri)
			
			;assign the winning cluster with the label
			(setf (nth cluster-number label-list) label)
		)
	)
)