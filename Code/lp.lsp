(ql:quickload "vecto")
(in-package #:vecto)

;Contains all the functions to the list processing assignment

;returns a boolean value if there is a single object in a list
(defun singleton-p (lst-input)
	(cond
		((null lst-input) nil)
		((null (cdr lst-input)) t)
		(t nil)
	)
)

;returns the last element in the list
(defun rac (lst-input)
	(cond
		((singleton-p lst-input) (car lst-input))
		(t (rac (cdr lst-input)))
	)
)

;returns a list of the original list excluding the last elements
(defun rdc (lst-input)
	(cond
		((singleton-p lst-input) ())
		(t (cons (car lst-input) (rdc (cdr lst-input))))
	)
)

;adds an element to the end of a list
(defun snoc (element lst-input)
	(cond
		((null lst-input) (list element))
		(t (cons (car lst-input) (snoc element (cdr lst-input))))
	)
)

;returns a boolean value if the list same forwards as it is backwards
(defun palindrome-p (lst-input)
	(cond
		((null lst-input) t)
		((singleton-p lst-input) t)
		((equal (car lst-input) (rac lst-input)) (palindrome-p (cdr (rdc lst-input))))
		(t nil)
	)
)

;returns the element at the specified element in a list
(defun select (element lst-input)
	(cond
		((null lst-input) nil)
		((equal element 0) (car lst-input))
		(t (select (- element 1) (cdr lst-input)))
	)
)

;returns a random element from a list
(defun pick (lst-input)
	(select (random (list-length lst-input)) lst-input)
)

;returns the sum of all the numbers in a list
(defun sum (lst-input)
	(cond
		((null lst-input) 0)
		(t (+ (car lst-input) (sum (cdr lst-input))))
	)
)

;returns the product of all the numbers in a list
(defun product (lst-input)
	(cond
		((null lst-input) 1)
		(t (* (car lst-input) (product (cdr lst-input))))
	)
)

;generates a list containing all numbers lower than element in consecutive order
(defun iota (element)
	(cond
		((equal element 0) ())
		(t (snoc element (iota (- element 1))))
	)
)

(defun duplicate (num lst)
	(cond
		((= 0 num) ())
		((= 1 num) lst)
		(t (duplicate (- num 1) (cons (car lst) lst)))
	)
)

;generates a list containing element number of lst-input items
(defun generate (element lst-input)
	(cond
		((equal element 0) ())
		(t (cons lst-input (generate (- element 1) lst-input)))
	)
)

;returns the factorial of the given number
(defun factorial (element)
	(product (iota element))
)

;returns the power of element to the Pow number
(defun power (element Pow)
	(product (generate Pow element))
)

;generates a list based on which elements in lst-input return true from the function fun
(defun filter-in (fun lst-input)
	(cond
		((null lst-input) ())
		((funcall fun (car lst-input)) (cons (car lst-input) (filter-in fun (cdr lst-input))))
		(t (filter-in fun (cdr lst-input)))
	)
)

;generates a list based on which elements in lst-input return true from the function fun
(defun filter-out (fun lst-input)
	(cond
		((null lst-input) ())
		((funcall fun (car lst-input)) (filter-out fun (cdr lst-input)))
		(t (cons (car lst-input) (filter-out fun (cdr lst-input))))
	)
)

;removes all instances of element from the list lst-input
(defun take-from (element lst-input)
	(cond
		((null lst-input) ())
		((equal element (car lst-input)) (take-from element (cdr lst-input)))
		(t (cons (car lst-input) (take-from element (cdr lst-input))))
	)
)

;rearranges all elements in lst-input in a random order
(defun random-permutation (lst-input &aux element rest)
	(cond
		((null lst-input) ())
		(t
			(setf element (pick lst-input))
			(setf rest (remove element lst-input :count 1))
			(cons element (random-permutation rest))
		)
	)
)