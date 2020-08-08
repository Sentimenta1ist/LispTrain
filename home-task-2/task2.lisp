;bubble imperative sorting and testing

(defun bubble-sort-imperative (lst)
  (dotimes (r (- (length lst) 1))
    (dotimes (j (- (- (length lst) 1) r))
      (when (> (nth j lst) (nth (+ j 1) lst))
	(let ((tmp (nth (+ j 1) lst)))
	  (setf (nth (+ j 1) lst) (nth j lst))
	  (setf (nth j lst) tmp)))))
  lst)

(defun check-bubble-imp (input expected)
  "Utility function for 'bubble-sort-imperative' result check"
  (let ((result (select-sort-imperative input)))
    (unless (equal result expected)
      (format t "Result of 'bubble-sort-imperative' doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
              result
              expected))))




;select imperative sorting and testing

(defun select-sort-imperative (lst)
  (dotimes (r (- (length lst) 1))
    (let ((min-elem (nth r lst)) (index-min r))
      (do ((j (+ 1 r) (1+ j)))
	  ((>= j (length lst)))
	(when (<=  (nth j lst) min-elem)
	  (setf min-elem (nth j lst))
	  (setf index-min j)))
      (setf (nth index-min lst) (nth r lst))      
      (setf (nth r lst) min-elem)))
  lst)

(defun check-select-imp (input expected)
  "Utility function for 'select-sort-imperative' result check"
  (let ((result (select-sort-imperative input)))
    (unless (equal result expected)
      (format t "Result of select-sort-imperative doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
              result
              expected))))




;insert imperative sorting and testing

(defun insert-sort-imperative (lst)
  (loop
    for i from 1 to (- (length lst) 1) do
      (let ((tmp (nth i lst)) (j (1- i)))
            (loop repeat (1+ j) do
	      (when (> (nth j lst) tmp)
		(setf (nth (1+ j) lst) (nth j lst)))
		  unless (> (nth j lst) tmp)
		    do (return)
		  do (decf j))
	(setf (nth (1+ j) lst) tmp)))
  lst)

(defun check-insert-imp (input expected)
  "Utility function for 'insert-sort-imperative' result check"
  (let ((result (insert-sort-imperative input)))
    (unless (equal result expected)
      (format t "Result of insert-sort-imperative doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
              result
              expected))))




;select recursive sorting and testing

(defun minimum (lst)
  (if (rest lst)
      (let ((min-element (minimum (rest lst))))
	(if (< (first lst) min-element)
	    (first lst)
	    min-element))
      (first lst)))

(defun select-sort-rec (lst)
  (when lst
     (let ((min-element (minimum lst)))
    (cons min-element (select-sort-rec (remove min-element lst :count 1))))))

(defun select-sort-rec2 (lst)
  (when lst
    (let ((min-element (apply #'min lst)))
      (cons min-element (select-sort-rec (remove min-element lst :count 1))))))

(defun check-select-rec (input expected)
  "Utility function for 'select-sort-rec' result check"
  (let ((result (select-sort-rec input)))
    (unless (equal result expected)
      (format t "Result of select-sort-rec doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
              result
              expected))))

(defun check-select-rec2 (input expected)
  "Utility function for 'select-sort-rec2' result check"
  (let ((result (select-sort-rec input)))
    (unless (equal result expected)
      (format t "Result of select-sort-rec2 doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
              result
              expected))))



;insert recursive sorting and testing

(defun insert-element (element lst)
  (if (null lst)
      (list element)
      (if (< (first lst) element)
	  (cons (first lst) (insert-element element (rest lst)))
	  (cons element lst))))

(defun insert-sort-rec (lst &optional sorted)
  (if lst
      (insert-sort-rec (rest lst) (insert-element (first lst) sorted))
      sorted))

(defun insert-sort-rec-local (lst-main)
  (labels ((%insert-sort-rec (lst)
	     (when lst
		 (%insert-element (first lst) (%insert-sort-rec (rest lst)))))
	   (%insert-element(element lst)
	     (if (null lst)
		 (list element)
		 (if (< (first lst) element)
		     (cons (first lst) (%insert-element element (rest lst)))
		     (cons element lst)))))
    (%insert-sort-rec lst-main)))

(defun check-insert-rec (input expected)
  "Utility function for 'insert-sort-rec' result check"
  (let ((result (insert-sort-rec input)))
    (unless (equal result expected)
      (format t "Result of insert-sort-rec doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
              result
              expected))))

(defun check-insert-rec-local (input expected)
    "Utility function for 'insert-sort-rec-local' result check"
  (let ((result (insert-sort-rec-local input)))
    (unless (equal result expected)
      (format t "Result of insert-sort-rec-local doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
              result
              expected))))




;bubble recursive sorting and testing

(defun bubble-element (element lst)
  (if lst
      (if (null element)
	 (bubble-element (first lst) (rest lst))
	 (if (> (first lst) element)
	     (cons element (bubble-element (first lst) (rest lst)))
	     (cons (first lst) (bubble-element element (rest lst)))))
      (cons element nil)))

(defun bubble-sort-rec (lst &optional sorted)
  (if lst
      (bubble-sort-rec (rest lst) (bubble-element (first lst) sorted))
      sorted))

(defun bubble-sort-rec-local (lst-main)
  (labels ((%bubble-element (element lst)
	     (if lst
		 (if (null element)
		    (%bubble-element (first lst) (rest lst))
		    (if (> (first lst) element)
			(cons element (%bubble-element (first lst) (rest lst)))
			(cons (first lst) (%bubble-element element (rest lst)))))
		 (cons element nil)))
	   (%bubble-sort-rec (lst &optional sorted)
	     (if lst
		 (%bubble-sort-rec (rest lst) (%bubble-element (first lst) sorted))
		 sorted)))
	   (%bubble-sort-rec lst-main)))
	     


(defun check-bubble-rec (input expected)
  "Utility function for 'bubble-sort-rec' result check"
  (let ((result (bubble-sort-rec input)))
    (unless (equal result expected)
      (format t "Result of bubble-sort-rec doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
              result
              expected))))

(defun check-bubble-rec-local (input expected)
  "Utility function for 'bubble-sort-rec-local' result check"
  (let ((result (bubble-sort-rec-local input)))
    (unless (equal result expected)
      (format t "Result of bubble-sort-rec-local doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
              result
              expected))))


(defun check-all (input expected)
  (check-select-imp input expected)
  (check-insert-imp input expected)
  (check-bubble-imp input expected)
  (check-select-rec input expected)
  (check-bubble-rec input expected)
  (check-insert-rec input expected)
  (check-select-rec2 input expected)
  (check-insert-rec-local input expected)
  (check-bubble-rec-local input expected))




(defun test-all ()
  (check-all '(1 2 3 3 2 1) '(1 1 2 2 3 3))
  (check-all '() '())
  (check-all '(1 2 3) '(1 2 3))
  (check-all '(5 4 3 2) '(2 3 4 5))
  ;;(check-all '(1 2 3 2 1) '(1 1 2 2 2)) ;;error input
  (check-all '(1 2 3 2 1) '(1 1 2 2 3)) ;;correct input
  )
