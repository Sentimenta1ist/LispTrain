
;---------------1-task------------------

(defun create-set (args)
  (lambda (elem)
    (when (member elem args :test #'eql) t)))


(defun my-union (contains-a contains-b)
  (lambda (elem)
    (or (funcall contains-a elem)  (funcall contains-b elem))))

(defun my-intersect (contains-a contains-b)
  (lambda (elem)
    (and (funcall contains-a elem)  (funcall contains-b elem))))

(defun my-difference (contains-a contains-b)
  (lambda (elem)
    (and (funcall contains-a elem)  (not (funcall contains-b elem)))))

(defun check-create (set-1 element expected)
  (let ((result (let* ((contains-1 (create-set set-1)))  
    (funcall contains-1 element))))
    (unless (equal result expected)
      (format t "Result of 'create-set' doesn't match expected result!~%
                 Actual: ~A~%Expected: ~A~%"
              result expected))))

(defun check-operation (operation set-1 set-2 element expected)
  (let ((result (let* ((contains-1 (create-set set-1))
		       (contains-2 (create-set set-2))
		       (result-cont (funcall operation contains-1 contains-2)))  
    (funcall result-cont element))))
    (unless (equal result expected)
      (format t "Result of ~A doesn't match expected result!~%Actual: ~A~%
                 Expected: ~A~%"
	      operation result expected))))


(defun test-all-sets ()
  (check-create '(1 2 3) 1 t)
  (check-create '(1) 1 t)
  (check-create '(1 2 3) 5 nil)
  (check-create '(1 2 3) 4 nil)
  (check-create '() 10 nil)
  (check-operation 'my-union '(1 2 3) '(4 5 6) 1 T)
  (check-operation 'my-union '(1 2 3) '(4 5 6) 5 T)
  (check-operation 'my-union '() '(4 5 6) 5 T)
  (check-operation 'my-union '(1 2 3) '() 4 nil)
  (check-operation 'my-intersect '(1 2 3) '(4 5 6) 1 nil)
  (check-operation 'my-intersect '(1 2 3) '(1 4 5 6) 1 T)
  (check-operation 'my-intersect '() '(4 5 6) 1 nil)
  (check-operation 'my-intersect '(2) '(3 2) 2 T)
  (check-operation 'my-difference '(1 2) '(2 3) 1 T)
  (check-operation 'my-difference '(1 2) '(2 3) 3 nil)
  (check-operation 'my-difference '(2) '() 2 T)
  (check-operation 'my-difference '(1 2 3) '(1 2 3) 3 nil)
  (check-operation 'my-difference '(2) '(2) 2 nil))





;------------------2-task---------------------

(defun gen-my-sum ()
  (let ((sum 0))
    (lambda (x)
      (incf sum x))))

(defun check-gen-sum (input expected)
  (let ((result (mapcar (gen-my-sum) input)))
    (unless (equal result expected)
      (format t "Result of 'gen-my-sum' doesn't match expected result!~%
                 Actual: ~A~%Expected: ~A~%"
              result expected))))

(defun test-gen-sum ()
  (check-gen-sum '(1 1 1 1) '(1 2 3 4))
  (check-gen-sum '(1 2 3 4) '(1 3 6 10))
  (check-gen-sum '(1 0 0 0) '(1 1 1 1))
  (check-gen-sum '() '()))





;-----------------3-task---------------------

(defun create-eraser (k)
  (lambda (lst-main)
    (labels ((%counter (lst)
	       (when lst	       
		 (if (>= (count (first lst) lst) k)
		     (cons (first lst) (%counter (remove (first lst) (rest lst))))
		     (%counter (rest lst))))))
      (%counter lst-main))))

(defun check-erase (input k expected)
  (let ((result (funcall (create-eraser k) input)))
    (unless (equal result expected)
      (format t "Result of 'create eraser' doesn't match expected result!~%
                 Actual: ~A~%Expected: ~A~%"
              result expected))))

(defun test-erase ()
  (check-erase '(1 2 3 3 2 3 a a c a a a) 3 '(3 a))
  (check-erase '(1 2 2 3 4 4 5) 1 '(1 2 3 4 5))
  (check-erase '(1 2 3) 5 '())
  (check-erase '() nil '()))





;-----------------4-task---------------------


(defun num-orders (lst)
  (labels ((%factorial (k)
	     (if ( <= k 1)
		 1
		 (* k (%factorial (1- k))))))
    (/ (%factorial (apply #'+ lst))
       (reduce #'* lst :key #'%factorial))))
	   
(defun check-num-orders (input expected)
  (let ((result (num-orders input)))
    (unless (equal result expected)
      (format t "Result of 'num-orders' doesn't match expected result!~%
                 Actual: ~A~%Expected: ~A~%"
	      result expected))))

(defun test-num-orders ()
  (check-num-orders '(1 1 1) 6)
  (check-num-orders '(1 2) 3)
  (check-num-orders '(1 1) 2)
  (check-num-orders '(1 2 3 4) 12600))




;-----------------5-task---------------------


(defun quicksort (lst-main)
  (labels ((%quicksort (lst lst-new)
	     (cond 
	       ((null lst) lst-new)
	       ((null (rest lst)) (cons (first lst) lst-new))
	       (t (let* ((pivot (first lst))
			 (lst-low (remove-if (lambda (x) (>= x pivot)) (rest lst)))
			 (lst-high (remove-if (lambda (x) (< x pivot)) (rest lst))))
		    (%quicksort lst-low (cons pivot (%quicksort lst-high lst-new))))))))
    (%quicksort lst-main nil)))


(defun quicksort2 (lst-main)
  (labels ((%quicksort (lst lst-new)
	     (cond 
	       ((null lst) lst-new)
	       (t (let* ((pivot (first lst))
			 (result (%part (rest lst) (lambda (x) (< x pivot)))))			
		    (%quicksort (first result) (cons pivot (%quicksort (rest result) lst-new)))))))
	   (%part (lst predicate &optional lst-low lst-high)	
	     (cond
	       ((null lst) (cons lst-low lst-high))
	       ((funcall predicate (first lst)) (%part (rest lst) predicate (cons (first lst) lst-low) lst-high))
	       (t (%part (rest lst) predicate lst-low (cons (first lst) lst-high))))))
    (%quicksort lst-main nil)))


(defun quicksort3 (lst-main)
  (labels ((%quicksort (lst lst-new)
	     (cond 
	       ((null lst) lst-new)
	       (t (let* ((pivot (first lst))
			 (result (multiple-value-list (%part (rest lst) (lambda (x) (< x pivot))))))			
		    (%quicksort (first result) (cons pivot (%quicksort (second result) lst-new)))))))
	   (%part (lst predicate)	
	     (when lst
	       (multiple-value-bind (low high) (%part (rest lst) predicate)
		 (if (funcall predicate (first lst))
		     (values (cons (first lst) low) high)
		     (values low (cons (first lst) high)))))))	 
    (%quicksort lst-main nil)))

(defun check-quicksort (input expected)
  (let ((result (quicksort input)))
    (unless (equal result expected)
      (format t "Result of 'quicksort' doesn't match expected result!~%
                 Actual: ~A~%Expected: ~A~%"
	      result expected))))

(defun check-quicksort3 (input expected)
  (let ((result (quicksort3 input)))
    (unless (equal result expected)
      (format t "Result of 'quicksort2' doesn't match expected result!~%
                 Actual: ~A~%Expected: ~A~%"
	      result expected))))



(defun test-quicksort ()
  (check-quicksort '(1 2 3 1) '(1 1 2 3))
  (check-quicksort '(2 1) '(1 2))
  (check-quicksort '() '())
  (check-quicksort '(1 2 3 1 2 3 1 2 3 2 1 1 1 2) '(1 1 1 1 1 1 2 2 2 2 2 3 3 3))
  (check-quicksort3 '(1 2 3 1) '(1 1 2 3))
  (check-quicksort3 '(2 1) '(1 2))
  (check-quicksort3 '() '())
  (check-quicksort3 '(1 2 3 1 2 3 1 2 3 2 1 1 1 2) '(1 1 1 1 1 1 2 2 2 2 2 3 3 3)))



;-------------------tests---------------------

(defun all-tests()
  (test-all-sets)
  (test-gen-sum)
  (test-erase)
  (test-num-orders)
  (test-quicksort))
