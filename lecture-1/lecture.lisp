(defun my-flatten (lst-main)
  (labels ((%my-flatten (lst lst-new)
	     (if lst
		 (let ((deep-lst (%my-flatten (rest lst) lst-new)))
		   (if (listp (first lst))
		       (%my-flatten (first lst) deep-lst)
	    	       (cons (first lst) deep-lst)))
		 lst-new)))
    (%my-flatten lst-main nil)))
 


(defun compose (&rest funcs)
  (lambda (&rest arg)
    (labels ((%compose-one (func)
               (if (rest func)
		   (funcall (first func) (%compose-one (rest func)))
		   (apply (first func) arg))))
      (%compose-one funcs))))

(defun compose2 (&rest funcs)
  (lambda (&rest arg)
    (first
     (reduce (lambda (x acc)
	       (cons (apply x acc) nil))
	     funcs
	     :from-end t
	     :initial-value arg))))

(defun check-flutten (input expected)
  "Utility function for 'my-flatten' result check"
  (let ((result (my-flatten input)))
    (unless (equal result expected)
      (format t "Result of 'my-flatten' doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
              result
              expected))))


(defun check-compose (input-lst input-funcs expected)
  (let ((result (apply (eval (cons 'compose2 input-lst)) input-funcs)))
    (unless (equal result expected)
      (format t "Result of COMPOSE doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
        result
        expected))))

(defun test-all ()
  (check-flutten '(1 2 3) '(1 2 3))
  (check-flutten '(1 (2) 3) '(1 2 3))
  (check-flutten '(1 (2 (3) ((((4)))))) '(1 2 3 4))
  (check-flutten '((1 2 (4) 3)) '(1 2 4 3))
  (check-flutten '(1 ((((((2))))))2 3) '(1 2 2 3))
  (check-flutten '(1 2 3) '(1 2 3))
  (check-flutten '(((()))((())((())))) nil)
  (check-flutten '((1 (2) 3 (4)((((4)))) (5 (6)))) '(1 2 3 4 4 5 6))
  (check-compose '(#'1+ #'1-) '(5) 5)
  (check-compose '(#'1+ #'1+ #'1+ #'1+) '(5) 9)
  (check-compose '(#'list #'1-) '(5) '(4))
  (check-compose '(#'rest #'list) '(1 2) '(2)))
