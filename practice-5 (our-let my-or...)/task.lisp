;;-----------------our-let--------------------

(defmacro our-let (vars &body body)
  `((lambda ,(mapcar (lambda (x)
		       (if (listp x)
			   (first x)
			   x))
	      vars)
      ,@body)
    ,@(mapcar (lambda (x)
		(when (listp x)
		  (second x)))
	      vars)))

(defun test-our-let ()
  (check-func "our-let" (our-let ((a 1) (b 2) c) (cons a (cons b c))) '(1 2))
  (check-func "our-let" (our-let (a c) (cons a c)) '(nil))
  (check-func "our-let" (our-let ((a 1) (b 2) (c 3)) (cons a (cons b c))) '(1 2 . 3)))


;;-----------------our-let*-------------------

(defmacro our-let* (vars-main &body body)
  (labels ((%rec (vars)
	     (if vars
		 `((lambda (,(if (listp (first vars))
				 (car (first vars))
				 (first vars)))
		     ,(if (rest vars)
			  (%rec (rest vars))
			  `(progn ,@body)))
			   
		   ,(when (listp (first vars))
		      (second (first vars)))))))
    (%rec vars-main)))

(defun test-our-let* ()
  (check-func "our-let*" (our-let* ((a 1) (b a) c) (cons a (cons b c))) '(1 1))
  (check-func "our-let*" (our-let* (a c) (cons a c)) '(nil))
  (check-func "our-let*" (our-let* ((a 1) (b 2) (c (+ a b))) (cons a (cons b c))) '(1 2 . 3))
  (check-func "our-let*" (our-let* ((a 1) (c '(3))) (cons a c)) '(1 3))
  (check-func "our-let*" (our-let* ((a 1) c) (list a c) (list c a)) '(nil 1)))


;;-----------------our-let*-2-----------------

(defmacro our-let*-2 (vars-main &body body)
  (labels ((%rec (vars)
	     (if vars
		 `((let (,(first vars))
		     ,@(%rec (rest vars))))
	     body)))
    (first (%rec vars-main))))

(defun test-our-let*-2 ()
  (check-func "our-let*-2" (our-let*-2 ((a 1) (b a) c) (cons a (cons b c))) '(1 1))
  (check-func "our-let*-2" (our-let*-2 (a c) (cons a c)) '(nil))
  (check-func "our-let*-2" (our-let*-2 ((a 1) (b 2) (c (+ a b))) (cons a (cons b c))) '(1 2 . 3))
  (check-func "our-let*-2" (our-let*-2 ((a 1) (c '(3))) (cons a c)) '(1 3))
  (check-func "our-let*-2" (our-let*-2 ((a 1) c) (list a c) (list c a)) '(nil 1)))


;;-----------------my-or-------------------

(defmacro my-or (&rest args-main)
      (labels ((%rec (args)
		 (when args
		     (let ((val (gensym)))
		       `(let ((,val ,(first args)))
			  (if ,val
			      ,val
			      ,(%rec (rest args))))))))
	(%rec args-main)))

(defun test-my-or ()
  (check-func "my-or" (my-or (> 1 2) (> 3 4)) nil)
  (check-func "my-or" (my-or (> 1 2) (> 3 4) (> 1 0)) t)
  (check-func "my-or" (my-or (> 1 2) (member 2 '(1 2 3))) '(2 3))
  (check-func "my-or" (my-or) nil))
  

;;-----------------alambda-------------------

(defmacro alambda (args-main &body body)
  `(labels ((self ,args-main ,@body))
     #'self))

(defun test-alambda ()
  (check-func "alambda" (funcall (alambda (x) (if (= x 0) 1 (* x (self (1- x))))) 4) 24)
  (check-func "alambda" (funcall (alambda (lst) (if lst (+ (first lst) (self (rest lst))) 0)) '(1 2 3 4)) 10)
  (check-func "alambda" (funcall (alambda (lst) (if (rest lst) (self (rest lst)) (first lst))) '(1 2 3 4 5)) 5))
  
  
;;-----------------tests--------------------

(defun check-func (name function expected)
  (let ((result function))
    (unless (equal result expected)
      (format t "Result of '~A' doesn't match expected result!~%
                   Actual: ~A ~% Expected: ~A ~% ~%~%"
	      name result expected))))

(defun all-tests ()
  (test-our-let)
  (test-my-or)
  (test-our-let*)
  (test-our-let*-2)
  (test-alambda))


		  


