;;------------------with-gensyns--------------------

(defmacro with-gensyms (argz-main &body body)
  (labels ((%rec (argz)
	     (when argz
	       (cons (list (first argz) '(gensym))
		     (%rec (rest argz))))))
    `(let ,(%rec argz-main) ,@body)))

(defmacro with-gensyms-2 (argz &body body)
  `(let ,(mapcar (lambda (x) (list x '(gensym))) argz) 
     ,@body))


(defun test-with-gensyms ()
  (format t "~%~%-----TEST WITH-GENSYMS MACRO------~%")
  
  (format t " ~%List:~% ~A~%~% Result:" '(with-gensyms (a b)`(let* ((,a (+ 1 2))
								    (,b (1+ a)))
							       (+ ,a ,b))))
  (print (macroexpand-1 '(with-gensyms (a b)`(let* ((,a (+ 1 2))
						    (,b (1+ a)))
					       (+ ,a ,b)))))

  (format t " ~%~%~%List:~% ~A~% Result:" '(with-gensyms (a b c)`(let ((,a 1)
								       (,b 2)
								       (,c 3))
								   (list ,a ,b c))))
  (print (macroexpand-1 '(with-gensyms (a b c)`(let ((,a 1)
						     (,b 2)
						     (,c 3))
						 (list ,a ,b c)))))
  ;;------------------------------------------------------------
  (format t "~%~%~%-----TEST WITH-GENSYMS2 MACRO------~%")
  (format t " ~%List:~% ~A~% Result:" '(with-gensyms (a b)`(let* ((,a (+ 1 2))
								  (,b (1+ a)))
							     (+ ,a ,b))))
  (print (macroexpand-1 '(with-gensyms (a b)`(let* ((,a (+ 1 2))
						    (,b (1+ a)))
					       (+ ,a ,b)))))

  (format t " ~%~%~%List:~% ~A~% Result:" '(with-gensyms (a b c)`(let ((,a 1)
								       (,b 2)
								       (,c 3))
								   (list ,a ,b c))))
  (print (macroexpand-1 '(with-gensyms (a b c)`(let ((,a 1)
						     (,b 2)
						     (,c 3))
						 (list ,a ,b c)))))
  nil)



;;----------------------aand-----------------------

(defmacro aand (&rest forms-main)
  (labels ((%rec (forms)
	     (if (rest forms)
		 `(let ((it ,(first forms)))
		     (declare (ignorable it))
		      (when it
			  ,(%rec (rest forms))))
		   (first forms))))
    (if forms-main
	(%rec forms-main)
	t)))

(defun test-aand ()
  (check-func "aand" (aand 1 (* it 2) (+ it 10) it) 12)
  (check-func "aand" (aand (< 1 2) (eq it t)) t)
  (check-func "aand" (aand) t))
  

;;--------------------------acond-----------------------

(defmacro acond (&rest forms-main)
  (labels ((%rec (forms &aux (expr (first forms)) (work (rest (first forms))))
	     (when forms
	       (let ((this-exp (gensym)))
		 `(let ((,this-exp ,(first expr)))  
		    (if ,this-exp
			(let ((it ,this-exp))
			  (declare (ignorable it))
			  (if ,(null work)
			      ,this-exp
			      (progn ,@work)))
			,(%rec (rest forms))))))))
    (%rec forms-main)))

(defun test-acond ()
  (check-func "acond" (acond ((member '2 '(1 2 3)) it)  (t t)) '(2 3))
  (check-func "acond" (acond (1 2 3)) 3)
  (check-func "acond" (acond (1)) 1)
  (check-func "acond" (let ((it 1)) (acond (nil nil) (it 2))) 2))
  
  
;;------------------mv-let*---------------------	       
	     
(defmacro mv-let* (vars-main &body body)
  (labels ((%rec (vars &aux (var (first vars)))
	     (if vars
		 (if (or (atom var) (atom (first var)))
		     `((let (,var) ,@(%rec (rest vars))))
		     `((multiple-value-bind ,(first var) ,@(rest var)
			 ,@(%rec (rest vars)))))
	         `((progn ,@body)))))
      	(car (%rec vars-main))))

(defun test-mv-let* ()
  (check-func "mv-let*"
	      (mv-let* (a
			(b 1)
			((c d) (values (not a) (1+ b))))
		(list a b c d))
	      '(nil 1 t 2))
  (check-func "mv-let*"
	      (mv-let* (a
			(b 1)
			((c d) (values (not a) (1+ b))))
		(list a b c d) (list a b c d a))
	      '(nil 1 t 2 nil))
  (check-func "mv-let*"
	      (mv-let* (((c d) (values 1 2)))
		(cons (cons c d) nil))
	      '((1 . 2))))

  



;;-----------------all-tests-------------------

(defun check-func (name function expected)
  (let ((result function))
    (unless (equal result expected)
      (format t "Result of '~A' doesn't match expected result!~%
                   Actual: ~A ~% Expected: ~A ~% ~%~%"
	      name result expected))))

(defun tests ()
  (test-with-gensyms)
  (test-aand)
  (test-acond)
  (test-mv-let*))
