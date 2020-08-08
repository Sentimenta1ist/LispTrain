;;----------------awhen----------------------

(defmacro awhen (expr &body body)
  `(let ((it ,expr))
     (declare (ignorable it))
     (when it ,@body)))

(defun test-awhen ()
  (check-func "awhen" (awhen (member 1 '(2 1 3) :test #'eql) it) '(1 3))
  (check-func "awhen" (awhen (member 1 '(1) :test #'eql) it) '(1))
  (check-func "awhen" (awhen (< 2 3) it) t)
  (check-func "awhen" (awhen (find 3 '(1 2 3)) it) 3))


;;------------------aif---------------------

(defmacro aif (expr true &optional else)
  (let ((this-expr (gensym)))
    `(let ((,this-expr ,expr))
       (if ,this-expr
	   (let ((it ,this-expr))
	     (declare (ignorable it))
	     ,true)
	   ,else))))

(defun test-aif ()
  (check-func "aif" (aif (member 1 '(2 1 3) :test #'eql) it nil) '(1 3))
  (check-func "aif" (aif (member 3 '(2 1 3) :test #'eql) it nil) '(3))
  (check-func "aif" (aif (find 2 '(1 2 3)) it nil) 2))
  

;;------------------cut---------------------

(defmacro cut (&rest rest)
  (let ((lst-vars '()))
    (labels ((%rec (arg)
	       (if (listp arg)
		   (mapcar #'%rec arg)
		   (if (eq arg '_)
		       (let ((var (gensym)))
			 (push var lst-vars)
			 var)
		       arg))))
      (let ((res (%rec rest)))
	`(lambda (,@(reverse lst-vars)) ,res)))))

(defun test-cut ()
  (format t "-----TEST CUT MACRO------~%")
  (format t " List:~% ~A~% Result:" '(cut lst a (_ (_)) d _))
  (print (macroexpand-1 '(cut lst a (_ (_)) d _)))

  (format t "~%~% List: ~A ~%Result:" '(cut list _ a))
  (print (macroexpand-1  '(cut list _ a)))
  
  (format t "~%~% List: ~A ~%Result:" '(cut (((((_))))_)))
  (print (macroexpand-1  '(cut (((((_))))_))))

  (format t "~%~% List: ~A ~%Result:" '(cut a b c))
  (print (macroexpand-1  '(cut a b c)))

  (format t "~%~% List: ~A ~%Result:" '(cut 1 _ (_ 2)))
  (print (macroexpand-1  '(cut 1 _ (_ 2))))
  nil)


;;-----------------tests-------------------
  
(defun check-func (name function expected)
  (let ((result function))
    (unless (equal result expected)
      (format t "Result of '~A' doesn't match expected result!~%
                   Actual: ~A ~% Expected: ~A ~% ~%~%"
	      name result expected))))

(defun all-tests ()
  (test-awhen)
  (test-aif)
  (test-cut))
	      
