;-------------------------my-remove------------------------

(defun my-remove (elem lst-main &key (key #'identity) (test #'eql))
  (labels ((%copy-list (lst)
	     (when lst
	       (if (funcall test elem (funcall key (first lst)) )
		   (%copy-list (rest lst))
		   (cons (first lst) (%copy-list (rest lst)))))))
    (%copy-list lst-main)))

(defun test-my-remove()
  (check-function "my-remove" (remove 1 '((1 2) (2 3) (1 3)) :key #'car) '((2 3)))
  (check-function "my-remove" (remove 4 '(4 4 4 4)) nil)
  (check-function "my-remove" (remove '(1 2) '((1) (1 2) (2) (3 1)) :test #'equal) '((1) (2) (3 1)))
  (check-function "my-remove" (remove nil '(1 2 nil 3)) '(1 2 3)))



;----------------------find-best-element-----------------------

 
(defun find-best-element (lst test &key (key #'identity))
  (labels ((%find-best-old (lst-old)
             (if (rest lst-old)
		 (let (( check-best (%find-best-old (rest lst-old)))
		       ( first-key (funcall key (first lst-old))))
		   (if ( funcall test first-key (cdr check-best))
		       (cons (first lst-old) first-key)
		       check-best))
		 (cons (first lst-old) (funcall key (first lst-old))))))
    (unless (null lst)
      (car (%find-best-old lst)))))

(defun find-best-element2 (lst test &key (key #'identity))
  (car (reduce
   (lambda (acc x)
     (let (( first-key (funcall key x)))
       (if (funcall test (cdr acc) first-key) acc  (cons x first-key))))
   (rest lst)
   :initial-value (cons (first lst) (funcall key (first lst))))))

(defun test-my-find-best()
  (check-function "find-best-element" (find-best-element '((1 2 3) (1 2) () (1 2 3 4)) #'> :key #'length) '(1 2 3 4))
  (check-function "find-best-element" (find-best-element '(1 2 3 4) #'> ) 4)
  (check-function "find-best-element" (find-best-element '() #'> ) nil)
  (check-function "find-best-element" (find-best-element '(1 2 3 4) #'< ) 1)
  (check-function "find-best-element" (find-best-element '((1 2 3) (1 2) () (1 2 3 4)) #'< :key #'length) '())
  
  (check-function "find-best-element2" (find-best-element2 '((1 2 3) (1 2) () (1 2 3 4)) #'> :key #'length) '(1 2 3 4))
  (check-function "find-best-element2" (find-best-element2 '(1 2 3 4) #'> ) 4)
  (check-function "find-best-element2" (find-best-element2 '() #'> ) nil)
  (check-function "find-best-element2" (find-best-element2 '(1 2 3 4) #'< ) 1)
  (check-function "find-best-element2" (find-best-element2 '((1 2 3) (1 2) () (1 2 3 4)) #'< :key #'length) '()))




;------------------------make-mapcar-if-----------------------

(defun make-mapcar-if (predicate func-1)
  (lambda (pred)
    (if (funcall predicate pred)
	(funcall func-1 pred)
	pred)))

(defun test-make-mapcar-if()
  (check-function "make-mapcar-if" (mapcar (make-mapcar-if #'oddp #'1+) '(1 2 3)) '(2 2 4))
  (check-function "make-mapcar-if" (mapcar (make-mapcar-if #'oddp #'1-) '(1 2 3)) '(0 2 2))
  (check-function "make-mapcar-if" (mapcar (make-mapcar-if #'(lambda (x) (<= x 3)) #'1+) '(1 2 3 5 5)) '(2 3 4 5 5))
  (check-function "make-mapcar-if" (mapcar (make-mapcar-if #'oddp #'1+) '()) '()))
  



;------------------------assoc-member-----------------------

(defun assoc-member (key-element lst &key test key)
  (when lst
    (if (member key-element (first (first lst)) :key key :test test)
	(first lst)
	(assoc-member key-element (rest lst) :key key :test test))))

(defun assoc-member2 (key-element lst &key test key)
  (first (member-if #'(lambda (lstx) (member key-element (first lstx) :key key :test test)) lst)))

(defun assoc-member3 (key-element lst &key test key)
  (find-if #'(lambda (lstx) (member key-element (first lstx) :key key :test test)) lst))

(defun test-assoc()
  (check-function "assoc-member" (assoc-member :dos '(((:uno :dos) first) ((:tres) second)) :test #'eq) '((:uno :dos) first))
  (check-function "assoc-member" (assoc-member :dos '((nil first) ((:uno (:dos)) second) ((:dos :tres) third) (:dos fourth)) :test #'eq) '((:dos :tres) third))
  (check-function "assoc-member" (assoc-member 4 '(((-1 2) first) ((3 4) second)) :test #'< :key #'1+) '((3 4) second))
  
  (check-function "assoc-member2" (assoc-member2 :dos '(((:uno :dos) first) ((:tres) second)) :test #'eq) '((:uno :dos) first))
  (check-function "assoc-member2" (assoc-member2 :dos '((nil first) ((:uno (:dos)) second) ((:dos :tres) third) (:dos fourth)) :test #'eq) '((:dos :tres) third))
  (check-function "assoc-member2" (assoc-member2 4 '(((-1 2) first) ((3 4) second)) :test #'< :key #'1+) '((3 4) second))
  
  (check-function "assoc-member3" (assoc-member3 :dos '(((:uno :dos) first) ((:tres) second)) :test #'eq) '((:uno :dos) first))
  (check-function "assoc-member3" (assoc-member3 :dos '((nil first) ((:uno (:dos)) second) ((:dos :tres) third) (:dos fourth)) :test #'eq) '((:dos :tres) third))
  (check-function "assoc-member3" (assoc-member3 4 '(((-1 2) first) ((3 4) second)) :test #'< :key #'1+) '((3 4) second)))



;------------------------my-reduce-----------------------

(defun my-reduce (func lst-main &key from-end (initial-value nil initial-value-set-p)  (key #'identity))
  (labels ((%rec-reduce (lst)
	     (if (rest lst)
		 (if from-end
		     (funcall func (funcall key (first lst)) (%rec-reduce (rest lst)))
		     (funcall func (%rec-reduce (rest lst)) (funcall key (first lst))))
		 (if initial-value-set-p
		     (if from-end
			 (funcall func (funcall key (first lst)) initial-value)
			 (funcall func initial-value (funcall key (first lst))))		     
		     (funcall key (first lst))))))
    (if from-end
	(if lst-main
	    (%rec-reduce lst-main)
	    initial-value)
	(%rec-reduce (reverse lst-main)))))

(defun my-reduce2 (func lst-main &key from-end (initial-value nil initial-value-set-p)  (key #'identity))
  (labels ((%rec-reduce-from-begin (lst init-lst)
	     (if lst
		 (%rec-reduce-from-begin (rest lst) (funcall func init-lst (funcall key (first lst))))
		 init-lst))
	   (%rec-reduce-from-end (lst) 
	     (if (rest lst)
		 (funcall func (funcall key (first lst)) (%rec-reduce-from-end (rest lst)))
		 (if initial-value-set-p
		     (funcall func (funcall key (first lst)) initial-value)
		     (funcall key (first lst))))))
    (if from-end
	(if lst-main
	    (%rec-reduce-from-end lst-main)
	    initial-value)
	(if initial-value-set-p
	    (%rec-reduce-from-begin lst-main initial-value)
	    (%rec-reduce-from-begin (rest lst-main) (funcall key (first lst-main)))))))

(defun my-reduce3 (func lst-main &key from-end (initial-value nil initial-value-set-p)  (key #'identity))
  (let ((divorce-func (if from-end
			  (lambda (x init-lst)
			    (funcall func x init-lst))
			  (lambda (x init-lst)
			    (funcall func init-lst x)))))
  
    (labels ((%rec-reduce (lst init-lst)
	       (if lst
		   (%rec-reduce (rest lst) (funcall divorce-func (funcall key (first lst)) init-lst))
		   init-lst)))
      
      (let ((lst (if from-end
		     (reverse lst-main)
		     lst-main)))
        (if initial-value-set-p
	    (%rec-reduce lst initial-value)
	    (%rec-reduce (rest lst) (funcall key (first lst))))))))

		     
		 
(defun test-my-reduce ()
  (check-function "my-reduce" (my-reduce #'+ '(5)) 5)
  (check-function "my-reduce" (my-reduce #'* '(1 2 3 4 5)) 120)
  (check-function "my-reduce" (my-reduce #'append '((1) (2)) :initial-value '(i n i t)) '(I N I T 1 2))
  (check-function "my-reduce" (my-reduce #'append '((1) (2)) :from-end t :initial-value '(i n i t)) '(1 2 I N I T))
  (check-function "my-reduce" (my-reduce #'list '(1 2 3 4)) '(((1 2) 3) 4))
  (check-function "my-reduce" (my-reduce #'list '(1 2 3 4) :from-end t) '(1 (2 (3 4))))
  (check-function "my-reduce" (my-reduce #'list '(1 2 3 4) :initial-value 'foo) '((((foo 1) 2) 3) 4))
  (check-function "my-reduce" (my-reduce #'list '(1 2 3 4) :from-end t :initial-value 'foo) '(1 (2 (3 (4 foo)))))
  (check-function "my-reduce" (my-reduce (lambda (elem acc) (format t "acc, elem: ~A, ~A~%" acc elem) elem) '() :from-end t :initial-value 0) 0)

  (check-function "my-reduce2" (my-reduce2 #'+ '(5)) 5)
  (check-function "my-reduce2" (my-reduce2 #'* '(1 2 3 4 5)) 120)
  (check-function "my-reduce2" (my-reduce2 #'append '((1) (2)) :initial-value '(i n i t)) '(I N I T 1 2))
  (check-function "my-reduce2" (my-reduce2 #'append '((1) (2)) :from-end t :initial-value '(i n i t)) '(1 2 I N I T))
  (check-function "my-reduce2" (my-reduce2 #'list '(1 2 3 4)) '(((1 2) 3) 4))
  (check-function "my-reduce2" (my-reduce2 #'list '(1 2 3 4) :from-end t) '(1 (2 (3 4))))
  (check-function "my-reduce2" (my-reduce2 #'list '(1 2 3 4) :initial-value 'foo) '((((foo 1) 2) 3) 4))
  (check-function "my-reduce2" (my-reduce2 #'list '(1 2 3 4) :from-end t :initial-value 'foo) '(1 (2 (3 (4 foo)))))
  (check-function "my-reduce2" (my-reduce2 (lambda (elem acc) (format t "acc, elem: ~A, ~A~%" acc elem) elem) '() :from-end t :initial-value 0) 0)

  (check-function "my-reduce3" (my-reduce3 #'+ '(5)) 5)
  (check-function "my-reduce3" (my-reduce3 #'* '(1 2 3 4 5)) 120)
  (check-function "my-reduce3" (my-reduce3 #'append '((1) (2)) :initial-value '(i n i t)) '(I N I T 1 2))
  (check-function "my-reduce3" (my-reduce3 #'append '((1) (2)) :from-end t :initial-value '(i n i t)) '(1 2 I N I T))
  (check-function "my-reduce3" (my-reduce3 #'list '(1 2 3 4)) '(((1 2) 3) 4))
  (check-function "my-reduce3" (my-reduce3 #'list '(1 2 3 4) :from-end t) '(1 (2 (3 4))))
  (check-function "my-reduce3" (my-reduce3 #'list '(1 2 3 4) :initial-value 'foo) '((((foo 1) 2) 3) 4))
  (check-function "my-reduce3" (my-reduce3 #'list '(1 2 3 4) :from-end t :initial-value 'foo) '(1 (2 (3 (4 foo)))))
  (check-function "my-reduce2" (my-reduce2 (lambda (elem acc) (format t "acc, elem: ~A, ~A~%" acc elem) elem) '() :from-end t :initial-value 0) 0))


  
;----------------------tests--------------------

(defun check-function (name function expected)
  (let ((result function))
    (unless (equal result expected)
      (format t "Result of '~A' doesn't match expected result!~%
                   Actual: ~A ~% Expected: ~A ~% ~%~%"
	      name result expected))))

(defun all-tests()
  (test-my-remove)
  (test-my-find-best)
  (test-make-mapcar-if)
  (test-assoc)
  (test-my-reduce))

      
