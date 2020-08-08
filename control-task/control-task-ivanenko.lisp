(defstruct node
  data
  rels)



(defun check-func (name function expected)
  (let ((result function))
    (unless (equal result expected)
      (format t "Result of '~A' doesn't match expected result!~%
                   Actual: ~A ~% Expected: ~A ~% ~%~%"
	      name result expected))))


;;--------------task1----------------

(defun build-tree (lst)
  (when lst
      (if (atom lst)
	  (make-node :data lst :rels nil)
	  (make-node :data (first lst) :rels (mapcar #'build-tree (rest lst))))))
    

;;--------------task2----------------

(defun print-tree (tree-main)
  (if (node-rels tree-main) 
		 (cons (node-data tree-main) (mapcar #'print-tree (node-rels tree-main)))
		 (node-data tree-main)))
	       
	     
(defun test-build-print ()
  (check-func "build-tree & print-tree" (print-tree (build-tree '(+ 1 2 (* 3 4 5) (- 10 2)))) '(+ 1 2 (* 3 4 5) (- 10 2)))
  (check-func "build-tree & print-tree" (print-tree (build-tree '(* 0 1000))) '(* 0 1000))
  (check-func "build-tree & print-tree" (print-tree (build-tree '(+ 1 2 (* 3 4 5) (- 3 2) (2) (- 10 2)))) '(+ 1 2 (* 3 4 5) (- 3 2) 2 (- 10 2))))




;;--------------task3----------------

(defun pre-order (tree-main &key (key #'identity) (mod #'identity))
  (funcall key (node-data tree-main))
  (funcall mod tree-main)
    (mapc (lambda (node) (pre-order node :key key :mod mod)) (node-rels tree-main)))


;;--------------task4----------------

(defun post-order (tree-main &key (key #'identity) (mod #'identity))
    (mapc (lambda (node) (post-order node :key key :mod mod)) (node-rels tree-main))
  (funcall key (node-data tree-main))
  (funcall mod tree-main))


;;--------------task5----------------

(defun breadth-first (tree-main &key (key #'identity) (mod #'identity))
  (labels ((%breadth-first (lst-nodes)
	     (when lst-nodes
	       (mapc (lambda (node) (funcall key (node-data node))) lst-nodes)
	       (mapc (lambda (node) (funcall mod node)) lst-nodes)
	       (%breadth-first
		(reduce (lambda (node acc) (append (node-rels node) acc))
		        lst-nodes
			:initial-value nil
			:from-end t)))))
		
    (%breadth-first (list tree-main))))


(defun test-order ()
  (format t "EXAMPLE : ~A~%"
	  (print-tree (build-tree '(+ 1 2 (* 3 (* 4 5)) (- 10 (+ 2 0))))))
  (format t "~%-------pre-order--------")
  (pre-order (build-tree '(+ 1 2 (* 3 (* 4 5)) (- 10 (+ 2 0)))) :key #'print)
  (format t "~%-------post-order-------")
  (post-order (build-tree '(+ 1 2 (* 3 (* 4 5)) (- 10 (+ 2 0)))) :key #'print)
  (format t "~%------breadth-first-----")
  (breadth-first (build-tree '(+ 1 2 (* 3 (* 4 5)) (- 10 (+ 2 0)))) :key #'print))


;;--------------task6----------------

(defun map-tree (func tree-main key)
  (let ((tree (build-tree (print-tree tree-main))))
    (funcall func
	     tree
	     :mod (lambda (node)
		    (setf (node-data node) (funcall key  (node-data node)))))
   tree))




(defun test-map-tree ()
  (check-func "map-tree (pre-order)" (print-tree (map-tree #'pre-order (build-tree '(+ 1 2 (* 3 4 5) (- 10 2))) #'identity))
	      '(+ 1 2 (* 3 4 5) (- 10 2)))
  (check-func "map-tree (post-order)" (print-tree (map-tree #'post-order  (build-tree '(+ 1 2 (* 3 4 5) (- 10 2))) #'identity))
	      '(+ 1 2 (* 3 4 5) (- 10 2)))
  (check-func "map-tree (breadth-first)" (print-tree (map-tree #'breadth-first  (build-tree '(+ 1 2 (* 3 4 5) (- 10 2))) #'identity))
	      '(+ 1 2 (* 3 4 5) (- 10 2))))


;;--------------task7----------------

(defun evaluate-expression (tree-main)
  (labels ((%evaluate (node)	    
	     (if (node-rels node)
		 (apply (node-data node) (mapcar #'%evaluate (node-rels node)))
		 (node-data node)))) 
      (%evaluate tree-main)))

(defun test-evaluate ()
  (check-func "evaluate-expression" (evaluate-expression (build-tree '(+ 1 2 (* 3 4 5) (- 10 2)))) 71)
  (check-func "evaluate-expression" (evaluate-expression (build-tree '(+ 1 2))) 3)
  (check-func "evaluate-expression" (evaluate-expression (build-tree '(+ 1 2 (- 10 2) (* 3 4 5) (* 7 3) (- 10 2)))) 100))


;;------all-tests-------

(defun tests ()
  (test-build-print)
  (test-map-tree)
  (test-order)
  (test-evaluate))


  
