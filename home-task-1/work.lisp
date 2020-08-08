(defun sum (lst)
  (if lst
      (+ (first lst) (sum (rest lst)))
      0))

(defun inc-list (lst)
  (when lst
    (cons (+ 1 (first lst)) (inc-list (rest lst)))))

(defun create-reverse-tail (lst tail)
  (if lst
      (create-reverse-tail (rest lst) (cons (first lst) tail))
      tail))

(defun my-reverse (lst)
  (create-reverse-tail lst nil))
      

(defun my-delete-repeats (lst)
  (when lst
      (if (position (first lst) (rest lst))
	  (my-delete-repeats (rest lst)) 
	  (cons (first lst) (my-delete-repeats (rest lst))))))

(defun check-inc-list (input expected)
  "Utility function for 'inc-list' result check"
  (let ((result (inc-list input)))
    (unless (equal result expected)
      (format t "Result of inc-list doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
              result
              expected))))

(defun check-sum (input expected)
  "Utility function for 'sum' result check"
  (let ((result (sum input)))
    (unless (equal result expected)
      (format t "Result of sum doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
              result
              expected))))

(defun check-my-reverse (input expected)
  "Utility function for 'my-revrese' result check"
  (let ((result (my-reverse input)))
    (unless (equal result expected)
      (format t "Result of my-reverse doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
              result
              expected))))

(defun check-my-delete-repeats (input expected)
  "Utility function for 'my-delete-repeats' result check"
  (let ((result (my-delete-repeats input)))
    (unless (equal result expected)
      (format t "Result of my-delete-repeats doesn't match expected result!~%Actual: ~A~%Expected: ~A~%"
              result
              expected))))

(defun test-inc-list ()
  (check-inc-list () ())
  (check-inc-list '(1) '(2))
  (check-inc-list '(1 10 -1) '(2 11 0))
  (check-inc-list '(1 1 1 1) '(2 2 2 2)))

(defun test-sum ()
  (check-sum '() 0)
  (check-sum '(1) 1)
  (check-sum '(1 10 -1) 10)
  (check-sum '(1 1 1 1) 4))

(defun test-my-reverse ()
  (check-my-reverse () ())
  (check-my-reverse '(1) '(1))
  (check-my-reverse '(1 10 -1) '(-1 10 1))
  (check-my-reverse '(1 2 3 40) '(40 3 2 1))
  (check-my-reverse '(1 2 3 nil nil) '(nil nil 3 2 1)))

(defun test-my-delete-repeats ()
  (check-my-delete-repeats () ())
  (check-my-delete-repeats '(1) '(1))
  (check-my-delete-repeats '(1 10 -1) '(1 10 -1))
  (check-my-delete-repeats '(1 1 1 1 2 2 2 nil nil  3) '(1 2 nil 3)))

(defun test-all ()
  (test-inc-list)
  (test-sum)
  (test-my-reverse)
  (test-my-delete-repeats))
