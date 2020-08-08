;;------------------merge-strings----------------------------

(defun merge-strings (str1 str2)
    (labels ((%merge (lst1 lst2)
	       (if (not lst1)
		   lst2
		   (cons (first lst1) (%merge lst2 (rest lst1))))))
      (coerce (%merge (coerce str1 'list) (coerce str2 'list)) 'string)))

(defun test-merge-strings ()
  (check-func "merge-strings" (merge-strings "abcde" "12345") "a1b2c3d4e5")
  (check-func "merge-strings" (merge-strings "abc" "12345") "a1b2c345")
  (check-func "merge-strings" (merge-strings "abcde" "123") "a1b2c3de")
  (check-func "merge-strings" (merge-strings "" "12345") "12345")
  (check-func "merge-strings" (merge-strings "" "") ""))



;;-------------------archieve-string-------------------------

(defun archieve-string (str)
  (labels ((%counter (lst &optional (count 1))
  (when lst
    (if (eql (first lst) (second lst))
	(%counter (rest lst) (1+ count))
	(cons  (first lst)
	       (if (= 1 count)
		   (%counter (rest lst))
		   (append (coerce (write-to-string count) 'list) (%counter (rest lst)))))))))
    (coerce (%counter (coerce str 'list)) 'string)))

(defun test-archieve-string ()
  (check-func "archieve-string" (archieve-string "abcde") "abcde")
  (check-func "archieve-string" (archieve-string "aaaabccccdeeee") "a4bc4de4")
  (check-func "archieve-string" (archieve-string "aaaaaaaaaaaa") "a12")
  (check-func "archieve-string" (archieve-string "") ""))
  






;;--------------------tests---------------------------------


(defun check-func (name function expected)
  (let ((result function))
    (unless (equal result expected)
      (format t "Result of '~A' doesn't match expected result!~%
                   Actual: ~A ~% Expected: ~A ~% ~%~%"
	      name result expected))))

(defun tests ()
  (test-merge-strings)
  (test-archieve-string)
  )
