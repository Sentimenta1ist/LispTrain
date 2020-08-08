;;-----------------task-1----------------------

(defun merge-strings (&rest strs)
  (labels ((%merge-all (lsts-of-str)
	     (when lsts-of-str
	       (reduce #'cons
		       lsts-of-str
		       :key #'first
		       :initial-value (%merge-all (%merge-one lsts-of-str #'rest))
		       :from-end t)))
	   (%merge-one (lst func)
	     (reduce (lambda (x acc) (if x (cons x acc) acc))
		     lst
		     :key func
		     :initial-value nil
		     :from-end t)))
    (coerce (%merge-all (%merge-one strs (lambda (x) (coerce x 'list)))) 'string)))

(defun test-merge ()
  ( check-function "merge-strings" (merge-strings "abc" "def" "hig") "adhbeicfg")
  ( check-function "merge-strings" (merge-strings "1" "2") "12")
  ( check-function "merge-strings" (merge-strings "abc111" "def" "hig222") "adhbeicfg121212")
  ( check-function "merge-strings" (merge-strings "a" "b" "c" "d") "abcd"))



;;-----------------task-2----------------------

(defun find-greatest-common-substring (str1 str2)      
  (let ((len1 (length str1))
	(len2 (length str2)))
    (labels ((%check (start end)
	       (let ((last-this (+ start end)))
		 (if (search str1 str2 :start1 start :end1 last-this)
		     (subseq str1 start last-this)
		     (if (< last-this len1)
			 (%check (1+ start) end)
			 (%check 0 (1- end)))))))
      (%check 0 (min len1 len2)))))


(defun test-find-greatest ()
  ( check-function "find-greatest-subs" (find-greatest-common-substring "abcabc" "abcabc") "abcabc")
  ( check-function "find-greatest-subs" (find-greatest-common-substring "123abc" "fffabc") "abc")
  ( check-function "find-greatest-subs" (find-greatest-common-substring "abcab" "12345") "")
  ( check-function "find-greatest-subs" (find-greatest-common-substring "" "") "")
  ( check-function "find-greatest-subs" (find-greatest-common-substring "bcdeafghk" "12345a67890") "a"))


;;-----------------task-3----------------------

(defun generate-triangle (file &key (depth 3) (max-n 5))
  "Generate triangle of numbers and dump as CSV 'file'"
  (with-open-file (out file :direction :output :if-exists :supersede)
    (dotimes (level depth)
      (let (numbers)
	(dotimes (_ (1+ level))
	  (push (1+ (random max-n)) numbers))
	(format out "~{~a~^,~}~%" numbers)))))

(defun split-str-to-list (str &aux (delimiterp (lambda (c)(or (char= c #\Space) (char= c #\,)))))
  (loop :for beg = (position-if-not delimiterp str)
	  :then (position-if-not delimiterp str :start (1+ end))	
	:for end = (and beg (position-if delimiterp str :start beg))
	:when beg :collect (parse-integer (subseq str beg end))	  
	  :while end))

(defun create-list (name-file)
  (let ((res-lst '()))
    (with-open-file (s name-file)
      (do ((str (read-line s) (read-line s nil 'eof)))
	  ((eq str 'eof))
	(push (split-str-to-list str) res-lst)))      
    (reverse res-lst)))

(defun shortest-path2 (lst-main)
  (labels ((%shortest (lst pos)
	     (if lst
		 (let ((this-element (nth pos (first lst))))
		   (multiple-value-bind (left-lst left-sum) (%shortest (rest lst) pos)
		     (multiple-value-bind (right-lst right-sum) (%shortest (rest lst) (1+ pos))
		       (if (< left-sum right-sum)
			   (values (cons this-element left-lst) (+ this-element left-sum))
			   (values (cons this-element right-lst) (+ this-element right-sum))))))
		   (values (first lst) 0))))	
    (first (multiple-value-list (%shortest lst-main 0)))))


(defun shortest-path (lst-main)
  (labels ((%shortest (lst pos)
	     (if lst
		 (let* ((left (%shortest (rest lst) pos))
			(right (%shortest (rest lst) (1+ pos)))
			(left-sum (cdr left))
			(right-sum (cdr right))
			(left-lst (car left))
			(right-lst (car right))
			(this-element (nth pos (first lst))))
		   (if (< left-sum right-sum)
		       (cons (cons this-element left-lst) (+ left-sum this-element))
		       (cons (cons this-element right-lst) (+ right-sum this-element))))
		 (cons (first lst) 0))))
     (car(%shortest lst-main 0))))


(defun test-shortest ()
 ( check-function "shortest-test" (shortest-path '((9) (9 5) (3 4 9) (4 4 6 4) (5 2 7 2 8)) ) '(9 5 4 4 2))
 ( check-function "shortest-test" (shortest-path '((1) (1 2))) '(1 1))
 ( check-function "shortest-test" (shortest-path '((5) (3 4) (3 5 3)) ) '(5 3 3))
 ( check-function "shortest-test" (shortest-path '((1))) '(1))
 ( check-function "shortest-test" (shortest-path2 '((9) (9 5) (3 4 9) (4 4 6 4) (5 2 7 2 8)) ) '(9 5 4 4 2))
 ( check-function "shortest-test" (shortest-path2 '((1) (1 2))) '(1 1))
 ( check-function "shortest-test" (shortest-path2 '((5) (3 4) (3 5 3)) ) '(5 3 3))
 ( check-function "shortest-test" (shortest-path2 '((1))) '(1)))




;;-----------------task-4----------------------

(defun greatest-palindrome (str-main)
  (let ((rev-str (reverse str-main))
	(len (length str-main)))
    (labels ((%search (start end)
	       (let* ((last-this (+ start end)))
		 (if (search str-main rev-str :start1 start :end1 last-this)
		     (subseq str-main start last-this)
		     (if (< last-this len)
			 (%search (1+ start) end)
			 (%search 0 (1- end)))))))
      (%search 0 len))))

(defun test-palindrome ()
  ( check-function "greatest-palindrome" (greatest-palindrome "123454321") "123454321")
  ( check-function "greatest-palindrome" (greatest-palindrome "123322") "2332")
  ( check-function "greatest-palindrome" (greatest-palindrome "") "")
  ( check-function "greatest-palindrome" (greatest-palindrome "12345") "1")
  ( check-function "greatest-palindrome" (greatest-palindrome "123454321") "123454321"))
  
  

;;------------tests-----------------

(defun check-function (name function expected)
  (let ((result function))
    (unless (equal result expected)
      (format t "Result of '~A' doesn't match expected result!~%
                   Actual: ~A ~% Expected: ~A ~% ~%~%"
	      name result expected))))

(defun tests()
  (test-find-greatest)
  (test-palindrome)
  (test-shortest)
  (test-merge))
