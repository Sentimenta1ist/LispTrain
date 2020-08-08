
;;--------------------string-to-hash-table----------------
;;--------------------hash-table-to-string----------------


(defun print-hash (key value)
  (format t "Key ~S; Value ~S; ~%" key value))

(defun string-to-hash-table (str)
  (let ((my-hash (make-hash-table)))
    (reduce (lambda (index x)
	      (push index (gethash x my-hash))
	      (1+ index))
	    str
	    :initial-value 0)
    my-hash))
     



(defun make-str (val key str)
  (loop
    for elem in val
	do (setf (char str elem) key)))
	  
(defun make-length (this-hash)
 (let ((lst-of-val '(-1)))
    (maphash (lambda (key val) 
                 (push (first val) lst-of-val))                 
             this-hash)
    (1+ (apply #'max lst-of-val))))

(defun hash-table-to-string (this-hash)
  (let ((result-string ( make-string (make-length this-hash) :initial-element #\0)))
     (maphash (lambda (key val) 
		    (make-str val key result-string))	      
             this-hash)
    result-string))	 



(defun check-func (name function expected)
  (let ((result function))
    (unless (equal result expected)
      (format t "Result of '~A' doesn't match expected result!~%
                   Actual: ~A ~% Expected: ~A ~% ~%~%"
	      name result expected))))

(defun test-lection ()
  (check-func "hash-table-to-string" (hash-table-to-string (string-to-hash-table "abcdef")) "abcdef")
  (check-func "hash-table-to-string" (hash-table-to-string (string-to-hash-table "123abc")) "123abc")
  (check-func "hash-table-to-string" (hash-table-to-string (string-to-hash-table "a")) "a")
  (check-func "hash-table-to-string" (hash-table-to-string (string-to-hash-table "")) ""))
