(defstruct person
  id
  name
  surname
  age)

(defun create-person (id name surname age)
  (make-person :id (parse-integer id)
               :name name
               :surname surname
               :age (parse-integer age)))


(defstruct bus
  id
  auto
  color
  year)

(defun create-bus (id auto color year)
  (make-bus  :id (parse-integer id)
             :auto auto
             :color color
             :year (parse-integer year)))


(defstruct table3
  id
  id-per
  id-bus)

(defun create-table3 (id id-per id-bus)
  (make-table3  :id (parse-integer id)
		:id-per (parse-integer id-per)
		:id-bus (parse-integer id-bus)))




(defun split-str-to-list (str &aux (delimiterp (lambda (c)(or (char= c #\Space) (char= c #\,)))))
  (loop :for beg = (position-if-not delimiterp str)
	  :then (position-if-not delimiterp str :start (1+ end))
	
	:for end = (and beg (position-if delimiterp str :start beg))
	:when beg :collect (subseq str beg end)
	  
	  :while end))

(defun create-list (constr name-file)
  (let ((res-lst '()))
    (with-open-file (s name-file)
      (do ((str (read-line s) (read-line s nil 'eof)))
	  ((eq str 'eof))
	(push  (apply constr (split-str-to-list str)) res-lst)))      
    res-lst))

(defun select (lst-id lst-per lst-bus)
      (mapcar (lambda (x)
		(element-of-table3 (table3-id-per x) (table3-id-bus x) lst-per lst-bus))
	      lst-id))
   


(defun element-of-table3 (id-per id-bus lst-per lst-bus)
  (labels ((%find-person (id lst)
	     (when lst
               (if (eql (person-id (first lst)) id)
		   (first lst)
		   (%find-person id (rest lst)))))
	   (%find-car (id lst)
	     (when lst
               (if (eql (bus-id (first lst)) id)
		   (first lst)
		   (%find-car id (rest lst))))))   
    (cons (%find-person id-per lst-per) (%find-car id-bus lst-bus))))


;;--------implementation-with-hash-tables-----------------

(defun print-hash (key value)
  (format t "Key ~S; Value ~S; ~%" key value))


(defun create-hash (constr name-file)
  (let ((my-hash (make-hash-table)))
    (with-open-file (s name-file)
      (do ((str (read-line s) (read-line s nil 'eof)))
	  ((eq str 'eof))
	(let ((this-lst (split-str-to-list str)))
          (setf (gethash (parse-integer (first this-lst)) my-hash) (apply constr this-lst)))))
    (maphash #'print-hash my-hash)
    my-hash))

(defun 2-element-of-table3 (id-per id-bus hash-person hash-bus)
    (cons (gethash id-per hash-person) (gethash id-bus hash-bus)))

(defun 2-select (lst-id hash-person hash-bus)
    (mapcar (lambda (x)
		(2-element-of-table3 (table3-id-per x) (table3-id-bus x) hash-person hash-bus))
	      lst-id))
	
;;-----------------------------------------------------------    
    




(defun make-select (f1 f2 f3)
  (let* ((hash-person (create-hash #'create-person f1))
	 (hash-bus (create-hash #'create-bus f2))
	 (lst-id (create-list #'create-table3 f3))
	 (lst-table3 (2-select lst-id hash-person hash-bus)))
(print lst-table3)
    (lambda (res-file &key name surname age auto color year)
      (with-open-file (stream res-file
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	(labels ((%check-and-write (lst)
		   (loop for x in lst
			 do (let ((this-person (car x))
				  (this-bus (cdr x)))
			      (when
				  (and
				   (if name (equal (person-name this-person) name) t)
				   (if surname (equal (person-surname this-person) surname) t)
				   (if age (eql (person-age this-person) age) t)
				   (if auto (equal (bus-auto this-bus) auto) t)
				   (if color (equal (bus-color this-bus) color) t)
				   (if year (eql (bus-year this-bus) year) t))
				(format stream "~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A~%"
					(person-id this-person)
					(person-name this-person)
					(person-surname this-person)
					(person-age this-person)
					(bus-id this-bus)
					(bus-auto this-bus)
					(bus-color this-bus)
					(bus-year this-bus)))))))
	  (%check-and-write lst-table3))))))

      
	   
  

(defun tests()
  (funcall (MAKE-SELECT "table1.txt" "table2.txt" "table3.txt") "res1.txt"  :surname "Simora" :age 20 )
  (funcall (MAKE-SELECT "table1.txt" "table2.txt" "table3.txt") "res2.txt"  :name "Dmytro" )
  (funcall (MAKE-SELECT "table1.txt" "table2.txt" "table3.txt") "res3.txt" :year 1980 :auto "Soul" ))
