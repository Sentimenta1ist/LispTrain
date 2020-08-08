(defun lexer (str)
  (labels ((%analyze (lst)
	     (when lst
	       (cond
		 ((digit-char-p (first lst))
		  (%number lst))
		 ((alpha-char-p (first lst))
		  (%id lst))
		 ((%separator (first lst))
		  (cons (%separator (first lst)) (%analyze (rest lst))))
		 ((%white-spaces (first lst))
		  (%analyze (rest lst)))
		 (t
		  (error "Undefined smb!!!")))))
	   (%separator (x)
	     (case x
	       (#\+ 'add)
	       (#\- 'sub)
	       (#\* 'mul)
	       (#\/ 'div)
	       (#\( 'o-parenth)
	       (#\) 'c-parenth)))
	   (%white-spaces (x)
	     (find x '(#\SPACE #\Newline #\Tab #\Linefeed)))
	   (%id-or-number-lst (lst lst-rest test)
	     (if lst
		 (if (funcall test (first lst))
		     (%id-or-number-lst (rest lst) (cons (first lst) lst-rest) test)
		     (list lst-rest lst))
		 (list lst-rest)))
	   (%id (lst)
	     (let* ((all-lst (%id-or-number-lst lst nil #'alphanumericp))
		    (element (reverse (first all-lst))))
	       (when element
		 (cons (list 'id (intern (coerce element 'string))) (%analyze (second all-lst))))))
	   (%number (lst)
	     (let* ((all-lst (%id-or-number-lst lst nil #'digit-char-p))
		    (next (first (car (rest all-lst))))
		    (element (reverse (first all-lst))))
	       (when element
		 (if (or (%separator next) (%white-spaces next) (eq next nil)) 
		     (cons (list 'num (parse-integer (coerce element 'string))) (%analyze (second all-lst)))
		     (error "Number ~A is followed by a letter \"~A\"!!!"
			    (coerce element 'string)
			    next))))))	 
    (%analyze (coerce str 'list))))


(defun parser (lst-main)
  (let ((lst lst-main))
    (labels ((%expression ()
	       (%expr))
	     (%expr()
	       (%expr-list (%term)))
	     (%term ()
	       (%term-list (%factor)))
	     (%factor ()
	       (let ((this (first lst)))
		 (cond
		   ((and (listp this) (or (eq (first this) 'num) (eq (first this) 'id)))
		    (pop lst))
		   ((eq this 'o-parenth)
		    (progn
		      (pop lst)
		      (let ((result-expr (%expr)))
			(if (eq (pop lst) 'c-parenth)
			  result-expr
			  (error "Expected close parenth")))))
		   (t (error "Error input")))))
	     (%term-list (result-factor)
	       (let ((this (first lst)))
		 (if (or (eq this 'mul) (eq this 'div))
		     (%term-list (list 'expr (pop lst) result-factor (%factor)))
		     result-factor)))
	     (%expr-list (result-term)
	       (let ((this (first lst)))
		 (if (or (eq this 'add) (eq this 'sub))
		     (%expr-list (list 'expr (pop lst) result-term (%term)))
		     result-term))))
      (when lst (%expression)))))

	   
(defun print-tree (parse-tree)
  (labels ((%dump (lst)
	     (when lst
	       (if (%operation-p lst)
		   (let* ((operator (second lst))
			  (operator-value (cdr (%names operator)))
			  (op1 (third lst))
			  (op2 (fourth lst)))
		     (format nil "~A ~A ~A"
			     (%parenth operator-value (%dump op1)  op1)
			     (car (%names operator))
			     (%parenth (1+ operator-value) (%dump op2) op2)))
		   lst)))
	   (%names (operator)
	     (case operator
	       (add (cons '+ 0))
	       (sub (cons '- 0))
	       (mul (cons '* 1))
	       (div (cons '/ 1))))
	   (%parenth (operator-value operand-res operand)
	     (if (not (%operation-p operand))
		 (second operand-res)
		 (let ((operator-operand (second operand)))
		   (if (>= (cdr (%names operator-operand)) operator-value)
		       operand-res
		       (format nil "(~A)" operand-res)))))
	   (%operation-p (x)
	      (eq 'expr (first x))))
	     
    (when parse-tree
      (%dump parse-tree))))



;;----------------tests------------------

(defun check-func (name function expected)
  (let ((result function))
    (unless (equal result expected)
      (format t "Result of '~A' doesn't match expected result!~%
                   Actual: ~A ~% Expected: ~A ~% ~%~%"
	      name result expected))))


(defun tests ()
  (check-func "lexer/parser/print-tree"  (print-tree (parser (lexer "((3 - 2)) - 1"))) "3 - 2 - 1")
  (check-func "lexer/parser/print-tree"  (print-tree (parser (lexer "123+1"))) "123 + 1")
  (check-func "lexer/parser/print-tree"  (print-tree (parser (lexer "2 * (4 - 5) + 1"))) "2 * (4 - 5) + 1")
  (check-func "lexer/parser/print-tree"  (print-tree (parser (lexer "2 / foo  * (4 - 5) + (1 - bar)"))) "2 / foo * (4 - 5) + (1 - bar)")
  (check-func "lexer/parser/print-tree"  (print-tree (parser (lexer "a+b-c*d/f"))) "a + b - c * d / f"))
