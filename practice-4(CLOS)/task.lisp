;;-----------------------ABSTRACT-------------------------
(defclass state-machine ()
  ((available-states
    :accessor available-states-gs
    :initarg :available-states
    :documentation "list of available-states of this state-machine")
   (hash-available-state
    :accessor hash-available-states-gs
    :initarg :hash-available-states
    :documentation "Hash-table key:symbol value:instance")
   (this-machine-state
    :accessor this-machine-state-gs
    :initarg :this-machine-state
    :documentation "Current state of the machine")
   (logging-actions
    :accessor logging
    :initarg :loggging
    :documentation "Flag for logging"))
  (:documentation
   "Class represents abstarct state-mashine. It contains slots for all state-mashines"))

(defclass sm-signal () ()
  (:documentation "Class represents abstract signal for all state-mashines"))


(defclass error-signal (sm-signal) ()
  (:documentation "Class represents error-signal for test"))


(defclass sm-state () ()
  (:documentation "Class represents abstract state for all state-mashines"))


(defmethod initialize-instance :after ((mach state-machine) &key &allow-other-keys)
  "Initializing hash-table for state-machine using slot 'available-states"
  (let ((hash-table-machine (make-hash-table))
	(lst-available-states (available-states-gs mach)))
    (mapc (lambda (element-of-available-states)
	    (setf (gethash (type-of element-of-available-states)
			   hash-table-machine)
		  element-of-available-states))
	  lst-available-states)
    (setf (hash-available-states-gs mach) hash-table-machine)))
	
    
(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))


(defgeneric handle-signal (mach st sig)
  (:documentation   "Method for processing signals based only on the type of transmitted elements")
  (:method (mach st sig ) (format t "~A IS UNSUPPORTED SIGNAL !!! :0~%" sig)))
    


;;---------nexstate--------

(defmethod nextstate :before ((mach state-machine) sm-state)
  (when (logging mach)
    (format t "This state-machine in '~A' state~%" (this-machine-state-gs mach))))

(defmethod nextstate ((mach state-machine) sm-state)
  "Method for going to the next state"
  (let ((new-state (gethash sm-state (hash-available-states-gs mach)))
	(old-state (this-machine-state-gs mach)))
    (cond
      ((not new-state) (format t "Unsupported state!!! :0~%"))
      ((eq sm-state old-state) (format t "This state now!!!~%"))
      (t (format t "State '~A' sets to state-machine, good choise :)~%" sm-state)
	 (setf (this-machine-state-gs mach) sm-state)))))

(defmethod nextstate :after ((mach state-machine) sm-state)
  (when (logging mach)
      (format t "NOW this state-machine in '~A' state (oh, it's work!)~%" (this-machine-state-gs mach))))
    


;;-----------send------------

(defmethod send :before ((mach state-machine) (sig sm-signal))
  (when (logging mach)
     (format t "~%Signal '~A' being processed~%" (type-of sig))))


(defmethod send ((mach state-machine) (sig sm-signal))
  "Method for sending a signal to transition to a new state"
  (handle-signal mach
		 (gethash (this-machine-state-gs mach) (hash-available-states-gs mach))
		 sig))

(defmethod send :after ((mach state-machine) (sig sm-signal))
  (when (logging mach) 
     (format t "The signal '~A' has been sent.~%~%" (type-of sig))))







;;------------------------------CONDITIONER-------------------------------

(defclass conditioner (state-machine)
  ((available-states
    :initform `(,(make-instance 'off-state-c)
		,(make-instance 'cool-state-c)
		,(make-instance 'heat-state-c)))	       
   (this-machine-state
    :initform 'off-state-c)
   (logging-actions
    :initform nil))
   (:documentation
    "Class represents a conditioner.
     States 'off 'cool 'heat can be called whatever you want"))


;;------------signals-of-conditioner--------------

(defclass off-signal-c (sm-signal) ())

(defclass cool-signal-c (sm-signal) ())

(defclass heat-signal-c (sm-signal) ())


;;-------------states-of-conditioner---------------

;;--off-state-conditioner--
  
(defclass off-state-c (sm-state) ())

(defmethod handle-signal ((mach conditioner) (st off-state-c) (sig cool-signal-c))
  (nextstate mach 'cool-state-c))

(defmethod handle-signal ((mach conditioner) (st off-state-c) (sig heat-signal-c))
  (nextstate mach 'heat-state-c))

;;--cool-state-conditioner--

(defclass cool-state-c (sm-state) ())

(defmethod handle-signal ((mach conditioner) (st cool-state-c) (sig off-signal-c))
  (nextstate mach 'off-state-c))

(defmethod handle-signal ((mach conditioner) (st cool-state-c) (sig heat-signal-c))
  (nextstate mach 'heat-state-c))

;;--heat-state-conditioner--

(defclass heat-state-c (sm-state) ())

(defmethod handle-signal ((mach conditioner) (st heat-state-c) (sig cool-signal-c))
  (nextstate mach 'cool-state-c))

(defmethod handle-signal ((mach conditioner) (st heat-state-c) (sig off-signal-c))
  (nextstate mach 'off-state-c))


;;-------------test-conditioner--------------

(defun test-conditioner ()
  "Function for test conditioner"
  (let ((conditioner (make-instance 'conditioner))
	(off-signal-conditioner (make-instance 'off-signal-c))
	(cool-signal-conditioner (make-instance 'cool-signal-c))
	(heat-signal-conditioner (make-instance 'heat-signal-c))
	(error-signal-conditioner (make-instance 'error-signal)))
    (format t "~%--------------------CONDITIONER------------------------~%~%")
    (send conditioner cool-signal-conditioner)
    (send conditioner heat-signal-conditioner)
    (send conditioner error-signal-conditioner)
    (send conditioner off-signal-conditioner)
    (send conditioner heat-signal-conditioner)
    (send conditioner heat-signal-conditioner)
    (send conditioner cool-signal-conditioner)
    (format t "~%--------------------------------------------------------~%~%")))




;;------------------------------HAIR-DRIER--------------------------------

(defclass hair-drier (state-machine)
  ((available-states
    :initform `(,(make-instance 'off-state-hd)
		,(make-instance 'normal-state-hd)
		,(make-instance 'turbo-state-hd)))	       
   (this-machine-state
    :initform 'off-state-hd)
      (logging-actions
       :initform nil))
     (:documentation
    "Class represents a hair-drier.
     States 'off - 'normal - 'turbo can be called only in this order"))



;;-----------signals-of-hair-drier--------------

(defclass off-signal-hd (sm-signal) ())

(defclass normal-signal-hd (sm-signal) ())

(defclass turbo-signal-hd (sm-signal) ())


;;------------states-of-hair-drier--------------

;;--off-state-hd---
  
(defclass off-state-hd (sm-state) ())

(defmethod handle-signal ((mach hair-drier) (st off-state-hd) (sig normal-signal-hd))
  (nextstate mach 'normal-state-hd))

;;--cool-state-hd---

(defclass normal-state-hd (sm-state) ())

(defmethod handle-signal ((mach hair-drier) (st normal-state-hd) (sig off-signal-hd))
  (nextstate mach 'off-state-hd))

(defmethod handle-signal ((mach hair-drier) (st normal-state-hd) (sig turbo-signal-hd))
  (nextstate mach 'turbo-state-hd))

;;--heat-state-hd---

(defclass turbo-state-hd (sm-state) ())

(defmethod handle-signal ((mach hair-drier) (st turbo-state-hd) (sig normal-signal-hd))
  (nextstate mach 'normal-state-hd))



;;--------------test-hair-drier--------------

(defun test-hair-drier ()
  "Function for test hair-drier"
  (let ((hair-drier (make-instance 'hair-drier))
	(off-signal-hd (make-instance 'off-signal-hd))
	(normal-signal-hd (make-instance 'normal-signal-hd))
	(turbo-signal-hd (make-instance 'turbo-signal-hd))
	(error-signal (make-instance 'error-signal)))
    (format t "~%--------------------HAIR-DRIER--------------------------~%~%")
    (send hair-drier normal-signal-hd)
    (send hair-drier turbo-signal-hd)
    (send hair-drier normal-signal-hd)
    (send hair-drier off-signal-hd)
    (send hair-drier turbo-signal-hd)
    (send hair-drier normal-signal-hd)
    (send hair-drier error-signal)
    (format t "~%--------------------------------------------------------~%~%")))







;;---------------------------------COFFEE-------------------------------------



(defclass coffee (state-machine)
  ((available-states
    :initform `(,(make-instance 'off-state-cof)
		,(make-instance 'start-state-cof)
		,(make-instance 'menu-state)
		,(make-instance 'payment-state)
		,(make-instance 'choose-type-state)
		,(make-instance 'coffee-making)))
   (this-machine-state
    :initform 'off-state-cof)
   (coffee-menu
    :accessor coffee-menu-gs
    :initform`(,(cons 'latte 50)
	       ,(cons 'espresso 60)
	       ,(cons 'americano 40))
    :documentation "Coffee price-list")
   (sugar-option
    :accessor sugar-option-gs
    :initform nil
    :documentation "Sugar option (sweet/not-sweet)")
   (coffee-type
    :accessor coffee-type-gs
    :initform nil
    :documentation "Current type of coffee")
   (cash
    :accessor cash-gs
    :initform 0
    :documentation "Current cash in coffee-mashine")
   (logging-actions
    :initform nil))

  (:documentation
   "Class represent  cofee-mashine
       
       (signal 'off-signal-cof)
       1 state - 'off-state-cof' - mashine is off
      
       (signal 'start-signal-cof)
       2 state - 'start-state-cof' - mashine is start screen

       (signal 'input-money)
       3 state - 'payment-state' - input money to coffee-machine 

       (signal 'menu-signal)
       4 state - 'menu-state' - all types of coffee

       (signal 'choose-type)      
       5 state - 'choose-type-state' choose type-coffee        

       (signal 'sugar-option-signal)
       6 state - coffee-making' - cofee-making

       The order of states:
       1 -> 2 -> 3 -> 4 -> 5 -> 6 -> (or 1 (if there is no client) 2 (if there is client))"))



;;-----------signals-of-coffee--------------

(defclass off-signal-cof (sm-signal) ())
    
(defclass start-signal-cof (sm-signal) ())

(defclass input-money (sm-signal)
  ((money
    :accessor money-gs
    :initarg :money
    :initform nil)))

(defclass menu-signal (sm-signal) ())

(defclass choose-type (sm-signal)
  ((certain-type
    :accessor certain-type-gs
    :initarg :type-of-coffee
    :initform nil)))
   

(defclass sugar-option-signal (sm-signal)
    ((sugar
    :accessor sugar-gs
    :initarg :certain-sugar
    :initform nil)))
   			 
				     
(defclass take-coffee (sm-signal) ())


;;------------states-of-coffee--------------


;;--off-state-cof---
  
(defclass off-state-cof (sm-state) ())

(defmethod handle-signal ((mach coffee) (st off-state-cof) (sig start-signal-cof))
  (nextstate mach 'start-state-cof))

;;--on-state-cof---

(defclass start-state-cof (sm-state) ())

(defmethod handle-signal ((mach coffee) (st start-state-cof) (sig input-money))
  (format t "You input ~A dollars~%" (money-gs sig))
  (incf (cash-gs mach) (money-gs sig))
  (format t "Now coffee-machine have ~A dollars ~%" (cash-gs mach))
  (nextstate mach 'payment-state))

;;--payment-state----

(defclass payment-state (sm-state) ())

(defmethod handle-signal ((mach coffee) (st payment-state) (sig menu-signal))
  (format t "Our PRICE-LIST: ~A~%" (coffee-menu-gs mach))
  (nextstate mach 'menu-state))

;;--menu-state--

(defclass menu-state (sm-state) ())

(defmethod handle-signal ((mach coffee) (st menu-state) (sig choose-type))
  (let* ((type (certain-type-gs sig))
	 (element-of-menu (find-if (lambda (x) (eq (first x) type)) (coffee-menu-gs mach))))
	(if element-of-menu
	    (if (< (cash-gs mach) (rest element-of-menu))
		(progn
		  (format t "Not enaugh money for type ~A Input more.~%" (first element-of-menu))
		  (nextstate mach 'start-state-cof))
	      (progn
		(setf (coffee-type-gs mach) (first element-of-menu))
		(decf (cash-gs mach) (rest element-of-menu))
		(format t "You choose ~A~%" (first element-of-menu))
		(nextstate mach 'choose-type-state)))
	  (format t "Error type of coffee~%"))))

;;--choose-type-state---

(defclass choose-type-state (sm-state) ())

(defmethod handle-signal ((mach coffee) (st choose-type-state) (sig sugar-option-signal))
  (setf (sugar-option-gs mach) (sugar-gs sig))
  (nextstate mach 'coffee-making))

;;-coffee-making-state-

(defclass coffee-making (sm-state) ())

(defmethod handle-signal ((mach coffee) (st coffee-making) (sig off-signal-cof))
  (format t "Your coffee - type: ~A, sugar: ~A ready! Take!~%" (coffee-type-gs mach) (sugar-option-gs mach))
  (nextstate mach 'off-state-cof))

(defmethod handle-signal ((mach coffee) (st coffee-making) (sig start-signal-cof))
  (format t "Your coffee - type: ~A, sugar: ~A ready! Take!~%" (coffee-type-gs mach) (sugar-option-gs mach))
  (nextstate mach 'start-state-cof))



;;------------------tests-coffee---------------

(defun test-coffee ()
  "Function for test coffee-mashine"
  (let ((coffee (make-instance 'coffee))
        (off (make-instance 'off-signal-cof))
	(start (make-instance 'start-signal-cof))
	(money (make-instance 'input-money :money 40))
	(money2 (make-instance 'input-money :money 20))
	(menu (make-instance 'menu-signal))
	(type (make-instance 'choose-type :type-of-coffee 'latte))
	(type2 (make-instance 'choose-type :type-of-coffee 'americano))
	(sugar-option (make-instance 'sugar-option-signal :certain-sugar 'sweet)))
    (format t "~%----------------------COFFEE---------------------------~%~%")
    (send coffee start)
    (send coffee money)
    (send coffee menu)
    (send coffee type)
    (send coffee money2)
    (send coffee menu)
    (send coffee type)
    (send coffee sugar-option)
    (send coffee start)
    (send coffee money)
    (send coffee menu)
    (send coffee type2)
    (send coffee sugar-option)
    (send coffee off)
    (format t "~%--------------------------------------------------------~%~%")))
    






;;-------------------------------ATM-mashine-----------------------------------

(defclass atm (state-machine)
  ((available-states
    :initform `(,(make-instance 'off-state-atm)
		,(make-instance 'on-state-atm)
		,(make-instance 'error-pin-state)
		,(make-instance 'account-menu-state)
		,(make-instance 'menu-state)
		,(make-instance 'take-money-state)
		,(make-instance 'balance-state)
		,(make-instance 'send-money-state)
		,(make-instance 'end-work-state)))
   (this-machine-state
    :initform 'off-state-atm)
   (logging-actions
    :initform nil))
   (:documentation
    "Class represents atm-mashine:

    (signal 'off-atm)
    1-state - 'off-state-atm' - atm is off

    (signal 'on-atm)
    2-state - 'on-state-atm'  - atm is on
   
    (signal 'insert-card)
    3-state - 'error-pin-state' - Your pin is incorrect
    4-state - 'account-menu-state'- Your account-menu
    
    (signal 'open-menu)
    5-state - 'menu-state' all possible function of atm

    (signal 'menu-option)
    6-state - 'take-money-state'
    7-state - 'balance-state'
    8-state - 'send-money-state'

    (signal 'end-work)
    9-state - 'end-work-state'

    The order of states:
    1 -> 2 -> (or 3 4) -> 5 -> (or 6 7 8) -> 9 -> (or 1 5)"))


;;-------signals-of-atm-------

(defclass off-atm (sm-signal) ())

(defclass on-atm (sm-signal) ())

(defclass insert-card (sm-signal)
  ((pin
   :accessor pin-gs
   :initarg :pin
   :initform nil)))

(defclass open-menu (sm-signal) ())

(defclass menu-option (sm-signal)
  ((option
    :accessor option-gs
    :initarg :option
    :initform nil)))

(defclass end-work (sm-signal) ())


;;------------states-of-atm--------------

;;--off-state-atm--

(defclass off-state-atm (sm-state) ())

(defmethod handle-signal ((mach atm) (st off-state-atm) (sig on-atm))
  (nextstate mach 'on-state-atm))

;;--on-state-atm--

(defclass on-state-atm (sm-state) ())

(defmethod handle-signal ((mach atm) (st on-state-atm) (sig insert-card))
  (if (eq (pin-gs sig) '1111)
      (nextstate mach 'account-menu-state)
      (nextstate mach 'error-pin-state)))

;;--error-pin-state--

(defclass error-pin-state (sm-state) ())

(defmethod handle-signal ((mach atm) (st error-pin-state) (sig off-atm))
  (nextstate mach 'off-state-atm))

;;--account-menu-state--

(defclass account-menu-state (sm-state) ())

(defmethod handle-signal ((mach atm) (st account-menu-state) (sig open-menu))
  (nextstate mach 'menu-state))

;;--menu-option--

(defclass menu-state (sm-state) ())

(defmethod handle-signal ((mach atm) (st menu-state) (sig menu-option))
  (let ((option (option-gs sig)))
    (cond
      ((eq option 'take-money) (nextstate mach 'take-money-state))
      ((eq option 'balance) (nextstate mach 'balance-state))
      ((eq option 'send-money) (nextstate mach 'send-money-state)))))

;;---take-money-state--

(defclass take-money-state (sm-state) ())

(defmethod handle-signal ((mach atm) (st take-money-state) (sig end-work))
  (nextstate mach 'end-work-state))

;;---balance-state--

(defclass balance-state (sm-state) ())

(defmethod handle-signal ((mach atm) (st balance-state) (sig end-work))
  (nextstate mach 'end-work-state))


;;---send-money-state--

(defclass send-money-state (sm-state) ())

(defmethod handle-signal ((mach atm) (st send-money-state) (sig end-work))
  (nextstate mach 'end-work-state))

;;---end-work-state--

(defclass end-work-state (sm-state) ())

(defmethod handle-signal ((mach atm) (st end-work-state) (sig off-atm))
  (nextstate mach 'off-state-atm))

(defmethod handle-signal ((mach atm) (st end-work-state) (sig open-menu))
  (nextstate mach 'menu-state))



;;-------------------tests-atm-------------------

(defun test-atm ()
  "Function for test atm-mashine"
  (let ((atm (make-instance 'atm))
        (off (make-instance 'off-atm))
	(on (make-instance 'on-atm))
	(insert-card-error (make-instance 'insert-card :pin '1112))
	(insert-card-right (make-instance 'insert-card :pin '1111))
	(open-menu (make-instance 'open-menu))
	(menu-option-balance (make-instance 'menu-option :option 'balance))
	(menu-option-take-money (make-instance 'menu-option :option 'take-money))
	(end-work (make-instance 'end-work))
	(error-signal (make-instance 'error-signal)))
    (format t "~%------------------------ATM---------------------------~%~%")
    (send atm on)
    (send atm insert-card-error)
    (send atm off)
    (send atm end-work);error input
    (send atm on)
    (send atm insert-card-right)
    (send atm open-menu)
    (send atm menu-option-balance)
    (send atm end-work)
    (send atm open-menu)
    (send atm menu-option-take-money)
    (send atm error-signal)
    (send atm end-work)
    (send atm off)
    (format t "~%--------------------------------------------------------~%~%")))
	     



;;-----------------------------ALL-TESTS------------------------------

(defun all-tests ()
  (test-conditioner)
  (test-hair-drier)
  (test-coffee)
  (test-atm))


