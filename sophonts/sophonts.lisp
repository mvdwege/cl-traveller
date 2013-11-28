;;;; Sophont generation and management.

;;; We could just implement it using a sophont base class and using
;;; the defsophont macro to define new subclasses. However, the MOP
;;; gives us the tools to store sophont racial data in the class
;;; definition out of sight of the instances, and to add behaviour to
;;; that data. So see this as an experiment in using AMOP.
(in-package :traveller)

;;; Data tables for sophont generation
(defvar *terrain-types* 
  '(mountain desert exotic rough-wood rough clear forest wetlands
  wetland-woods ocean ocean-depths))

(defvar *locomotion-types*
  '((walker walker amphib amphib amphib walker walker amphib amphib flyphib Aquatic)
    (walker walker walker walker walker walker walker aquatic walker swim diver)
    (walker walker walker walker walker walker walker walker walker swim diver)
    (walker walker walker walker walker walker walker walker walker swim diver)
    (walker walker flyphib walker walker walker walker triphib triphib aquatic diver)
    (flyer flyer flyer flyer flyer walker walker flyer flyphib diver diver)))

(defclass sophont-class (standard-class) 
  ((characteristics :initform (make-list 6)
		    :initarg :characteristics)
   (characteristic-dice :initform (make-list 6 :initial-element 2)
			:initarg :characteristic-dice)
   (homeworld :initarg :homeworld 
	      :initform (make-world)
	      :reader homeworld)
   (native-terrain :initarg :native-terrain
		   :initform (flux)
		   :reader native-terrain)))

;; Only four different conditions, so just a few ifs will do, no need
;; to break out method dispatch.
(defmethod terrain-modifiers ((sophont-class sophont-class))
  (+
   (if (>= (atmosphere (homeworld sophont-class)) 8)
       -2
       0)
   (if (<= (size (homeworld sophont-class)) 5)
       -1
       0)
   (if (>= (hydrographics (homeworld sophont-class)) 6)
       1
       0)
   (if (>= (hydrographics (homeworld sophont-class)) 9)
       1
       0)))

(defmethod native-terrain :around ((sophont-class sophont-class)) 
  (string-capitalize
   (substitute #\Space #\-
	       (symbol-name
		(nth (+ (call-next-method) 5) *terrain-types*)))))

;; Keep the compiler happy. Especially SBCL insists on this method
;; existing. We're not doing really deep MOP things, so a simple "Yes,
;; this is supposed to be a subclass of standard-class" will suffice.
(defmethod closer-mop:validate-superclass ((class sophont-class) (super standard-class)) 
  t)

;; Base class for sophont individuals
(defclass sophont () 
  ((name :accessor name
	 :initarg :name)
   (homeworld :accessor homeworld
	      :initarg :homeworld)
   (birthworld :reader birthworld)
   (age :accessor age
       :initarg :age)
   (characteristics :accessor characteristics
		    :initform (make-list 6)
		    :initarg :characteristics))
  (:metaclass sophont-class))

;; Convenience macro.
(defmacro defsophont (name) 
  `(progn
     (defclass ,name (sophont) () (:metaclass sophont-class))
     ;; ,@(mapcar #'(lambda (x y) 
     ;; 		   `(defmethod ,x ((sophont ,name)) 
     ;; 		      (,x (nth ,y (characteristics sophont))))) 
     ;; 	       '(strength 
     ;; 		 dexterity agility grace 
     ;; 		 endurance stamina vigor
     ;; 		 intelligence 
     ;; 		 education training instinct
     ;; 		 social-standing charisma caste)
     ;; 	       '(1 2 2 2 3 3 3 4 5 5 5 6 6 6))
     (find-class ',name)))    
;;; Characteristics


