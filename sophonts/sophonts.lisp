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

(defvar *niche-basic*
  '(producer producer herbivore herbivore omnivore omnivore omnivore
    omnivore omnivore carnivore carnivore scavenger scavenger))

(defvar *ecological-niche*
  '(herbivore (grazer grazer grazer intermittent intermittent
	       intermittent intermittent grazer grazer grazer grazer
	       grazer filter)
    omnivore (hunter hunter hunter hunter hunter gatherer
	      hunter-gatherer gatherer gatherer gatherer gatherer
	      gatherer eater)
    carnivore (pouncer pouncer pouncer pouncer pouncer pouncer chaser
	       chaser chaser chaser trapper siren killer)
    scavenger (carrion-eater carrion-eatercarrion-eater hijacker
	       hijacker hijacker intimidator intimidator intimidator
	       intimidator intimidator reducer reducer)
    producer (collector collector collector collector collector
	      collector basker basker basker basker basker basker)))

;;; Sophont class
(defclass sophont-class (standard-class) 
  ((characteristics :initform (make-list 6)
		    :initarg :characteristics)
   (characteristic-dice :initform (make-list 6 :initial-element 2)
			:initarg :characteristic-dice)
   (homeworld :initarg :homeworld 
	      :reader homeworld)
   (native-terrain :initarg :native-terrain
		   :reader native-terrain)))

;;; Homeworld generation
(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'homeworld)))
  (setf (slot-value sophont slot) (make-world)))

;;; Determine Native Terrain, Locomotion, and Ecological Niche.

;; Only four different conditions, so just a few ifs will do, no need
;; to break out method dispatch.
(defmethod terrain-modifiers ((sophont sophont-class))
  (+
   (if (>= (atmosphere (homeworld sophont)) 8)
       -2
       0)
   (if (<= (size (homeworld sophont)) 5)
       -1
       0)
   (if (>= (hydrographics (homeworld sophont)) 6)
       1
       0)
   (if (>= (hydrographics (homeworld sophont)) 9)
       1
       0)))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'native-terrain)))
  (setf (slot-value sophont slot) (min 5 (max -5 (+ (flux) (terrain-modifiers sophont))))))

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
  `(defclass ,name (sophont) () (:metaclass sophont-class)))

;;; Characteristics


