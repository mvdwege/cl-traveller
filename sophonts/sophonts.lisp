;;;; Sophont generation and management.

;;; We could just implement it using a sophont base class and using
;;; the defsophont macro to define new subclasses. However, the MOP
;;; gives us the tools to store sophont racial data in the class
;;; definition out of sight of the instances, and to add behaviour to
;;; that data. So see this as an experiment in using AMOP.
(in-package :traveller)

;;; Sophont class metaobject
(defclass sophont-class (standard-class) 
  ((characteristics :initarg :characteristics
		    :reader characteristics)
   (characteristic-dice :initform (make-list 6 :initial-element 2)
			:initarg :characteristic-dice)
   (homeworld :initarg :homeworld 
	      :reader homeworld)
   (native-terrain :initarg :native-terrain
		   :reader native-terrain
		   :reader native-terrain-mod)
   (locomotion :initarg :locomotion
	       :reader locomotion)
   (ecological-niche :initarg :ecological-niche
		     :reader ecological-niche)))

;; Keep the compiler happy. Especially SBCL insists on this method
;; existing. We're not doing really deep MOP things, so a simple "Yes,
;; this is supposed to be a subclass of standard-class" will suffice.
(defmethod closer-mop:validate-superclass ((class sophont-class) (super standard-class)) 
  t)

;;; Homeworld generation
(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'homeworld)))
  (setf (slot-value sophont slot) (make-world)))

;;; Determine Native Terrain, Locomotion, and Ecological Niche.
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

(defvar *basic-niche*
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
  (setf (slot-value sophont slot) 
	(min 5 (max -5 (+ (flux) (terrain-modifiers sophont))))))


(defmethod native-terrain :around ((sophont-class sophont-class)) 
  (string-capitalize
   (substitute #\Space #\-
	       (symbol-name
		(nth (+ (call-next-method) 5) *terrain-types*)))))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'locomotion)))
  (setf (slot-value sophont slot) 
	(nth (min 10 (max 0 (+ (flux) 5 (native-terrain-mod sophont)))) 
	     (nth (random 6) *locomotion-types*))))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'ecological-niche)))
  (let ((basic-niche (nth (+ (flux) 6) *basic-niche*)))
    (setf (slot-value sophont slot) 
	  (list basic-niche 
		(nth (+ 
		      (flux) 6 
		      (native-terrain-mod sophont)) 
		     (getf *ecological-niche* basic-niche))))))

;;; Generate Characteristics
(defvar *sophont-characteristics*
  `(,(loop repeat 11 collect 'strength)
     (agility agility agility agility dexterity dexterity dexterity
     grace grace grace grace)
     (stamina stamina stamina stamina endurance endurance endurance vigor vigor vigor vigor)
     ,(loop repeat 11 collect 'intelligence)
     (instinct instinct instinct instinct education education
     education training training training training)
     (caste caste caste social-standing social-standing
     social-standing social-standing charisma charisma charisma
     charisma)))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'characteristics)))
  (let ((dm (cond
	      ((eql (locomotion sophont) 'flyer) -2)
	      ((eql (locomotion sophont) 'swimmer) 2)
	      ((eql (locomotion sophont) 'diver) 2)
	      (t 0)))
	(characteristics))
    (dotimes (c 6) (push 
		    (nth (min 10 (max 0 (+ (flux) 5 dm)))
			 (nth c *sophont-characteristics*)) characteristics))
    (setf (slot-value sophont slot) (nreverse characteristics))))
    
(defmethod genetic-profile ((sophont sophont-class))
  (let ((clist (characteristics sophont))
	(profile-list))
    (dolist (c clist)
      (push (subseq (symbol-name c) 0 1) profile-list))
    ;; The last characteristic has two posibilities starting with a
    ;; 'C'. If it is Caste, then the genetic profile letter (currently
    ;; the first element of profile-liest) must be 'K' to distinguish
    ;; it from Charisma.
    (if (eql (car (reverse clist)) 'caste)
	(setf (car profile-list) "K"))
    (format nil "~{~a~}" (nreverse profile-list))))

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


