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
		    :initarg :genetic-profile
		    :reader characteristics)
   (characteristic-dice :initarg :characteristic-dice
			:reader characteristic-dice)
   (homeworld :initarg :homeworld 
	      :reader homeworld)
   (native-terrain :initarg :native-terrain
		   :reader native-terrain
		   :reader native-terrain-mod)
   (locomotion :initarg :locomotion
	       :reader locomotion)
   (ecological-niche :initarg :ecological-niche
		     :reader ecological-niche)
   (caste :reader caste)
   (genders :reader genders)))

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
   (symbol-to-name (nth (+ (call-next-method) 5) *terrain-types*))))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'locomotion)))
  (setf (slot-value sophont slot) 
	(flux-on 
	 (nth (random 6) *locomotion-types*)
	 :dm (native-terrain-mod sophont))))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'ecological-niche)))
  (let ((basic-niche (nth (+ (flux) 6) *basic-niche*)))
    (setf (slot-value sophont slot) 
	  (list basic-niche 
		(flux-on  
		 (getf *ecological-niche* basic-niche)
		 :shift 6
		 :dm (native-terrain-mod sophont))))))

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

(defvar *characteristic-values*
  '((1 1 2 2 2 2 3 4 5 6 7 8)
    (1 1 2 2 2 2 2 3 3 3 3 3)
    (1 1 2 2 2 2 2 3 3 3 3 3)
    (1 1 2 2 2 2 2 3 3 3 3 3)
    (1 1 2 2 2 2 2 2 2 2 2 2)
    (1 1 2 2 2 2 2 2 2 2 2 2)))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'characteristics)))
  (let ((dm (cond
	      ((eql (locomotion sophont) 'flyer) -2)
	      ((eql (locomotion sophont) 'swimmer) 2)
	      ((eql (locomotion sophont) 'diver) 2)
	      (t 0)))
	(characteristics))
    (dotimes (c 6) (push 
		    (flux-on (nth c *sophont-characteristics*) :dm dm) characteristics))
    (setf (slot-value sophont slot) (nreverse characteristics))))

;; If characteristics is set using :genetic-profile, we have to
;; convert it to a list. If not, it has either been explicitly set as
;; a list in the initargs, or we will trigger generation through
;; slot-unbound.
(defmethod characteristics :around ((sophont sophont-class))
  (let ((characteristics (slot-value sophont 'characteristics)))
    (when (stringp characteristics)
      (let ((characteristics-list)
	    (characteristics-grouped (mapcar #'remove-duplicates *sophont-characteristics*)))
	;; Change spelling of Caste to Kaste to make it match with the
	;; "K" in the GP string.
	(nsubstitute 'kaste 'caste (nth 5 characteristics-grouped))
	(dotimes (c 6)
	  (push
	   (find (aref characteristics c) 
		 (nth c characteristics-grouped)
		 :test #'(lambda (x y) 
			   (eql x (aref (string-upcase (symbol-name y)) 0))))
	   characteristics-list))
	;; Revert 'Kaste' spelling back to normal.
	(nsubstitute 'caste 'kaste characteristics-list)
	(setf (slot-value sophont 'characteristics) (nreverse characteristics-list)))))
  (call-next-method))

(defmethod genetic-profile ((sophont sophont-class))
  (let ((clist (characteristics sophont))
	(profile-list))
    (dolist (c clist)
      (push (subseq (symbol-name c) 0 1) profile-list))
    ;; The last characteristic has two posibilities starting with a
    ;; 'C'. If it is Caste, then the genetic profile letter (currently
    ;; the first element of profile-list) must be 'K' to distinguish
    ;; it from Charisma.
    (if (eql (car (reverse clist)) 'caste)
	(setf (car profile-list) "K"))
    (format nil "~{~a~}" (nreverse profile-list))))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'characteristic-dice)))
  (let ((result-list))
    (dotimes (c 6)
      (let* ((characteristic (nth c (characteristics sophont)))
	     (cv-column (nth c *characteristic-values*))
	     (dm (+
		  (if (<= c 2) (native-terrain-mod sophont) 0)
		  (if (find 'chaser (ecological-niche sophont)) 2 0)
		  (if (find 'pouncer (ecological-niche sophont)) -2 0)))
	     (result (if (or
			  (eql characteristic 'education)
			  (eql characteristic 'training))
			 2
			 (flux-on cv-column :dm dm))))
	(push result result-list)))
    (setf (slot-value sophont slot) (nreverse result-list))))

;; sophont constructor
(defmacro defsophont (&rest initargs)
  `(make-instance 'sophont-class
		  :direct-superclasses (list (find-class 'sophont))
		  ,@initargs))

;;; Individual sophonts

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
		    :initarg :characteristics)
   (genetics :accessor genetics))
  (:metaclass sophont-class))

;;; Individual Characteristics
(defmethod slot-unbound (class (specimen sophont) (slot (eql 'characteristics)))
  (let ((characteristic-list) (genetic-list))
    (dotimes (c 6)
      (let ((characteristic-value 0)
	    (dice (nth c (characteristic-dice (class-of specimen)))))
	(if (>= dice 4)
	    (progn
	      (- dice 2)
	      (setf characteristic-value 12)))
	(push (roll 1) genetic-list)
	(incf characteristic-value (+ (roll (decf dice)) (car genetic-list)))
	(push characteristic-value characteristic-list)))
    (setf (slot-value specimen 'genetics) (nreverse genetic-list))
    (setf (slot-value specimen slot) (nreverse characteristic-list))))


