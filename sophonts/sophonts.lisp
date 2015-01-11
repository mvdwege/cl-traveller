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
   (caste-structure :initarg :caste-structure
		    :reader caste-structure)
   (caste-table :initarg caste-table
		:reader caste-table)
   (gender-structure :initarg :gender-structure
		     :reader gender-structure)
   (gender-table :initarg :gender-table
		 :reader gender-table)
   (gender-differences :initarg :gender-differences
		       :reader gender-differences)))

;; Keep the compiler happy. Especially SBCL insists on this method
;; existing. We're not doing really deep MOP things, so a simple "Yes,
;; this is supposed to be a subclass of standard-class" will suffice.
(defmethod closer-mop:validate-superclass ((class sophont-class) (super standard-class)) 
  t)

;;; Homeworld generation
(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'homeworld)))
  (setf (slot-value sophont slot)
	;; Brute-force approach: generate worlds until we have one
	;; suitable for life.
	(do ((world (make-world) (make-world)))
	    ((and
	      (>= (population world) 7)
	(>= (atmosphere world) 2)) world)
	  ())))
     
;;; Determine Native Terrain, Locomotion, and Ecological Niche.
(defvar *terrain-types* 
  '(mountain desert exotic rough-wood rough clear forest wetlands
  wetland-woods ocean ocean-depths))

(defvar *locomotion-types*
  '((walker walker amphib amphib amphib walker walker amphib amphib flyphib aquatic)
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
    scavenger (carrion-eater carrion-eater carrion-eater hijacker
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
	(nth (+ 5 (native-terrain-mod sophont))(nth (random 6) *locomotion-types*))))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'ecological-niche)))
  (let ((basic-niche (flux-on *basic-niche* :shift 6)))
    (if (eq basic-niche 'producer)
	(setf (slot-value sophont 'locomotion) 'immobile))
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

;;; Number of dice to roll for characteristic generation
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

;;; Gender generation
(defvar *gender-structure-table*
  '(solitaire solitaire eab eab dual dual dual fmn fmn group group))

;; Mapping from numbered genders to names
(defvar *gender-structures*
  '(solitaire (solo)
    dual (female male)
    fmn (female male neuter)
    eab (egg-donor activator bearer)
    group (one two three four five six)))

(defvar *gender-tables*
  `(solitaire (,@(loop repeat 11 collect 'solo))
    dual (female female female female male female male male male female male)
    eab (egg-donor egg-donor egg-donor activator egg-donor activator bearer 
	bearer bearer activator bearer)
    fmn (female female female male female male neuter neuter neuter male neuter)
    group (six six four four two one three five five six six)))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'gender-structure)))
  (setf (slot-value sophont slot) (flux-on *gender-structure-table*)))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'gender-table)))
  ;; Initial gender assignments
  (with-slots (gender-structure) sophont
    (setf (slot-value sophont slot)
	  (let ((gender-table))
	    (unless (eql gender-structure 'group)
	      (mapcar 
		#'(lambda (gender) (push gender gender-table)) 
		(getf *gender-structures* gender-structure)))
	    (do () 
		((= (length gender-table) 11) (nreverse gender-table))
	      (push (flux-on (getf *gender-tables* gender-structure)) gender-table))))))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'gender-differences)))
  (let ((difference-table nil))
    (dolist (g (remove-duplicates (gender-table sophont)))
      (let ((differences nil))
      (dotimes (c 6)
	(let ((flux-roll (flux)))
	  (push 
	   ;; Special treatment for Strength and rolls between -1 and +1.
	   (cond
	     ((and (= c 0) (>= flux-roll 3)) (roll flux-roll))
	     ((and (= c 0) (= flux-roll 1) 1))
	     ((and (>= flux-roll -1) (<= flux-roll 1)) 0)
	     (t flux-roll))
	   differences)))
      (setf (getf difference-table g) (nreverse differences))))
    (setf (slot-value sophont slot) difference-table)))
	
	

;;; Caste generation
(defvar *caste-structure-table*
  '(body economic family military social skilled))

(defvar *caste-tables* 
  '(body (healer gender antibody sensor memory muscle muscle muscle voice special claw)
    economic (innovator gender guard researcher artisan laborer craftsman clerk manager 
	      special entrepreneur)
    family (healer gender defender caregiver caregiver breadwinner breadwinner
	    breadwinner uncle special leader)
    military (medic gender aide scout specialist soldier technician warrior leader 
	      special staff)
    social (artist gender enforcer drone artist unit unit unit patron special entertainer)
    special (deminimus useless advisor- instructor shaman expendable defective valuable 
	     advisor+ sport vice-leader)))

(defvar *caste-tables-uniques*
  '(body brain economic director family archon military general social ruler))

(defvar *caste-skills*
  '(((Recon Aeronautics Admin advocate soundmimic acv)
     (Spines Aquanautics Artillery Artist Biologics Author)
     (Sensors Automotive Astrogator Beams Computer Broker)
     (Actor Bureaucracy Craftsman Computer Driver Mole)
     (Flyer BattleDress Dancer Diplomat Explosives Medic)
     (Empath Engineer Designer Exotics G-Drive Grav))
    ((Flapper Fluidics Electronics Forensics J-Drive Math)
     (Leader Heavy Wpns Engineer Legged Liaison JOT)
     (Tracked Launcher Gravitics Mechanic Athlete Trader)
     (Pilot Magnetics Hostile Env Ordnance Blades LTA)
     (Animals Life Support Language P-Plant Counsellor Sail)
     (Tactics Photonics Musician Sapper Ortillery Ship))
    ((Turrets Programmer Strategy Small Craft Fighting Rotor)
     (Seafarer Slug Thrower M-Drive Stealth Osmancer Rider)
     (Survey Naval Arch Navigation Survival Wheeled Spray)
     (Comms Streetwise Polymers Trainer Screens Sub)
     (Teacher Teamster Spacecraft Animals Steward Wing)
     (Unarmed Vacc Suit Starships No Skill Zero-G WMD))))

(defmethod caste-p ((sophont sophont-class))
  (eql (nth 5 (characteristics sophont)) 'caste))


(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'caste-structure)))
  (setf (slot-value sophont 'caste-structure)
	(if (caste-p sophont)
	    (roll-on *caste-structure-table*)
	    nil)))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'caste-table)))
  (if (caste-p sophont)
      "caste-table"
      (setf (slot-value sophont 'caste-table) nil)))

;; sophont constructor
(defmacro defsophont (&rest initargs)
  `(make-instance 'sophont-class
		  :direct-superclasses (list (find-class 'sophont))
		  ,@initargs))

;;;; Standard sophonts
;;; Humaniti
(setf (find-class 'human)
      (defsophont
	:name 'human
	:genetic-profile "SDEIES"
	:characteristic-dice '(2 2 2 2 2 2)
	:homeworld (make-world :uwp "A877B99-D")
	:native-terrain 0
	:locomotion 'walker
	:ecological-niche '(omnivore hunter-gatherer)
	:gender-structure 'dual
	:gender-table '(female male female male female male female male female male female)
	:gender-differences '(male (0 0 0 0 0 0) female (0 0 0 0 0 0))))
;;; Aslan
(setf (find-class 'aslan)
      (defsophont
	  :name 'aslan
	:genetic-profile "SDSIES"
	:characteristic-dice '(2 2 2 2 2 2)
	:homeworld (make-world :uwp "A876986-E")
	:native-terrain 1 :locomotion 'walker
	:ecological-niche '(carnivore pouncer)
	:gender-structure 'dual
	:gender-table '(male male male male female female female female female female female)
	:gender-differences '(male (2 0 2 0 0 -3) female (0 0 0 0 0 0))))
;;; K'kree
;; TODO: K'kree have Caste, but caste generation is not finished yet.
(setf (find-class 'k-kree)
      (defsophont
	  :name 'k-kree
	:genetic-profile "SDEIEC"
	:characteristic-dice '(4 2 4 2 2 2)
	:homeworld (make-world :uwp "B863A03-F")
	:native-terrain 0
	:locomotion 'walker
	:ecological-niche '(herbivore grazer)
	:gender-structure 'FMN
	:gender-table '(female male neuter female female neuter male male male neuter male)
	:gender-differences '(male (3 0 3 0 0 0) female (0 0 0 0 0 0) neuter (0 0 0 0 0 0))))
