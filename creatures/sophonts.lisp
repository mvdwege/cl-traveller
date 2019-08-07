;;;; Sophont generation and management.

;;; We could just implement it using a sophont base class and using
;;; the defsophont macro to define new subclasses. However, the MOP
;;; gives us the tools to store sophont racial data in the class
;;; definition out of sight of the instances, and to add behaviour to
;;; that data. So see this as an experiment in using AMOP.
(in-package :traveller)

;;; Sophont class metaobject
(defclass sophont-class (creature-class)
  ((characteristics :initarg :characteristics
            :initarg :genetic-profile
            :reader characteristics)
   (characteristic-dice :initarg :characteristic-dice
            :reader characteristic-dice)
   (caste-structure :initarg :caste-structure
            :reader caste-structure)
   (caste-table :initarg caste-table
        :reader caste-table)
   (caste-shift-method :initarg caste-shift-method
               :reader caste-shift-method)
   (caste-assignment-method :initarg caste-assignment-method
                :reader caste-assignment-method)
   (gender-structure :initarg :gender-structure
             :reader gender-structure)
   (gender-table :initarg :gender-table
         :reader gender-table)
   (gender-differences :initarg :gender-differences
               :reader gender-differences)
   (life-stages :initarg :life-stages
                :reader life-stages))
  (:documentation "Sophont metaclass, used to generate sophont
classes (instances of which are individual beings"))

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

(defgeneric genetic-profile (sophont)
  (:documentation "Return the Genetic Profile for this sophont"))

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
             (flux-on cv-column :shift 5 :dm dm))))
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
  '(((ACV Comms High-G Steward Ordnance Naval-Arch)
     (JOT Rider Sensors Fwd-Obs Survival Streetwise)
     (LTA Spines Flapper Seafarer nil Astrogator)
     (WMD Leader Tracked Engineer Computer Navigation)
     (Chef Survey Animals Fluidics Bay-Wpns Explosives)
     (Mole Dancer Tactics Launcher Magnetics Jump-Drive))
    ((Grav Artist Turrets Teamster Photonics Counsellor)
     (Boat Legged Teacher Designer Vacc-Suit Submersible)
     (Ship Sapper Unarmed Engineer Artillery Aeronautics)
     (Wing Driver Exotics Language Craftsman Aquanautics)
     (Recon Gunner Stealth Musician Gravitics BattleDress)
     (Actor Blades Trainer Strategy Forensics Electronics))
    ((Flyer Zero-G Animals Maneuver Biologics Hostile-Environment)
     (Pilot Author Liaison Polymers Ortillery Power-Systems)
     (Rotor Broker Athlete Advocate Automotive Life-Support)
     (Admin Trader Fighter Computer Bureaucrat Slug-Thrower)
     (Beams Sprays Wheeled Diplomat Heavy-Weapons Fleet-Tactics)
     (Medic Gambler Screens Mechanic Programmer Spacecraft))))

(defvar *caste-shift-methods*
  '(nil nil nil nil mid-life-shift rotation))

(defvar *caste-assignment-methods*
  '(assigned-at-birth assigned-at-adolescence assigned-by-heredity
    assigned-by-community family-choice personal-choice))

(defmethod caste-p ((sophont sophont-class))
  (eql (nth 5 (characteristics sophont)) 'caste))


(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'caste-structure)))
  (setf (slot-value sophont 'caste-structure)
    (if (caste-p sophont)
        (roll-on *caste-structure-table*)
        nil)))

(defmethod generate-caste-table ((sophont sophont-class))
  (let* ((caste-table-values (getf *caste-tables* (caste-structure sophont)))
         (caste-table (loop repeat 11 collect (let ((roll (flux-on caste-table-values)))
                                                (if (eq roll 'special)
                                                    (flux-on (getf *caste-tables* 'special))
                                                    roll)))))
    ;; We set the slot, so we can now safely use the reader to set the
    ;; special values, 2D6=7 Common and 2D6=12 Unique.
    (setf (nth 5 caste-table)
          (nth 5 caste-table-values))
    (setf (nth 10 caste-table)
          (getf *caste-tables-uniques* (caste-structure sophont)))
    caste-table))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'caste-table)))
  (setf (slot-value sophont 'caste-table)
        (cond
          ((not (caste-p sophont)) nil)
          ((and (caste-p sophont) (eq (caste-structure sophont) 'skilled)) nil)
          (t (generate-caste-table sophont)))))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'caste-shift-method)))
  (setf (slot-value sophont 'caste-shift-method) (roll-on *caste-shift-methods*)))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'caste-assignment-method)))
  (setf (slot-value sophont 'caste-assignment-method)
      (if (caste-p sophont) (roll-on *caste-assignment-methods*))))

(defvar *life-stages*
  '(infant child teen young-adult adult peak mid-life senior elder retirement))

(defvar *life-stage-duration*
;;  Table S-09-A, page 561 in edition 5.09
  '((1/2 1 0 0 0 1 0 0 0 1)
    (1/2 1 1 1 1 1 1 1 1 1)
    (1/2 1 1 1 1 1 1 1 1 1)
    (1/2 1 1 1 1 1 1 1 1 1)
    (1/2 2 2 2 2 2 2 2 2 2)
    (1/2 2 2 2 2 2 2 2 2 2)
    (1/2 2 2 2 2 2 2 2 2 2)
    (1/2 3 3 3 3 3 3 3 3 3)
    (1/2 3 3 3 3 3 3 3 3 3)
    (1/2 4 4 4 4 4 4 4 4 4)
    (1/2 6 6 6 6 6 6 6 6 6)))

(defmethod slot-unbound (class (sophont sophont-class) (slot (eql 'life-stages)))
  (setf (slot-value sophont slot)
        (mapcar #'(lambda (term) (let ((index (position term *life-stages*)))
                                   (cons term (nth index (flux-on *life-stage-duration*)))))
                *life-stages*)))

(defmethod life-expectancy ((sophont sophont-class))
  (apply #'+ (mapcar #'(lambda (terms) (* 4 (cdr terms))) (life-stages sophont))))
