;;;; Individual specimens of sophonts (for player or non-player
;;;; characters)
(in-package :traveller)

;; Base class for sophont individuals
(defclass sophont () 
  ((name :accessor name
	 :initarg :name)
   (homeworld :accessor homeworld
	      :initarg :homeworld)
   (birthworld :reader birthworld)
   (age :accessor age
        :initarg :age)
   (%next-aging-check
    :accessor %next-aging-check)
   (%mortal-illness-p
    :accessor %mortal-illness-p
    :initform nil)
   (health-status :accessor health-status
                  :initform 'ok)
   (birthday :reader birthday)
   (history :reader history)
   (characteristics :accessor characteristics
		    :initarg :characteristics)
   (genetics :accessor genetics)
   (gender :initarg :gender
	   :reader gender)
   (caste :accessor caste :initform nil))
  (:metaclass sophont-class
              :documentation "Class to represent sophont individuals"))

(defmethod initialize-instance :after ((specimen sophont) &key &allow-other-keys)
  (if (slot-boundp specimen 'age)
      (setf (%next-aging-check specimen) (set-next-aging-check specimen))))

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

;;; Utility functions
(defun halve-if (value name &rest characteristics)
  (if (some #'(lambda (x) (eql name x)) characteristics)
      (round (/ value 2))
      value))

;;; Characteristic handling
(defgeneric c (specimen n)
  (:documentation "Generic function to get Characteristic n from a Sophont"))

(defmethod c ((specimen sophont) n)
  (nth (- n 1) (characteristics specimen)))

(defgeneric strength (specimen) )

(defgeneric dexterity (specimen)
  (:documentation "Return C2. Full value if C2=Dex, half value if C2=Agi or C2=Gra"))

(defgeneric agility (specimen)
  (:documentation "Return C2. Full value if C2=Agi, half value if C2=Dex or C2=Gra"))

(defgeneric grace (specimen)
  (:documentation "Return C2. Full value if C2=Gra, half value if C2=Agi or C2=Dex"))

(defgeneric endurance (specimen)
  (:documentation "Return C3. Full value if C2=End, half value if C3=Vig or C3=Sta"))

(defgeneric vigor (specimen)
  (:documentation "Return C3. Full value if C3=Vig, half value if C3=End or C3=Sta"))

(defgeneric stamina (specimen)
  (:documentation "Return C3. Full value if C3=Sta, half value if C3=Vig or C3=End"))

(defgeneric education (specimen)
  (:documentation "Return C5. Full value if C5=Edu, half value if C5=Tra or 4 if C5=Ins"))

(defgeneric training (specimen)
  (:documentation "Return C5. Full value if C5=Tra, half value if C5=Edu or 4 if C5=Ins"))

(defgeneric instinct (specimen)
  (:documentation "Return C5. Full value if C5=Ins, 4 if C5=Tra or C5=Edu"))

(defgeneric social-standing (specimen)
  (:documentation "Return C6. Full value if C6=Soc, full value if C6=Cha or 4 if C6=Cas"))

(defgeneric charisma (specimen)
  (:documentation "Return C6. Full value if C6=Cha, half value if C6=Soc or 4 if C6=Cas"))

(defgeneric caste (specimen)
  (:documentation "Return C6. Returns the Caste symbol if C6=Cas, 4 if called for Soc or Cha"))

(defmethod strength ((specimen sophont))
  (c specimen 1))

;; For now we use the brute force method to check if we need to halve
;; a Characteristic, defining mostly the same functions. Later we
;; might refactor that, for example defining the methods
;; programmatically.
(defmethod dexterity ((specimen sophont))
  (let ((value (c specimen 2))
	(name (nth 1 (characteristics (class-of specimen)))))
    (halve-if value name 'agility 'grace)))

(defmethod agility ((specimen sophont))
  (let ((value (c specimen 2))
	(name (nth 1 (characteristics (class-of specimen)))))
    (halve-if value name 'dexterity 'grace)))

(defmethod grace ((specimen sophont))
  (let ((value (c specimen 2))
	(name (nth 1 (characteristics (class-of specimen)))))
    (halve-if value name 'dexterity 'agility)))

(defmethod endurance ((specimen sophont))
  (let ((value (c specimen 3))
	(name (nth 2 (characteristics (class-of specimen)))))
    (halve-if value name 'stamina 'vigor)))

(defmethod stamina ((specimen sophont))
  (let ((value (c specimen 3))
	(name (nth 2 (characteristics (class-of specimen)))))
    (halve-if value name 'endurance 'vigor)))

(defmethod vigor ((specimen sophont))
  (let ((value (c specimen 3))
	(name (nth 2 (characteristics (class-of specimen)))))
    (halve-if value name 'endurance 'stamina)))

(defmethod education ((specimen sophont))
  (let ((value (c specimen 5))
	(name (nth 4 (characteristics (class-of specimen)))))
    (if (eq name 'instinct)
	4
	(halve-if value name 'training))))

(defmethod training ((specimen sophont))
  (let ((value (c specimen 5))
	(name (nth 4 (characteristics (class-of specimen)))))
    (if (eq name 'instinct)
	4
	(halve-if value name 'education))))

(defmethod instinct ((specimen sophont))
  (let ((value (c specimen 5))
	(name (nth 4 (characteristics (class-of specimen)))))
    (if (or (eq name 'training) (eq name 'education))
	4
	value)))

(defmethod social-standing ((specimen sophont))
  (let ((value (c specimen 6))
	(name (nth 5 (characteristics (class-of specimen)))))
    (if (eq name 'caste)
	4
	value)))

(defmethod charisma ((specimen sophont))
  (let ((value (c specimen 6))
	(name (nth 5 (characteristics (class-of specimen)))))
    (cond ((eq name 'social-standing) (/ value 2))
	  ((eq name 'caste) 4)
	  (t value))))

(defmethod caste :around ((specimen sophont))
  (let ((name (nth 5 (characteristics (class-of specimen)))))
    (if (or (eq name 'social-standing) (eq name 'charisma))
	4
	(call-next-method))))

(defmethod slot-unbound (class (specimen sophont) (slot (eql 'genetics)))
  (let ((genetics nil))
    (dolist (x (characteristics specimen))
      (let ((gene (roll 1)))
	(if (> gene x)
	    (push x genetics)
	    (push gene genetics))))
    (setf (slot-value specimen slot) (nreverse genetics))))

(defmethod slot-unbound (class (specimen sophont) (slot (eql 'gender)))
  (setf (slot-value specimen slot) (roll-on (gender-table (class-of specimen)) :dice 2))
  (let ((differences (getf (gender-differences (class-of specimen)) (gender specimen))))
    (setf (slot-value specimen 'characteristics)
	  (mapcar #'+ (characteristics specimen) differences)))
  (gender specimen))

;;; Aging
(defmethod life-expectancy ((specimen sophont))
  (life-expectancy (class-of specimen)))

(defun cumulative-ages (life-stages)
  (loop for term-length in
       (mapcar
        #'(lambda (x) (* (cdr x) 4))
        life-stages)
        sum term-length into cumulative-length
        collect (- cumulative-length 1)))

(defmethod current-life-stage ((specimen sophont))
  (let* ((age-thresholds (cumulative-ages (life-stages (class-of specimen))))
         (retirement-age (car (last age-thresholds))))
    (cond
      ((>= (age specimen) retirement-age) 'retirement)
      (t (nth
          (position-if #'(lambda (x) (>= x (age specimen))) age-thresholds)
          *life-stages*)))))

(defmethod check-aging ((specimen sophont))
  (if (eql (health-status specimen) 'dead)
      (return-from check-aging 'dead))
  (let ((affected-characteristics 0)
        (ch (characteristics specimen))
        (ch-indexes '(0 1 2))
        (life-stage-index (position (current-life-stage specimen) *life-stages*)))
    ;; Intelligence always gets checked, but if Instinct is there, it
    ;; should be checked too.
    (cond
      ((and (>= life-stage-index 9)
            (eql (nth 4 (characteristics (class-of specimen))) 'instinct))
       (append ch-indexes '(3 4)))
      ((>= life-stage-index 9) (append ch-indexes '(3))))
    (dolist (n ch-indexes)
      (when (< (roll 2) life-stage-index)
        (when (eql (decf (nth n ch)) 0)
          (setf (nth n ch) 1)
          (incf affected-characteristics))))
    (setf (health-status specimen)
          (cond
            ((eql affected-characteristics 2) 'serious-illness)
            ((and (eql affected-characteristics 3)
                  (not (%mortal-illness-p specimen)))
             (setf (%mortal-illness-p specimen) t) 'major-illness)
            ((and (eql affected-characteristics 3)
                  (%mortal-illness-p specimen)) 'dead)
            ('ok)))))

(defmethod slot-unbound (class (specimen sophont) (slot (eql 'age)))
  "If age is not set, it will default to the start of Young Adult, the
age of a starting character before Career Resolution. This will also set the next Aging Check to the start of Life Stage 5, Peak."
  (setf (age specimen) (+ 1 (nth 2 (cumulative-ages (life-stages (class-of specimen))))))
  (setf (%next-aging-check specimen) (set-next-aging-check specimen))
  (age specimen))

(defmethod set-next-aging-check ((specimen sophont))
  (let ((life-stage (position (current-life-stage specimen) *life-stages*))
        (peak-start-age (+ (nth 4 (cumulative-ages (life-stages (class-of specimen)))) 1)))
    ;; We have three conditions: Life Stage is less than Peak, Life
    ;; Stage is exactly the first year of Peak, or age falls in Peak
    ;; or higher.
    (setf (%next-aging-check specimen)
          (cond
            ((< life-stage 4)
             peak-start-age)
            ((= (age specimen) peak-start-age)
             peak-start-age)
            (t
             (do ((current-age peak-start-age))
                 ((> current-age (age specimen)) current-age)
                (incf current-age 4)))))))

(defmethod shift-caste ((specimen sophont))
  (let ((c (caste-shift-method (class-of specimen)))
	(age-thresholds
	 (mapcar
	  #'(lambda (x) (+ 1 x))
	  (cumulative-ages (life-stages (class-of specimen)))))
	(current-age (age specimen)))
    (cond
      ((and (eql c 'rotation) (position current-age age-thresholds))
       ;; Increase Caste by 1, roll over from 12 to 1 and gain new
       ;; Skill if 'skilled
       (if (eq (c specimen 5) 12)
	    (setf (nth 5 (characteristics specimen)) 2)
	    (incf (nth 5 (characteristics specimen))))
       (if (eql (caste-structure specimen) 'skilled)
	    (roll-on (roll-on (roll-on *caste-skills*)))))
      ((and (eql c 'mid-life-shift) (eql (nth 5 age-thresholds) current-age))
       (setf (nth 5 (characteristics specimen)) (roll-on (caste-table (class-of specimen)))))
      (t
       nil))))

(defmethod %random-caste ((specimen sophont))
    (cond
    ((not (caste-p (class-of specimen))) (setf (slot-value specimen 'caste) nil))
    ((and (caste-p (class-of specimen))
          (eql (caste-structure (class-of specimen))
               'skilled)) (setf (slot-value specimen 'caste) (roll-on (roll-on (roll-on *caste-skills* :sides 3)))))
    (t (setf (caste specimen) (nth (c specimen 5) (caste-table (class-of specimen)))))))

(defmethod assign-caste ((specimen sophont))
  ;; Only execute when we are a casted sophont species, and no caste
  ;; has been set yet
  (when (and (caste-p (class-of specimen))
	     (not (caste specimen)))
    ;; We have 3 conditions: life stage zero, life stage 2, and any
    ;; life stage higher. Within those, we also have the possibility
    ;; to throw an 'interaction-required error.
    (let ((method (caste-assignment-method (class-of specimen)))
	  (age-thresholds
	   (mapcar
	    #'(lambda (x) (+ 1 x))
	    (cumulative-ages (life-stages (class-of specimen)))))
	  (current-age (age specimen)))
      (restart-case
	  (cond
	    ;; Newborn or older, but random assignment at Life Stage 0
	    ((and
	      (position method '(assigned-at-birth assigned-by-heredity assigned-by-community))
	      (>= current-age 0)) (%random-caste specimen))
	    ;; Assigned at Adolescence, and Adolescent or older, still random
	    ((and
	      (eql method 'assigned-at-adolescence)
	      (>= current-age (nth 1 age-thresholds))) (%random-caste specimen))
	    ;; What's left is interactive by default, if Adolescent or older
	    ((>= current-age (nth 1 age-thresholds))
	     (error 'interaction-required
		    :what "Caste Assignment"
		    :options (remove-duplicates (caste-table (class-of specimen))))))
	(store-value (value)
	  :interactive
	  (lambda () (format *query-io* "~%Alternative: ") (list (read *query-io*)))
	  (setf (caste specimen) value)))))
  ;; Whatever else, always return caste.
  (caste specimen))
		 
	      
(defmethod (setf age) :around (new-age (specimen sophont))
  ;; Set new age, then run all checks necessary if the age is on a
  ;; stage boundary (Aging Checks, Caste Assignment and Caste shift,
  ;; Gender Assignment and Gender Shift).
  (if (eql (health-status specimen) 'dead)
      (error "~a is already dead" specimen))
  (call-next-method)
  (let ((methods '(set-next-aging-check
		   assign-caste
		   shift-caste
		   check-aging)))
    (dolist (method methods)
      (funcall method specimen))))
