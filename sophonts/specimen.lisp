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
   (caste :accessor caste))
  (:metaclass sophont-class
              :documentation "Class to represent sophont individuals"))

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

(defgeneric c (specimen n)
  (:documentation "Generic function to get Characteristic n from a Sophont"))

(defmethod c ((specimen sophont) n)
  (nth (- n 1) (characteristics specimen)))

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

(defmethod slot-unbound (class (specimen sophont) (slot (eql 'caste)))
  (cond
    ((not (caste-p (class-of specimen))) (setf (slot-value specimen 'caste) nil))
    ((and (caste-p (class-of specimen))
          (eql (caste-structure (class-of specimen))
               'skilled)) (setf (slot-value specimen 'caste) (roll-on (roll-on (roll-on *caste-skills* :sides 3)))))
    (t (setf (caste specimen) (nth (c specimen 5) (caste-table (class-of specimen)))))))

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
  (let ((age-thresholds (cumulative-ages (life-stages (class-of specimen)))))
    (nth
     (position-if #'(lambda (x) (>= x (age specimen))) age-thresholds)
     *life-stages*)))

(defmethod check-aging ((specimen sophont))
  (let ((affected-characteristics 0)
        (ch (characteristics specimen))
        (ch-indexes '(0 1 2))
        (life-stage-index (position (current-life-stage specimen) *life-stages*)))
    (if (>= life-stage-index 9)
        (append ch-indexes '(5)))
    (dolist (n ch-indexes)
      (when (< (roll 2) life-stage-index)
        (when (eql (decf (nth n ch)) 0)
          (setf (nth n ch) 1)
          (incf affected-characteristics))))
    (setf (health-status specimen)
          (cond
            ((eql affected-characteristics 2) 'major-illness)
            ((and (eql affected-characteristics 3)
                  (not (%mortal-illness-p specimen)))
             (setf (%mortal-illness-p specimen) t) 'etremely-major-illness)
            ((and (eql affected-characteristics 3)
                  (%mortal-illness-p specimen)) 'dead)
            ('ok)))))

(defmethod age-up ((specimen sophont) &key (increase 1))
  (incf (age specimen) increase))

(defmethod slot-unbound (class (specimen sophont) (slot (eql 'age)))
  "If age is not set, it will default to the start of Young Adult, the
age of a starting character before Career Resolution. This will also set the next Aging Check to the start of Life Stage 5, Peak."
  (setf (%next-aging-check specimen)
        ( + 1 (nth 4 (cumulative-ages (life-stages (class-of specimen))))))
  (setf (age specimen) (nth 2 (cumulative-ages (life-stages (class-of specimen))))))

(defmethod calculate-next-aging-check ((specimen sophont))
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
