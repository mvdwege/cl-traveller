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
   (characteristics :accessor characteristics
		    :initarg :characteristics)
   (genetics :accessor genetics)
   (gender :initarg :gender
	   :reader gender)
   (caste :accessor caste))
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

(defmethod c ((specimen sophont) n)
  (nth n (characteristics specimen)))

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
    (t (nth (c specimen 5) (caste-table (class-of specimen))))))

