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
	   :reader gender))
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

(defmethod slot-unbound (class (specimen sophont) (slot (eql 'gender)))
  (setf (slot-value specimen slot) (roll-on (gender-table (class-of specimen)) :dice 2))) 
