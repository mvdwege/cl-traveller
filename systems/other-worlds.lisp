(in-package :traveller)
;;;; Generate other world types beyond Mainworlds.

;;; Normal word types
(defclass planetoids (world)
  ((size :initform (make-instance 'size :code 0))
   (atmosphere :initform (make-instance 'atmosphere :code 0))
   (hydrographics :initform (make-instance 'hydrographics :code 0))))

(defclass iceworld (world) () )

(defmethod slot-unbound (class (w iceworld) (slot (eql 'population)))
  (setf (slot-value w 'population)
	(make-instance 'population :code (max 0 (roll 2 :dm -6)))))

(defclass bigworld (world) () )

(defmethod slot-unbound (class (bigworld bigworld) (slot (eql 'size)))
  (setf (slot-value bigworld 'size)
	(make-instance 'size :code 
		       (do ((size 0 (roll 2 :dm 7))) ((>= size 11) size) ()))))
;;; Gas Giants
(defclass gas-giant (body)
    ((size :reader size)
     (orbits :initform (make-list 26 :initial-element nil))))

(defclass large-gas-giant (gas-giant) () )
(defclass small-gas-giant (gas-giant) () )
(defclass ice-giant (gas-giant) () )
(defclass brown-dwarf (gas-giant secondary-star) 
  ((size :initform (make-instance 'size :code 24))
   (spectrum :initform '(BD nil nil))))

;; (defmethod size :around ((self gas-giant))
;;   (code (call-next-method)))

(defmethod slot-unbound (class (self gas-giant) (slot (eql 'size)))
  (let ((random-size (+ (roll 2) 19)))
    (if (<= random-size 22)
	(change-class self 'small-gas-giant)
	(change-class self 'large-gas-giant))
    (setf (slot-value self 'size) (make-instance 'size :code random-size))))


