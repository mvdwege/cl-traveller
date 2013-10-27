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

;;; Gas Giants
(defclass gas-giant (body)
    ((size)))

(defclass large-gas-giant (gas-giant) () )
(defclass small-gas-giant (gas-giant) () )
(defclass ice-giant (gas-giant) () )
(defclass brown-dwarf (gas-giant secondary-star) 
  ((size :initform (make-instance 'size :code 24))) )
