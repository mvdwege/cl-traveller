(in-package :traveller)

(defclass system ()
  ((mainworld)
   (primary)
   (orbits :initform (make-array 20 :initial-element nil)
	   :reader orbits)))

(defun number-of-gas-giants () 
  (truncate (max 0 (- (/ (roll 2) 2) 2))))
    
(defun number-of-planetoid-belts ()
  (truncate (max 0 (- (roll 1) 3))))
