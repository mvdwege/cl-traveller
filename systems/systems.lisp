(in-package :traveller)

(defclass system ()
  ((primary
    :reader primary)))
   
(defmethod slot-unbound (class (self system) (slot (eql 'primary)))
  (let ((my-primary (make-instance 'primary-star)))
    (dolist (potential-secondary '(close-star near-star far-star))
      (when (>= (flux) 3)
	(let* ((secondary 
		(make-instance potential-secondary :primary my-primary))
	       (secondary-orbit (orbit self secondary))
	       (secondary-orbits (- secondary-orbit 3)))
	  (if (> secondary-orbits 0)
	      (setf (slot-value secondary 'orbits) 
		    (make-list secondary-orbits :initial-element nil)))
	  (setf (nth secondary-orbit (orbits my-primary)) secondary))))
    (setf (slot-value self 'primary) my-primary)))
      
;; The orbit function takes a system argument which is superfluous for
;; secondary stars and satellites, but vital for planets and gas
;; giants, which calculate their orbit based on the system primary.
(defgeneric orbit (system body)
  (:documentation "Generate the correct orbital position for a celestial body"))

;; Determine orbits of any secondary stars. Note that we do not use
;; the star in the function itself, it's only in the lambda list for
;; dispatching purposes.
(defmethod orbit ((system system) (star close-star))
  (roll 1 :dm -1))

(defmethod orbit ((system system) (star near-star))
  (roll 1 :dm 5))

(defmethod orbit ((system system) (star far-star))
  (roll 1 :dm 11))

(defun number-of-gas-giants () 
  (truncate (max 0 (- (/ (roll 2) 2) 2))))
    
(defun number-of-planetoid-belts ()
  (truncate (max 0 (- (roll 1) 3))))
