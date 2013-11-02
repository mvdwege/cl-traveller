(in-package :traveller)

(defclass system ()
  ((primary
    :reader primary)
   (gas-giants :reader gas-giants)))
   
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
      
(defmethod orbits ((system system)) 
  (orbits (primary system)))

;; The orbit function takes a primary argument which is superfluous
;; for secondary stars and satellites, but vital for planets, gas
;; giants and satellites, which calculate their orbit based on their
;; primary.
(defgeneric orbit (primary body)
  (:documentation "Generate the correct orbital position for a celestial body"))

;; Determine orbits of any secondary stars. Note that we do not use
;; the star in the function itself, it's only in the lambda list for
;; dispatching purposes.
(defmethod orbit (primary (star close-star))
  (roll 1 :dm -1))

(defmethod orbit (primary (star near-star))
  (roll 1 :dm 5))

(defmethod orbit (primary (star far-star))
  (roll 1 :dm 11))

(defmethod orbit ((primary body) (gas-giant gas-giant))
  (or
   (position gas-giant (orbits primary))
   nil))

(defmethod orbit ((primary body) (gas-giant large-gas-giant))
  (or
   (call-next-method)
   (+ (habitable-zone primary) (roll 2 :dm -5))))

(defmethod orbit ((primary body) (gas-giant small-gas-giant))
  (or
   (call-next-method)
   (+ (habitable-zone primary) (roll 2 :dm -4))))

(defmethod orbit ((primary body) (gas-giant ice-giant))
  (or
   (call-next-method)
   (+ (habitable-zone primary) (roll 2 :dm -1))))

(defun number-of-gas-giants () 
  (truncate (max 0 (- (/ (roll 2) 2) 2))))
    
(defun number-of-planetoid-belts ()
  (truncate (max 0 (- (roll 1) 3))))

(defmethod slot-unbound (class (system system) (slot (eql 'gas-giants)))
  (let ((number-of-gas-giants (number-of-gas-giants)))
    (loop repeat number-of-gas-giants do 
	 (let ((gas-giant (make-instance 'gas-giant)))
	   (size gas-giant)
	   (if (typep gas-giant 'small-gas-giant)
	       ;; Every other Small Gas Giant is an Ice
	       ;; Giant. Mathematically this the same as generating an
	       ;; Ice Giant if there is an odd number of Small Gas
	       ;; Giants already in the system.
	       (if (oddp 
		    (count-if 
		     #'(lambda (body) (typep body 'small-gas-giant)) 
		     (orbits (primary system))))
		   (change-class gas-giant 'ice-giant)))
	   (setf (nth (orbit (primary system) gas-giant) 
		      (orbits (primary system))) 
		 gas-giant))))
  ;; We generated the gas giants and put them in their orbits. Now we
  ;; gather them back and assign them to the gas-giants slot.
  (setf (slot-value system 'gas-giants) 
	(remove-if-not 
	 (lambda (body) (typep body 'gas-giant)) 
	 (orbits (primary system)))))
	       
