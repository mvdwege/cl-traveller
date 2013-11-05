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
	       (secondary-orbit (orbit (primary self) secondary))
	       (secondary-orbits (- secondary-orbit 3)))
	  (if (> secondary-orbits 0)
	      (setf (slot-value secondary 'orbits) 
		    (make-list secondary-orbits :initial-element nil)))
	  (setf (nth secondary-orbit (orbits my-primary)) secondary))))
    (setf (slot-value self 'primary) my-primary)))

(defmethod orbits ((system system)) 
  (orbits (primary system)))

(defmethod stars ((system system))
  (let ((stars (remove-if-not 
		#'(lambda (body) (typep body 'star)) 
		(orbits system))))
    (push (primary system) stars)))

;; The orbit function takes a primary argument which is superfluous
;; for secondary stars but vital for planets, gas giants and
;; satellites, which calculate their orbit based on their primary.
(defgeneric orbit (primary body)
  (:documentation "Return the correct orbital position for a celestial body"))

;; In practice this method will never be called directly; it is here
;; to abstract out the common function of finding the position of a
;; body relative to its primary. The subclasses will call it using
;; call-next-method.
(defmethod orbit ((primary body) (body body))
  (or
   (position body (orbits primary))
   nil))

;; Determine orbits of any secondary stars. Note that we do not use
;; the star in the function itself, it's only in the lambda list for
;; dispatching purposes.
(defmethod orbit ((primary body) (star close-star))
  (roll 1 :dm -1))

(defmethod orbit ((primary body) (star near-star))
  (roll 1 :dm 5))

(defmethod orbit ((primary body) (star far-star))
  (roll 1 :dm 11))

(defmethod orbit ((primary body) (gas-giant large-gas-giant))
  (or
   (call-next-method)
   (min
    (max 
     0
     (+ (habitable-zone primary) (roll 2 :dm -5)))
    (last-orbit primary))))

(defmethod orbit ((primary body) (gas-giant small-gas-giant))
  (or
   (call-next-method)
   (min
    (max
     0
     (+ (habitable-zone primary) (roll 2 :dm -4)))
    (last-orbit primary))))

(defmethod orbit ((primary body) (gas-giant ice-giant))
  (or
   (call-next-method)
   (min
    (+ (habitable-zone primary) (roll 2 :dm -1))
    (last-orbit primary))))

(defun number-of-gas-giants () 
  (truncate (max 0 (- (/ (roll 2) 2) 2))))
    
(defun number-of-planetoid-belts ()
  (truncate (max 0 (- (roll 1) 3))))

(defmethod occupied-orbits ((star star) body-type)
  (count-if #'(lambda (x) (typep x body-type)) (orbits star)))

(defmethod free-orbits ((body body))
  (count nil (orbits body)))

(defmethod free-orbits ((star star))
  (- (call-next-method) (or (surface-orbit star) 0)))

(defmethod first-free-inner-orbit ((primary body) (body body) orbital-position) 
  (position nil (orbits primary) :from-end t :end (+ 1 orbital-position)))

(defmethod first-free-outer-orbit ((primary body) (body body) orbital-position) 
  (position nil (orbits primary) :start orbital-position))

(defmethod first-free-inner-orbit ((primary body) (body ice-giant) orbital-position)
  (position nil (orbits primary) :from-end t :end orbital-position :start (+ 1 (habitable-zone primary))))

(defmethod closest-free-orbit ((primary body) (body body) orbital-position)
  (+
   orbital-position
   (car 
    (sort 
     (list 
      (-
       ;; If there is no free orbit, the first-free-*-orbit functions
       ;; return nil. We then coerce the result to 99 and -99
       ;; respectively to make sure that they will always be the
       ;; furthest away, so the other will be chosen.
       (or
	(first-free-outer-orbit primary body orbital-position)
	99)
       orbital-position)
      (- 
       (or
	(first-free-inner-orbit primary body orbital-position)
	-99)
       orbital-position))
     #'<
     :key #'abs))))

(defmethod slot-unbound (class (system system) (slot (eql 'gas-giants)))
  (setf 
   (slot-value system 'gas-giants)
   (loop repeat (number-of-gas-giants) collect (make-instance 'gas-giant))))

(defmethod place-gas-giants ((system system))
  (let ((stars (stars system)))
    (dolist (gas-giant (gas-giants system))
      ;; Remove any stars from the list that have no free orbits
      (remove-if #'(lambda (star) (= 0 (free-orbits star))) stars)
      ;; Now sort the list on occupied orbits. The one with the fewest
      ;; is the car of the list and the next to receive a gas giant.
      (setf stars 
	    (sort stars 
		  #'< :key 
		  (lambda (star) (occupied-orbits star 'gas-giant))))
      ;; Calculate gas giant size and replace every other small gas
      ;; giant with an ice giant
      (size gas-giant)
      (if (and
	   (typep gas-giant 'small-gas-giant)
	   (oddp (count-if #'(lambda (small-gas-giant) (typep small-gas-giant 'small-gas-giant)) (gas-giants system))))
	  (change-class gas-giant 'ice-giant))
      ;; Calculate orbit and place gas giant
      (let* ((star (car stars))
	     (potential-orbit (orbit star gas-giant)))
	;; Occupied orbit?
	(if (nth potential-orbit (orbits star))
	;; Yes, find closest free orbit
	    (setf potential-orbit 
		  (closest-free-orbit star gas-giant potential-orbit)))
	(setf (nth potential-orbit (orbits star)) gas-giant)))))
