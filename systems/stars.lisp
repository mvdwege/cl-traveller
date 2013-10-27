;;;; Generate stars and star systems
(in-package :traveller)

(defvar *spectral-type-table* '(OB A A F F G G K K M M M BD BD BD))

(defvar *size-table* '(O (Ia Ia Ib II III III III V V V IV D IV IV IV)
		       B (Ia Ia Ib II III III III III V V IV D IV IV IV)
		       A (Ia Ia Ib II II IV V V V V V D V V V)
		       F (II II III IV V V V V V V VI D VI VI VI)
		       G (II II III IV V V V V V V VI D VI VI VI)
		       K (II II III IV V V V V V V VI D VI VI VI)
		       M (II II II II III V V V V VI D VI VI VI)))

;; In order to get the right results, the flux roll must be shifted by
;; 6 to index into the *spectral-type-table* and the *size-table*.
(defvar *flux-shift* 6)

(defclass star ()
  ((spectrum :initarg :spectrum
	     :reader spectrum
	     :documentation "Takes a list of the form '(<spectral class> <spectral decimal> <size category>).")
   (surface-orbit)
   (habitable-zone)
   (companion :initarg :companion
	      :reader companion))
  (:documentation 
"Base class to hold a stellar object."))

(defclass primary-star (star)
  ((companion :initarg :companion
	      :reader :companion)
   (original-flux :initarg :flux
		  :reader original-flux
		  :documentation
		  "The original flux rolled on generating the spectral class. Required."
		  )
   (surface-orbit)
   (habitable-zone))
  (:documentation 
"Class to hold a primary star."))

;; Utility methods to roll up spectral class and size for primary
;; stars. Do not call these outside constructors unless you want your
;; code to blow up.
(defmethod spectral-type ((self primary-star))
  (let* ((flux-roll (flux))
	 (spectral-symbol (nth (+ (flux) *flux-shift*) *spectral-type-table*)))
    (values 
     (list spectral-symbol
	   (random 10))
     flux-roll)))

;; Pass spectral class symbol as a parameter. Use a method instead of
;; a function for dispatching.
(defmethod stellar-size ((self primary-star) spectral-symbol)
  (with-slots (spectrum) self
    (nth 
     (+ (flux) *flux-shift*) 
     (getf *size-table* spectral-symbol))))

(defmethod slot-unbound (class (self star) (slot (eql 'spectrum)))
  (with-slots (spectrum original-flux) self
    (multiple-value-bind (partial-spectrum flux-roll) (spectral-type self)
      (let ((reversed-spectrum (reverse partial-spectrum))
	    (size-symbol (stellar-size self (car partial-spectrum))))
	(if (eql size-symbol 'D)
	    (setf (car reversed-spectrum) nil))
	(when flux-roll
	  (setf original-flux flux-roll))
	(setf spectrum
	      (do
	       ((candidate-spectrum))
	       ((valid-spectrum-p candidate-spectrum) candidate-spectrum)
		(setf candidate-spectrum 
		      (nreverse (push size-symbol reversed-spectrum)))))))))

(defmethod slot-unbound (class (self primary-star) (slot (eql 'original-flux)))
  (with-slots (original-flux) self
    (spectrum self)
    original-flux))

(defmethod slot-unbound (class (self star) (slot (eql 'companion)))
  (with-slots (companion) self
    (setf companion
	  (if (>= (flux) 3)
	      (make-instance 'companion :primary self)))))
      
(defclass companion (star)
  ((primary :initarg :primary
	    :reader primary)
   (spectrum :initarg :spectrum
	     :reader spectrum
	     :documentation
	     "Takes a list of the form '(<spectral class> <spectral decimal> <size category>.")
   (surface-orbit))
  (:documentation 
"Class to hold companion stars in a multiple star system."))
  
;; Should we for whatever reason have a companion without a primary,
;; we generate one
(defmethod slot-unbound (class (self companion) (slot (eql 'primary)))
  (let ((parent (make-instance 'primary-star)))
    (setf (slot-value parent 'companion) self)
    (setf (slot-value self 'primary) parent)))

(defmethod spectral-type ((self companion))
  (let ((spectral-symbol (nth 
			  (+ (original-flux (primary self)) 
			     (roll 1 :dm -1) 
			     *flux-shift*) 
	*spectral-type-table*)))
    (values
     (list
      spectral-symbol
      (if (or (eql spectral-symbol 'D) (eql spectral-symbol 'BD))
	  nil
	  (random 10)))
     nil)))

(defmethod stellar-size ((self companion) spectral-symbol)
  (with-slots (spectrum) self
    (if (eql spectral-symbol 'BD)
	nil
	(nth 
	 (+ (original-flux (primary self)) (roll 1 :dm 2)) 
	 (getf *size-table* spectral-symbol)))))
    
(defmethod original-flux ((self companion))
  (original-flux (primary self)))
   
(defun valid-spectrum-p (spectrum-list)
  ;; Function to test if the rolled up spectrum-list is valid (see
  ;; p. 436). Returns the original spectrum-list on true.
  (if
   (or
    ;; Size IV is not possible for K5-K9 and M0-M9
    ;; stars.
    (and (eql (nth 2 spectrum-list) 'IV)
	 (or
	  (and
	   (eql (nth 0 spectrum-list) 'K)
	   (>= (nth 1 spectrum-list) 5))
	  (eql (nth 0 spectrum-list) 'M)))
    ;; Size VI is not possible for A0-A9 and F0-F4
    ;; stars.
    (and (eql (nth 2 spectrum-list) 'VI)
	 (or
	  (and
	   (eql (nth 0 spectrum-list) 'F)
	   (<= (nth 1 spectrum-list) 4))
	  (eql (nth 0 spectrum-list) 'A))))
   nil
   spectrum-list))

(defmethod habitable-zone ((self star))
  (if (slot-boundp self 'habitable-zone)
      (slot-value self 'habitable-zone)
      (progn 
	(setf (slot-value self 'habitable-zone)
	      (with-open-file 
		  (stream (merge-pathnames *data* "habitable-zones"))
		(let ((hz-data (read stream))
		      (size (nth 2 (spectrum self)))
		      (spectral-type (nth 0 (spectrum self)))
		      (spectral-decimal (nth 1 (spectrum self))))
		  (if (eql size 'D)
		      0
		      (progn
			(dolist (hz-values 
				  (getf (getf hz-data size) spectral-type))
			  (if (and
			       (>= spectral-decimal (nth 0 hz-values))
			       (<= spectral-decimal (nth 1 hz-values)))
			      (return (nth 2 hz-values))))))))))))

(defmethod surface-orbit ((self star))
  (with-slots (surface-orbit) self
    (if (slot-boundp self 'surface-orbit)
	surface-orbit
	(progn
	  (setf surface-orbit
		(with-open-file
		    (stream (merge-pathnames *data* "surface-orbits"))
		  (let ((surface-orbits (read stream))
			(size (nth 2 (spectrum self)))
			(spectral-type (nth 0 (spectrum self)))
			(spectral-decimal (nth 1 (spectrum self))))
		    (dolist (surface-orbit-values
			      (getf (getf surface-orbits size) spectral-type))
		      (if (and
			   (>= spectral-decimal (nth 0 surface-orbit-values))
			   (<= spectral-decimal (nth 1 surface-orbit-values)))
			  (return (nth 2 surface-orbit-values)))))))))))

      
