;;;; Generate stars and star systems
(in-package :traveller)

(defclass star (body) 
  ((spectrum :initform nil
	    :initarg :spectrum
	    :reader spectrum
	    :documentation 
"Takes a list of the form 
'(<spectral class> <spectral decimal> <size category>.")
   (flux :initform nil
	 :initarg :flux
	 :documentation 
"The original flux rolled on generating the spectral class. Required
if this is a primary star object, otherwise may be left nil.")
   (primary :initform nil
	    :initarg :primary
	    :reader primary)
   (surface-orbit)
   (habitable-zone)
   (companion :initarg :companion))
  (:documentation 
"Class to hold a stellar object. Like all classes in this package,
slots left unbound in make-instance will be filled with randomly
generated values as soon as they are read the first time."))

(defmethod random-spectrum ((self star))
"Generates a random spectrum and saves the spectrum list and the Flux
roll to itself. Note that this mutates the object!"
  (setf (slot-value self 'spectrum)
	;; Define a function to test if the rolled up spectrum-list is
	;; valid (see p. 436).
	(flet ((valid-spectrum-p (spectrum-list)
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
		  spectrum-list)))
	  ;; Repeat generation until a valid spectrum is
	  ;; generated. Usually the first iteration is valid anyway,
	  ;; so no big deal to just do a brute force loop.
	  (do ((sp-list))
	      ((valid-spectrum-p sp-list) sp-list)
	    (setf sp-list
		  ;; TODO Move the content of these bindings into a file?
		  (let* ((flux-shift 6)
			 (spectral-class '(OB A A F F G G K K M M M BD BD BD))
			 (spectral-decimal (random 10))
			 (size 
			  '(O (Ia Ia Ib II III III III V V V IV D IV IV IV)
			    B (Ia Ia Ib II III III III III V V IV D IV IV IV)
			    A (Ia Ia Ib II II IV V V V V V D V V V)
			    F (II II III IV V V V V V V VI D VI VI VI)
			    G (II II III IV V V V V V V VI D VI VI VI)
			    K (II II III IV V V V V V V VI D VI VI VI)
			    M (II II II II III V V V V VI D VI VI VI)))
			 ;; Spectrum and Size are rolled up differently
			 ;; depending on whether we're a primary star
			 ;; or not. So for each we bind a function with
			 ;; a different body depending on our status in
			 ;; the star system.
			 (spectrum-function 
			  (if (primary self)
			      (lambda () 
				(+ 
				 (if (slot-value (primary self) 'flux) 
				     (slot-value (primary self) 'flux)
				     (progn 
				       (random-spectrum (primary self))
				       (slot-value (primary self) 'flux)))
				 (roll 1 :dm -1) flux-shift))
			     (lambda () 
			       (+ (setf (slot-value self 'flux) (flux)) 
				  flux-shift))))
			(size-function
			 (if (primary self)
			     (lambda () 
			       (+ 
				(slot-value (primary self) 'flux) 
				(roll 1 :dm 2) flux-shift))
			     (lambda () (+ (flux) flux-shift)))))
		    ;; Main body. We blithely overwrite the bindings
		    ;; of spectral-class and size, as we don't need
		    ;; the lists anymore after we're done randomly
		    ;; choosing from them.
		    (setf spectral-class 
			  (nth (funcall spectrum-function) 
			       spectral-class))
		    (setf size 
			  (nth (funcall size-function) 
			       (getf size spectral-class)))
		    (when (eql spectral-class 'BD)
		      (setf size nil)
		      (setf spectral-decimal nil))
		    (when (eql size 'D)
		      (setf spectral-decimal nil))
		    (list spectral-class spectral-decimal size)))))))

(defmacro with-spectrum (star &body body) 
  `(progn
     (unless (slot-boundp ,star 'spectrum) 
       (setf (slot-value ,star 'spectrum) (random-spectrum ,star)))
     ,@body))

(defmethod habitable-zone ((self star))
  (with-spectrum self
    (if (slot-boundp self 'habitable-zone)
	(slot-value self 'habitable-zone)
	(progn 
	  (setf (slot-value self 'habitable-zone)
		(with-open-file 
		    (stream (merge-pathnames *data* "habitable-zones"))
		  (let ((hz-data (read stream))
			(size (nth 2 (spectrum self)))
			(spectral-class (nth 0 (spectrum self)))
			(spectral-decimal (nth 1 (spectrum self))))
		    (if (eql size 'D)
			0
			(progn
			  (dolist (hz-values 
				    (getf (getf hz-data size) spectral-class))
			    (if (and
				 (>= spectral-decimal (nth 0 hz-values))
				 (<= spectral-decimal (nth 1 hz-values)))
				(return (nth 2 hz-values)))))))))))))

(defmethod surface-orbit ((self star))
  (with-spectrum self
    (with-slots (surface-orbit) self
      (if (slot-boundp self 'surface-orbit)
	  surface-orbit
	  (progn
	    (setf surface-orbit
		  (with-open-file
		      (stream (merge-pathnames *data* "surface-orbits"))
		    (let ((surface-orbits (read stream))
			  (size (nth 2 (spectrum self)))
			  (spectral-class (nth 0 (spectrum self)))
			  (spectral-decimal (nth 1 (spectrum self))))
		      (dolist (surface-orbit-values
				(getf (getf surface-orbits size) spectral-class))
			(print surface-orbit-values)
			(if (and
			     (>= spectral-decimal (nth 0 surface-orbit-values))
			     (<= spectral-decimal (nth 1 surface-orbit-values)))
			    (return (nth 2 surface-orbit-values))))))))))))

(defmethod initialize-instance :after ((self star) &key &allow-other-keys)
  (with-slots (spectrum flux companion primary) self
    (unless (and spectrum flux)
      (random-spectrum self))
    (unless (slot-boundp self 'companion)
      (if (>= (flux) 3)
	  (setf companion (make-instance 'star :companion nil :primary self))
	  (setf companion nil)))))
      
