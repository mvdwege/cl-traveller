;;;; Classes, generators, and support methods for the UWP attributes
;;;; of systems.

(in-package :traveller)

;;; Read global definitions
;; Compiler does not like defvars inside macros.
(defvar *uwp-definitions*)
(with-open-file (stream (merge-pathnames *data* "uwp-attributes"))
  (setf *uwp-definitions* (read stream)))

;;; Generic UWP Attribute
(defclass uwp-attribute () 
  ((code
   :initarg :code
   :reader code)
   (description :reader description
		:initform ""))
  (:documentation 
"Generic class to hold UWP attributes for worlds. Leave 'code
attribute unbound to randomly generate its value on read."))

(defmethod print-object ((u uwp-attribute) stream) 
  (if (slot-boundp u 'code)
      (format stream "~a" (to-ehex (code u)))))

;;; Starport class
(defclass starport (uwp-attribute)
  ((quality)
   (yards)
   (repairs)
   (fuel)
   (downport
    :initform t)
   (highport)
   (bases)))

(defmethod slot-unbound (class (st starport) (slot (eql 'code)))
  (with-slots (code) st
    (setf code
	  (let ((r (roll 2)))
	    (cond
	      ((<= r 3) (to-number "A"))
	      ((<= r 6) (to-number "B"))
	      ((<= r 8) (to-number "C"))
	      ((<= r 9) (to-number "D"))
	      ((<= r 11) (to-number "E"))
	      (t (to-number "X")))))))

(defmethod slot-unbound (class (w world) (slot (eql 'starport)))
  (setf (slot-value w 'starport) (make-instance 'starport)))

(defmethod starport :around ((w world))
  (code (call-next-method)))

;;; Size and supporting methods and classes.
(defclass size (uwp-attribute)
  ())

(defmethod slot-unbound (class (s size) (slot (eql 'code)))
  (setf (slot-value s 'code)
	(let ((siz (roll 2 :DM -2)))
	  (if (= siz 10)
	      (roll 1 :DM 9)
	      siz)))
  (setf (slot-value s 'description) (car (nth (slot-value s 'code) (getf *uwp-definitions* 'size))))
  s)
  
(defmethod slot-unbound (class (w world) (slot (eql 'size)))
  (code (setf (slot-value w 'size) (make-instance 'size))))

(defmethod size :around ((w world))
  (code (call-next-method)))

;; Atmosphere
(defclass atmosphere (uwp-attribute)
  ((effects)))

;; Atmosphere cannot be initialized using slot-unbound, as it needs
;; other attributes from the world class to calculate its initial
;; code. So we use slot-unbound on the world class to initialize the
;; code slot, and then initialize-instance grabs the description.
(defmethod initialize-instance :after  ((a atmosphere) &key)
  (with-slots (code description effects) a
    ;; Only run if slot code is bound
    (if (slot-boundp a 'code)
	(let ((definition 
	       (nth code (getf *uwp-definitions* 'atmosphere))))
	  (setf description 
		(format nil "~{~a~^, ~}" 
			(pop definition)))
	  (setf effects
		(pop definition))))))

(defmethod slot-unbound (class (w world) (slot (eql 'atmosphere)))
  (setf (slot-value w 'atmosphere)
	 (make-instance
	  'atmosphere :code
	  (let ((s (size w)))
	    (if (= s 0)
		0 ; if Size=0, Atmosphere is always 0.
		(min 15 (max 0 (+ (flux) s))))))))
  

(defmethod atmosphere :around ((w world))
  (code (call-next-method)))

;; Hydrograpics 

;; Like atmosphere, hydrograpics cannot generate its value, but it
;; must be generated from the (hydrographics world) method.
(defclass hydrographics (uwp-attribute)
  ())

;; If we get passed a code at initialization, set description
;; accordingly.
(defmethod initialize-instance :after ((h hydrographics) &key)
	   (with-slots (code description) h
	     (if (slot-boundp h 'code)
		 (setf description (nth code (getf *uwp-definitions* 'hydrographics))))))

(defmethod slot-unbound (class (w world) (slot (eql 'hydrographics)))
  (setf (slot-value w 'hydrographics)
	(make-instance
	 'hydrographics :code
	 (if (< (size w) 2)
	     0
	     (min 10
		  (max 0 (- (+ (atmosphere w) (flux))
			    (if (or (< (atmosphere w) 2)
				    (> (atmosphere w) 9))
				4
				0))))))))

(defmethod hydrographics :around ((w world))
  (code (call-next-method)))

;; Population
(defclass population (uwp-attribute) 
  ())

(defmethod slot-unbound (class (w world) (slot (eql 'population)))
  (setf (slot-value w 'population)
	(make-instance
	 'population :code
	 (let ((pop (roll 2 :dm -2)))
	   (if (= pop 10)
	       (roll 1 :dm 9)
	       pop)))))

(defmethod population :around ((w world))
  (code (call-next-method)))

;; Government
(defclass government (uwp-attribute)
  ((long-description :reader long-description)))

(defmethod slot-unbound (class (w world) (slot (eql 'government)))
  (setf (slot-value w 'government)
	(make-instance
	 'government :code
	 (max 0 (min 15 (+ (population w) (flux)))))))

(defmethod government :around ((w world))
  (code (call-next-method)))

;; Law
(defclass law (uwp-attribute)
  ((long-description :reader long-description)))

(defmethod slot-unbound (class (w world) (slot (eql 'law)))
  (setf (slot-value w 'law)
	(make-instance
	 'law :code
	 (max 0 (min 18 (+ (government w) (flux)))))))

(defmethod law :around ((w world))
  (code (call-next-method)))

;; Tech Level
(defgeneric tl-dm (uwp-attribute))

(defmethod tl-dm ((st starport))
  (cond
    ((= (code st) 10) 6)
    ((= (code st) 11) 4)
    ((= (code st) 12) 2)
    ((= (code st) (to-number "X")) -4)
    ((= (code st) 15) 1)
    (t 0)))

(defmethod tl-dm ((s size))
  (cond
    ((<= (code s) 1) 2)
    ((<= (code s) 4) 1)
    (t 0)))

(defmethod tl-dm ((a atmosphere))
  (cond
    ((<= (code a) 3) 1)
    ((>= (code a) 10) 1)
    (t 0)))

(defmethod tl-dm ((h hydrographics))
    (cond
      ((>= (code h) 9) 1)
      ((>= (code h) 10) 2)
      (t 0)))

(defmethod tl-dm ((p population))
    (cond
      ((and (>= (code p) 1) (<= (code p) 5)) 1)
      ((>= (code p) 9) 2)
      ((>= (code p) 10) 4)
      (t 0)))

(defmethod tl-dm ((g government))
    (cond
      ((= (code g) 0) 1)
      ((= (code g) 5) 1)
      ((= (code g) 13) -2)
      (t 0)))

(defclass tech-level (uwp-attribute) () )

(defmethod initialize-instance :after ((tl tech-level) &key)
	   (with-slots (code) tl
	     (setf code (to-number code))))
  
(defmethod tech-level ((w world))
  (with-slots (tech-level) w
    (if (slot-boundp w 'tech-level)
	(code tech-level)
	(setf tech-level 
	      (make-instance 
	       'tech-level :code
	       (max 0
		    (let ((r (roll 1)))
		      (+ r 
			 (apply #'+ 
				(loop for attribute in '(starport 
							 size 
							 atmosphere 
							 hydrographics 
							 population 
							 government) collect 
				     (to-number 
				      (progn
					(funcall 
					 #'tl-dm
					 (slot-value w attribute))))))))))))))

