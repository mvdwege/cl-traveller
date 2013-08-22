(in-package :traveller)
;;; Read UWP and Trade Classification definitions.
;; Compiler does not like defvars inside macros.
(defvar *uwp-definitions*)
(with-open-file (stream (merge-pathnames *data* "uwp-attributes"))
  (setf *uwp-definitions* (read stream)))

(defvar *trade-codes*)
(with-open-file (stream (merge-pathnames *data* "trade-codes"))
  (setf *trade-codes* (read stream)))

(defvar *uwp-attributes* '(starport size atmosphere hydrographics population government law tech-level))
  
(defvar world-types
  '(close-satellite
   far-satellite
   planet))

(defclass world (body)
  ((name
    :initarg :name
    :reader name)
   (primary :initarg primary
	    :accessor primary)
   (world-type)
   (hz-variance
    :reader hz-variance)
   (climate)
   (starport
    :initarg :starport
    :reader starport)
   (size :reader size)
   (atmosphere :reader atmosphere)
   (hydrographics :reader hydrographics)
   (population :reader population)
   (government :reader government)
   (law :reader law)
   (tech-level :reader tech-level)
   (trade-classifications
    :accessor trade-classifications)))

;;; UWP Attributes
(defclass uwp-attribute () 
  ((code
   :initarg :code
   :reader code)
   (description :reader description
		:initform "")))

(defmethod print-object ((u uwp-attribute) stream) 
  (format stream "~a" (to-ehex (code u))))

;; Starport class
(defclass starport (uwp-attribute)
  ((quality)
   (yards)
   (repairs)
   (fuel)
   (downport
    :initform t)
   (highport)
   (bases)))

(defmethod initialize-instance :after ((st starport) &key)
	   (if (not (slot-boundp st 'code))
	       (with-slots (code) st
		 (setf code
		       (let ((r (roll 2)))
			 (cond
			   ((<= r 3) (to-number "A"))
			   ((<= r 6) (to-number "B"))
			   ((<= r 8) (to-number "C"))
			   ((<= r 9) (to-number "D"))
			   ((<= r 11) (to-number "E"))
			   (t (to-number "X"))))))))
			   
(defmethod starport :around ((w world))
  (if (slot-boundp w 'starport)
      (to-ehex (code (call-next-method)))
      (to-ehex
       (code (setf (slot-value w 'starport) (make-instance 'starport))))))

;; Size and supporting methods and classes.
(defclass size (uwp-attribute)
  ())

(defmethod initialize-instance :after ((s size) &key)
  (with-slots (code description) s
    (if (not (slot-boundp s 'code))
	(setf code
	      (let ((siz (roll 2 :DM -2)))
		(if (= siz 10)
		    (roll 1 :DM 9)
		    siz))))
    (setf description (car (nth code (getf *uwp-definitions* 'size))))))
  
(defmethod size :around ((w world))
  (if (not (slot-boundp w 'size))
      (code (setf (slot-value w 'size) (make-instance 'size)))
      (code (call-next-method))))

;; Atmosphere
(defclass atmosphere (uwp-attribute)
  ((effects)))

(defmethod initialize-instance :after ((a atmosphere) &key)
	   (with-slots (code description effects) a
	     (if (slot-boundp a 'code)
		 (let ((definition 
			(nth code (getf *uwp-definitions* 'atmosphere))))
		   (setf description 
			 (format nil "~{~a~^, ~}" 
				  (pop definition)))
		   (setf effects
			 (pop definition))))))

(defmethod atmosphere :around ((w world))
  (if (not (slot-boundp w 'atmosphere))
      (code
       (setf (slot-value w 'atmosphere)
	     (make-instance
	      'atmosphere :code
	      (let ((s (size w)))
		(if (= s 0)
		    0 ; if Size=0, Atmosphere is always 0.
		    (min 15 (max 0 (+ (flux) s))))))))
      (code (call-next-method))))

;; Hydrograpics
(defclass hydrographics (uwp-attribute)
  ())

(defmethod initialize-instance :after ((h hydrographics) &key)
	   (with-slots (code description) h
	     (if (slot-boundp h 'code)
		 (setf description (nth code (getf *uwp-definitions* 'hydrographics))))))

(defmethod hydrographics :around ((w world))
  (if (not (slot-boundp w 'hydrographics))
      (code
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
      (code (call-next-method))))

;; Population
(defclass population (uwp-attribute) 
  ())

(defmethod population :around ((w world))
  (if (not (slot-boundp w 'population))
      (code
       (setf (slot-value w 'population)
	     (make-instance
	      'population :code
	      (let ((pop (roll 2 :dm -2)))
		(if (= pop 10)
		    (roll 1 :dm 9)
		    pop)))))
      (code (call-next-method))))

;; Government
(defclass government (uwp-attribute)
  ((long-description :reader long-description)))

(defmethod government :around ((w world))
  (if (not (slot-boundp w 'government))
      (code
       (setf (slot-value w 'government)
	     (make-instance
	      'government :code
	      (max 0 (min 15 (+ (population w) (flux)))))))
      (code (call-next-method))))

;; Law
(defclass law (uwp-attribute)
  ((long-description :reader long-description)))

(defmethod law :around ((w world))
  (if (not (slot-boundp w 'law))
      (code
       (setf (slot-value w 'law)
	     (make-instance
	      'law :code
	      (max 0 (min 18 (+ (government w) (flux)))))))
      (code (call-next-method))))

;; Tech Level
(defgeneric tl-dm (world uwp-attribute))

(defmethod tl-dm ((w world) (st starport))
  (cond
    ((= (code st) 10) 6)
    ((= (code st) 11) 4)
    ((= (code st) 12) 2)
    ((= (code st) (to-number "X")) -4)
    ((= (code st) 15) 1)
    (t 0)))

(defmethod tl-dm ((w world) (s size))
  (cond
    ((<= (code s) 1) 2)
    ((<= (code s) 4) 1)
    (t 0)))

(defmethod tl-dm ((w world) (a atmosphere))
  (cond
    ((<= (code a) 3) 1)
    ((>= (code a) 10) 1)
    (t 0)))

(defmethod tl-dm ((w world) (h hydrographics))
    (cond
      ((>= (code h) 9) 1)
      ((>= (code h) 10) 2)
      (t 0)))

(defmethod tl-dm ((w world) (p population))
    (cond
      ((and (>= (code p) 1) (<= (code p) 5)) 1)
      ((>= (code p) 9) 2)
      ((>= (code p) 10) 4)
      (t 0)))

(defmethod tl-dm ((w world) (g government))
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
					(funcall attribute w)
					(funcall 
					 #'tl-dm w 
					 (slot-value w attribute))))))))))))))

(defmethod trade-classifications :before ((w world))
  (with-slots 'trade-classifications w
    (unless (slot-boundp w 'trade-classifications)
      (setf trade-classifications (find-trade-codes w)))))

(defmethod initialize-instance :after ((w world) 
				       &key st siz atm hyd pop gov law tl) 
  (when (not (slot-boundp w 'hz-variance))
    (setf (slot-value w 'hz-variance) (truncate (/ (flux) 3))))
  (when (not (slot-boundp w 'climate))
    (setf (slot-value w 'climate)
	  (cond
	    ((= (hz-variance w) 0) "Temperate")
	    ((= (hz-variance w) -1) "Hot")
	    ((= (hz-variance w) 1) "Cold"))))
  (when st
    (setf (slot-value w 'starport)
	  (make-instance 'starport :code st)))
  (when siz
    (setf (slot-value w 'size) 
	  (make-instance 'size :code siz)))
  (when atm
    (setf (slot-value w 'atmosphere) 
	  (make-instance 'atmosphere :code atm)))
  (when hyd
    (setf (slot-value w 'hydrographics) 
	  (make-instance 'hydrographics :code hyd)))
  (when pop
    (setf (slot-value w 'population)
	  (make-instance 'population :code pop)))
  (when gov
    (setf (slot-value w 'government)
	  (make-instance 'government :code gov)))
  (when law
    (setf (slot-value w 'law)
	  (make-instance 'law :code law)))
  (when tl
    (setf (slot-value w 'tech-level)
	  (make-instance 'tech-level :code tl))))

(defmethod uwp ((w world))
  (format nil "~a~{~a~}-~a~{ ~a~}" (starport w) (loop for attribute in '(size atmosphere hydrographics population government law) collect (to-ehex(funcall attribute w))) (tech-level w) (trade-classifications w)))

(defun parse-uwp (uwp-string) 
  (loop for char across (remove #\- uwp-string) collect (string char)))  

(defclass planet (world) 
  ((world-type :initform "Planet")) )
(defclass close-satellite (world) 
  ((world-type :initform "Close Satellite")) )
(defclass far-satellite (world) 
  ((world-type :initform "Far Satellite")) )
(defclass hospitable (world) () )
(defclass planetoids (world)
  ((size
    :initform (make-instance 'size :code 0))
   (atmosphere 
    :initform (make-instance 'size :code 0))
   (hydrographics
    :initform (make-instance 'size :code 0))))
(defclass iceworld (world) () )
(defclass radworld (world) () )
(defclass inferno (world) () )
(defclass bigworld (world) () )
(defclass worldlet (world) () )
(defclass inner-world (world) () )
(defclass stormworld (world) () )

(defun make-world-with-uwp (&key name uwp (world-type 'world))
  (destructuring-bind (st siz atm hyd pop gov law tl) 
      (parse-uwp uwp) 
    (make-instance world-type :name name :st st :siz siz :atm atm :hyd hyd :pop pop :gov gov :law law :tl tl)))

(defmethod find-trade-codes ((w world))
  (let ((trade-codes nil)) 
    (loop for trade-code in *trade-codes* do
	 (let ((trade-code (trade-code-applies-p w trade-code)))
	   (when trade-code
	     (push trade-code trade-codes))))
    (setf (trade-classifications w) trade-codes)))
  
(defmethod trade-code-applies-p ((w world) trade-code)
  (when
      (destructuring-bind (st si at hy po go la tl)
	  (loop for attribute in *uwp-attributes* collect
	       (let ((attribute-match (getf trade-code attribute)))
		 (if attribute-match
		     (find (funcall attribute w) attribute-match)
		     t)))
	(and st si at hy po go la tl))
    (getf trade-code 'code)))

(defmethod default-skills ((w world)) 
  )
