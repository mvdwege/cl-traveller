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
   (primary)
   (orbit)
   (world-type)
   (hz-variance
    :reader hz-variance)
   (climate)
   (starport
    :initarg :starport
    :reader starport)
   (size)
   (atmosphere)
   (hydrographics)
   (population)
   (government)
   (law)
   (tech-level)
   (trade-classifications
    :accessor trade-classifications)))

;;; UWP Attributes
(defclass uwp-attribute () 
  ((code
   :initarg :code
   :reader code)))

(defmethod initialize-instance :after ((u uwp-attribute) &key)
  (when (slot-boundp u 'code)
    (with-slots (code) u
      (setf code (to-number code)))))

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
			   
(defmethod starport ((w world))
  (if (slot-boundp w 'starport)
      (to-ehex (code (slot-value w 'starport)))
      (to-ehex
       (code (setf (slot-value w 'starport) (make-instance 'starport))))))

;; Size and supporting methods and classes.
(defclass size (uwp-attribute)
  ((diameter :reader diameter)))

(defmethod initialize-instance :after ((s size) &key)
  (with-slots (code diameter) s
    (if (not (slot-boundp s 'code))
	(setf code
	      (let ((siz (roll 2 :DM -2)))
		(if (= siz 10)
		    (roll 1 :DM 9)
		    siz))))
    (setf diameter (car (nth code (getf *uwp-definitions* 'size))))))
  
(defmethod size ((w world))
  (if (slot-boundp w 'size)
      (code (slot-value w 'size))
      (code (setf (slot-value w 'size) (make-instance 'size)))))

;; Atmosphere
(defclass atmosphere (uwp-attribute)
  ((description :reader description)
   (effects)))

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

(defmethod atmosphere ((w world))
  (with-slots (atmosphere) w
    (if (slot-boundp w 'atmosphere)
	(code atmosphere)
	(code
	 (setf atmosphere 
	       (make-instance 
		'atmosphere :code 	 
		(let ((s (size w)))
		  (if (= s 0)
		      0 ; if Size=0, Atmosphere is always 0.
		      (min 15 (max 0 (+ (flux) s)))))))))))
;; Hydrograpics
(defclass hydrographics (uwp-attribute)
  ((description :reader description)))

(defmethod initialize-instance :after ((h hydrographics) &key)
	   (with-slots (code description) h
	     (if (slot-boundp h 'code)
		 (setf description (nth code (getf *uwp-definitions* 'hydrographics))))))

(defmethod hydrographics ((w world))
  (with-slots (hydrographics) w
    (if (slot-boundp w 'hydrographics)
	(code hydrographics)
	(code 
	 (setf hydrographics
	       (make-instance 
		'hydrographics :code
		(if (< (size w) 2)
		    0
		    (min 10
			 (max 0 (- (+ (atmosphere w) (flux))
				   (if (or (< (atmosphere w) 2) 
					   (> (atmosphere w) 9))
				       4
				       0)))))))))))

;; Population
(defclass population (uwp-attribute) 
  ((description :reader description)))

(defmethod population ((w world))
    (with-slots (population) w
      (if (slot-boundp w 'population)
	  (code population)
	  (code
	   (setf population
		 (make-instance
		  'population :code
		  (let ((pop (roll 2 :dm -2)))
		    (if (= pop 10)
			(roll 1 :dm 9)
			pop))))))))

;; Government
(defclass government (uwp-attribute)
  ((short-description :reader short-description)
   (long-description :reader long-description)))

(defmethod government ((w world))
  (with-slots (government) w
    (if (slot-boundp w 'government)
	(code government)
	(code
	 (setf government
	       (make-instance
		'government :code
		(max 0 (min 15 (+ (population w) (flux))))))))))

;; Law
(defclass law (uwp-attribute)
  ((short-description :reader short-description)
   (long-description :reader long-description)))

(defmethod law ((w world))
    (with-slots (law) w
      (if (slot-boundp w 'law)
	  (code law)
	  (code 
	   (setf law
		 (make-instance
		  'law :code
		  (max 0 (min 18 (+ (government w) (flux))))))))))

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
