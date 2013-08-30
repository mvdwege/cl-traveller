(in-package :traveller)

(defvar *trade-codes*)
(with-open-file (stream (merge-pathnames *data* "trade-codes"))
  (setf *trade-codes* (read stream)))

(defvar *uwp-attributes* '(starport size atmosphere hydrographics population government law tech-level))
  
;;; World class and methods
(defvar world-types
  '(close-satellite
   far-satellite
   planet))

(defclass world (body)
  ((name
    :initarg :name
    :reader name)
   (system :initarg system
	    :accessor system)
   (world-type)
   (hz-variance
    :reader hz-variance)
   (climate)
   (starport
    :initarg :st
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

(defmethod trade-classifications :before ((w world))
  (with-slots 'trade-classifications w
    (unless (slot-boundp w 'trade-classifications)
      (setf trade-classifications (find-trade-codes w)))))

(defmethod uwp ((w world))
  (format nil "~a~{~a~}-~a~{ ~a~}" (to-ehex (starport w))
	  (mapcar #'(lambda (attribute) (to-ehex(funcall attribute w))) 
		  '(size atmosphere hydrographics population government law)) 
	  (tech-level w) (trade-classifications w)))

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
