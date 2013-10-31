(in-package :traveller)
;;; Generic definitions
(defvar *trade-codes*)
(with-open-file (stream (merge-pathnames *data* "trade-codes"))
  (setf *trade-codes* (read stream)))

(defvar *uwp-attributes* '(starport size atmosphere hydrographics population government law tech-level))

;; Worlds, stars and gas giants all derive from 'body in order to have
;; an 'orbits slot and reader.
(defclass body () 
  ((orbits 
    :initform nil
    :reader orbits)))

;;; World class and methods
;;;
;;; The base class 'world is by definition the mainworld of a
;;; system. Other worlds are subclasses or Gas Giants (an entirely
;;; different hierarchy)
(defclass world (body)
  ((name
    :initarg :name
    :reader name)
   (world-type
    :reader world-type)
   (hz-variance
    :reader hz-variance)
   (climate 
    :reader climate)
   (starport
    :initarg :st
    :reader starport)
   (size :initarg :siz 
	 :reader size)
   (atmosphere :initarg :atm
	       :reader atmosphere)
   (hydrographics :initarg :hyd
		  :reader hydrographics)
   (population :initarg :pop
	       :reader population)
   (government :initarg :gov
    :reader government)
   (law :initarg :law
	:reader law)
   (tech-level :initarg :tl)
   (trade-classifications
    :accessor trade-classifications)))

(defmethod slot-unbound (class (w world) (slot (eql 'hz-variance)))
  (setf (slot-value w 'hz-variance) (truncate (/ (flux) 3))))

(defmethod slot-unbound (class (w world) (slot (eql 'climate)))
  (setf (slot-value w 'climate) (nth (+ 1 (hz-variance w)) '("Hot" "Temperate" "Cold"))))

(defmethod slot-unbound (class (w world) (slot (eql 'world-type)))
  (setf (slot-value w 'world-type)
	(let ((flux-roll (flux)))
	  (cond
	    ((or (= flux-roll -5) (= flux-roll -4)) 'far-satellite)
	    ((or (= flux-roll -6) (= flux-roll -3)) 'close-satellite)
	    (t 'planet)))))

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

(defmacro make-world (&key (uwp "*******-*") (world-type `'world))
"Constructor macro for a world object. Pass in a standard Traveller UWP string to set the UWP attribute values; use * in the UWP string to keep attributes unset, they will be lazily evaluated later when you call their reader. 

Defaults to *******-* in order to generate a world with all slots unbound, to either use random generation or other methods to set world attributes."
  (let ((initargs '(:st :siz :atm :hyd :pop :gov :law :tl))
	(attributes (mapcar #'(lambda (x) `(quote ,x)) *uwp-attributes*)))
    `(make-instance ,world-type
		    ,@(mapcan #'(lambda (x y z) 
				  (unless (equal z "*") 
				    (list x `(make-instance ,y :code (to-number ,z))))) initargs attributes (parse-uwp uwp)))))
    
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

;; Mainworld class. Essentially the same as world, but a different
;; type eases dispatching.
(defclass mainworld (world) () )
