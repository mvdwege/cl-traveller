(in-package :traveller)
;;; Generic definitions
;; Compiler does not like defvars inside macros.
(defvar *trade-codes*)
(with-open-file (stream (merge-pathnames *data* "trade-codes"))
  (setf *trade-codes* (read stream)))

(defvar *uwp-attributes* '(starport size atmosphere hydrographics population government law tech-level))

(defvar *uwp-definitions*)
(with-open-file (stream (merge-pathnames *data* "uwp-attributes"))
  (setf *uwp-definitions* (read stream)))

(defclass body () 
  ((orbits 
    :initform nil
    :reader orbits)
   (name
    :initarg :name
    :accessor name))
  (:documentation
   "Worlds, stars and gas giants all derive from 'body in order to
have an 'orbits slot and reader and the applicable utility methods."))

(defmethod last-orbit ((body body))
  (- (length (orbits body)) 1))

;;; World class and methods
;;;
(defclass world (body)
  ((world-type
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
   (tech-level :initarg :tl
               :reader tech-level)
   (trade-classifications
    :accessor trade-classifications))
  (:documentation "World class, holding the main slot and method
definitions"))

(defmethod slot-unbound (class (w world) (slot (eql 'name)))
  (setf (slot-value w 'name) (format nil "~3,'0d-~3,'0d" (random 1000) (random 1000))))

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

(defmethod satellite-p ((w world))
  (or (eq (world-type w) 'far-satellite)
      (eq (world-type w) 'close-satellite)))

(defmethod slot-unbound (class (w world) (slot (eql 'starport)))
  (setf (slot-value w 'starport)
	  (let ((r (roll 2)))
	    (cond
	      ((<= r 3) (to-number "A"))
	      ((<= r 6) (to-number "B"))
	      ((<= r 8) (to-number "C"))
	      ((<= r 9) (to-number "D"))
	      ((<= r 11) (to-number "E"))
	      (t (to-number "X"))))))

;;; Size
(defmethod slot-unbound (class (w world) (slot (eql 'size)))
  (setf (slot-value w 'size)
	(let ((siz (roll 2 :DM -2)))
	  (if (= siz 10)
	      (roll 1 :DM 9)
	      siz))))

;;; Atmosphere
(defmethod slot-unbound (class (w world) (slot (eql 'atmosphere)))
  (setf (slot-value w 'atmosphere)
        (let ((s (size w)))
          (if (= s 0)
              0 ; if Size=0, Atmosphere is always 0.
              (min 15 (max 0 (+ (flux) s)))))))

;;; Hydrographics
(defmethod slot-unbound (class (w world) (slot (eql 'hydrographics)))
  (setf (slot-value w 'hydrographics)
        (if (< (size w) 2)
            0
            (min 10
                 (max 0 (- (+ (atmosphere w) (flux))
                           (if (or (< (atmosphere w) 2)
                                   (> (atmosphere w) 9))
                               4
                               0)))))))

;;; Population
(defmethod slot-unbound (class (w world) (slot (eql 'population)))
  (setf (slot-value w 'population)
        (let ((pop (roll 2 :dm -2)))
	   (if (= pop 10)
	       (roll 1 :dm 9)
	       pop))))

;;; Government
(defmethod slot-unbound (class (w world) (slot (eql 'government)))
  (setf (slot-value w 'government)
        (max 0 (min 15 (+ (population w) (flux))))))

;;; Law
(defmethod slot-unbound (class (w world) (slot (eql 'law)))
  (setf (slot-value w 'law)
        (max 0 (min 18 (+ (government w) (flux))))))

(defmethod slot-unbound (class (w world) (slot (eql 'tech-level)))
  (setf (slot-value w 'tech-level)
        (roll 1 :dm (+ (starport-dm w)
                       (size-dm w)
                       (atm-dm w)
                       (hyd-dm w)
                       (pop-dm w)
                       (gov-dm w)))))

(defmethod starport-dm ((w world))
  (let ((st (to-ehex (starport w))))
    (cond
      ((equalp st "A") 6)
      ((equalp st "B") 4)
      ((equalp st "C") 2)
      ((equalp st "F") 1)
      ((equalp st "X") -4)
      (t 0))))

(defmethod size-dm ((w world))
  (let ((siz (size w)))
    (cond
      ((<= siz 1) 2)
      ((<= siz 4) 1)
      (t 0))))

(defmethod atm-dm ((w world))
  (let ((atm (atmosphere w)))
    (cond
      ((<= atm 3) 1)
      ((>= atm 10) 1)
      (t 0))))

(defmethod hyd-dm ((w world))
  (let ((hyd (hydrographics w)))
    (cond
      ((eql hyd 9) 1)
      ((eql hyd 10) 2)
      (t 0))))

(defmethod pop-dm ((w world))
  (let ((pop (population w)))
    (cond
      ((<= pop 5) 1)
      ((eql pop 9) 2)
      ((eql pop 10) 4)
      (t 0))))

(defmethod gov-dm ((w world))
  (let ((gov (government w)))
    (cond
      ((eql gov 0) 1)
      ((eql gov 5) 1)
      ((eql gov 13) -2)
      (t 0))))

(defmethod trade-classifications :before ((w world))
  (with-slots 'trade-classifications w
    (unless (slot-boundp w 'trade-classifications)
      (setf trade-classifications (find-trade-codes w)))))

(defgeneric uwp (world)
  (:documentation "Returns the UWP code for a world."))

(defmethod uwp ((w world))
  (format nil "~a~{~a~}-~a~{ ~a~}" (to-ehex (starport w))
	  (mapcar #'(lambda (attribute) (to-ehex(funcall attribute w))) 
		  '(size atmosphere hydrographics population government law)) 
	  (tech-level w) (trade-classifications w)))

(defun parse-uwp (uwp-string) 
  (loop for char across (remove #\- uwp-string) collect (string char)))  

(defun make-world (&key (uwp "*******-*") (world-type 'mainworld) name)
"Constructor for a world object. Pass in a standard Traveller UWP
string to set the UWP attribute values; use * in the UWP string to
keep attributes unset, they will be lazily evaluated later when you
call their reader.

Defaults to *******-* in order to generate a world with all slots
unbound, to either use random generation or other methods to set world
attributes."
  (let ((init-args '(:name :st :siz :atm :hyd :pop :gov :law :tl))
	(init-values (append (list name) (mapcar #'to-number (parse-uwp uwp)))))
    (apply #'make-instance world-type
           (loop
              for init-arg in init-args
              for init-value in init-values
              append
                (if init-value (list init-arg init-value))))))

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
  "Returns the default skills for a world"
  )

(defclass mainworld (world) ()
  (:documentation
   "Mainworld class. Essentially the same as world, but a different
type eases dispatching."))

(defclass asteroids (planetoids mainworld) () )

(defmethod trade-classifications :around ((mw mainworld))
  (let ((tc (call-next-method)))
    (if (some #'(lambda (x) (string= x "As")) tc)
	(change-class mw 'asteroids))
    tc))
