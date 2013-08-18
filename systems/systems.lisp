(in-package :traveller)

(defclass body ()
  ((orbits)))

(defclass map-object () 
  ((locations)
   (name :initarg :name
	 :accessor name)))

(defclass sector (map-object)
  ((locations :initform (make-array '(4 4)))))

(defclass subsector (map-object)
  ((locations :initform (make-array '(8 10) :initial-element nil))
   (sector :initarg :sector
	   :accessor sector)))

(defgeneric location (map-object coordinates))
(defgeneric (setf location) (map-object coordinates))

(defun generate-subsector (&optional &key name)
  (let ((subsector (make-instance 'subsector :name name)))
    (let ((width (array-dimension (slot-value subsector 'locations) 0)) 
	  (height (array-dimension (slot-value subsector 'locations) 1)))
      (dotimes (x width) 
	   (dotimes (y height)
		(if (> (roll 1) 3)
		    (setf (aref (slot-value subsector 'locations) x y) 
			  (make-instance 'world))))))
    subsector))
