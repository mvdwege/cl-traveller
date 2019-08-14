;;;; Weapon definitions and GunMaker
(in-package :traveller)

(defclass weapon-class (item-class)
  ((weapon-type :initarg :type :reader weapon-type)
   (weapon-subtype :initarg :subtype :reader weapon-subtype)
   (descriptor :initarg :descriptor :reader descriptor)
   (weapon-burden :initarg :weapon-burden :reader weapon-burden
                  :documentation "This is the Burden descriptor, not to be confused with the Burden attribute of the QREBS subsystem")
   (stage :initarg :stage :reader stage)
   (users :initarg :users :reader users)
   (portability :initarg :portability :reader portability))
  (:documentation "Use this metaclass to create a weapon class (i.e. a model from a specific manufacturer), then instantiate that class to produce individual weapons"))

(defclass weapon-subtype (item-descriptor)
  ((category :initarg :category :reader category)
   (range :initarg :range :reader range)
   (mass :initarg :mass :reader mass)
   (burden :initarg :burden :reader burden)
   (hits-1 :initarg :h1 :reader h1)
   (damage-dice-1 :initarg :d1 :reader d1)))

(defmethod slot-unbound (class (weapon weapon-class) (slot (eql 'weapon-type)))
  (setf (slot-value weapon slot) (getf (roll-on *weapon-types-table*) :type)))

(defmethod slot-unbound (class (weapon weapon-class) (slot (eql 'weapon-subtype)))
  (let* ((wt (weapon-type weapon))
         (subtypes-table
          (getf (find-if (lambda (x) (eql (getf x :type) wt)) *weapon-types-table*) :subtypes))
         (random-subtype (roll-on subtypes-table))
         (subtype-args (getf *weapon-subtypes-table* random-subtype)))
    (setf (slot-value weapon 'weapon-subtype)
          (apply #'make-instance
                 (append (list 'weapon-subtype :descriptor random-subtype) subtype-args)))))

(defmethod slot-unbound (class (weapon weapon-class) (slot (eql 'descriptor)))
  (let* ((wt (weapon-type weapon))
     (descriptor-table
      (getf
       (find-if (lambda (x) (eql (getf x :type) wt)) *weapon-types-table*)
       :descriptors)))
    (setf (slot-value weapon slot) (roll-on descriptor-table))))

;;; Superclass for individual weapon items.
(defclass weapon (item)
  ()
  (:metaclass weapon-class))
