;;;; Weapon definitions and GunMaker
(in-package :traveller)

(defclass weapon-class (item-class)
  ((weapon-type :initarg :type :reader weapon-type)
   (weapon-subtype :initarg :subtype :reader weapon-subtype)
   (weapon-descriptor :initarg :descriptor :reader weapon-descriptor)
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

(defclass weapon-descriptor (item-descriptor)
  ((category :initarg :category :reader category)
   (range :initarg :range :reader range)
   (mass :initarg :mass :reader mass)
   (burden :initarg :burden :reader burden)
   (hits-2 :initarg :h2 :reader h2)
   (damage-dice-2 :initarg :d2 :reader d2)
   (hits-3 :initarg :h3 :reader h3)
   (damage-dice-3 :initarg :d3 :reader d3)))

(defmethod slot-unbound (class (weapon weapon-class) (slot (eql 'weapon-descriptor)))
"If trying to read weapon-descriptor, this will fill it
randomly. Should it run into a random roll that according to GunMaker
T09 has multiple alternatives (like a Poison Dart or regular Dart
Rifle) it will throw an 'interaction-required error, and provides a
store-value restart by default"
  (let* ((wt (weapon-type weapon))
         (descriptor-table
          (getf
           (find-if (lambda (x) (eql (getf x :type) wt)) *weapon-types-table*)
           :descriptors))
         (random-descriptor (roll-on descriptor-table))
         (descriptor-args (getf *weapon-descriptors-table* random-descriptor)))
     (restart-case
        (setf (slot-value weapon 'weapon-descriptor)
              (cond
                ((eql (type-of random-descriptor) 'null) nil)
                ((eql (type-of random-descriptor) 'symbol)
                 (apply #'make-instance
                        (append (list 'weapon-descriptor :descriptor random-descriptor)
                                descriptor-args)))
                ((eql (type-of random-descriptor) 'cons) (error 'interaction-required
                                                      :what 'weapon-descriptor
                                                      :options random-descriptor))))
      (store-value (value)
        :interactive (lambda () (format *query-io* "~%Alternative: ") (list (read *query-io*)))
        ;; If we get here, we need to get our args out of the table
        ;; again, as there is no way to use a cons as a key in a
        ;; plist.
        (setf descriptor-args (getf *weapon-descriptors-table* random-descriptor))
        (setf (slot-value weapon 'weapon-descriptor)
              (apply #'make-instance
                     (append (list 'weapon-descriptor :descriptor value)
                             descriptor-args)))))))

;;; Superclass for individual weapon items.
(defclass weapon (item)
  ()
  (:metaclass weapon-class))
