;;;; Weapon definitions and GunMaker
(in-package :traveller)

;;; Superclass, used to find categories and dispatch.
(defclass weapon (item) () )

;;; Base classes for categories, used for dispatch.
(defclass artillery (weapon) () )
(defclass long-gun (weapon) () )
(defclass handgun (weapon) () )
(defclass shotgun (weapon) () )
(defclass machinegun (weapon) () )
(defclass projector (weapon) () )
(defclass designator (weapon) () )
(defclass launcher (weapon) () )

(defmacro weapon-type (category (&key code type tl range mass burden h1 d1 hits-v1 price))
  `(defclass ,(intern (string-upcase type)) (,category)
     ((code :initform ,code :allocation :class)
      (tl :initform ,tl :allocation :class)
      (range :initform ,range :allocation :class)
      (mass :initform ,mass :allocation :class)
      (burden :initform ,burden :allocation :class)
      (h1 :initform ,h1 :allocation :class)
      (d1 :initform ,d1 :allocation :class)
      (hits-v1 :initform ,hits-v1 :allocation :class)
      (price :initform ,price :allocation :class))))



  
