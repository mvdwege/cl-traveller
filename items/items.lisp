;;;; Base classes and utility functions for items.
"All items in Traveller (or for that matter any RPG) form a dual hierarchy. The first is the item class, which describes its function, it's designed Quality, Reliability, Ease of Use, Burden and Safety parameters, e.g. an LSP Tablet Computer. Then, once we have the class, the instance of that class, the actual object, will be the actual object in play; notabley its QREBS values may differ from the factory defaults.

Basic use is to define a metaclass derived from item-class for each type of equipment, and then create a subclass of item using that class as metaclass.

The basic 'item superclass assumes QREBS 50000. If the Referee has set other defaults in the item-class definition, any attempt to call QREBS related methods on the item object will trigger an inspection, setting the actual values to what the metaclass defines. If the metaclass supplies no definition, aka its slots are unbound, QREBS will be randomly generated"

(in-package :traveller)

(defclass item-class (standard-class)
  ((quality :accessor quality :initarg :quality :type integer)
   (reliability :accessor reliability :initarg :reliability :type integer)
   (ease-of-use :accessor ease-of-use :initarg :ease-of-use :type integer)
   (burden :accessor burden :initarg :burden :type integer)
   (safety :accessor safe :initarg :safety :type integer)))

(defmethod closer-mop:validate-superclass ((class item-class) (super standard-class))
  t)

(defclass item-descriptor ()
  ((descriptor :initarg :descriptor :reader descriptor)
   (code :initarg :code :reader code))
  (:documentation "A generic descriptor class. Subclass this for specific item types (Weapons, Armor, Vehicles, Ship components) and specific descriptors (Bulk, Stage, Type, Subtype etc.)."))

;;; Item base class, with slots for QREBS (pp. 190-196). Safety is
;;; a shadowed symbol.
(defclass item ()
  ((quality :accessor quality :initform 5 :type integer)
   (reliability :accessor reliability :initform 0 :type integer)
   (ease-of-use :accessor ease-of-use :initform 0 :type integer)
   (burden :accessor burden :initform 0 :type integer)
   (safety :accessor safe :initform 0 :type integer))
  (:metaclass item-class))
