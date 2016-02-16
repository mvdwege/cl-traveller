;;;; Base classes and utility functions for items.
(in-package :traveller)

;;; Item base classe, with slots for QREBS (pp. 190-196). Safety is
;;; a shadowed symbol.
(defclass item () 
  ((quality :accessor quality :initform 5 :type integer)
   (reliability :accessor reliability :initform 0 :type integer)
   (ease-of-use :accessor ease-of-use :initform 0 :type integer)
   (burden :accessor burden :initform 0 :type integer)
   (safety :accessor safe :initform 0 :type integer)))

