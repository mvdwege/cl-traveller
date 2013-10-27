(in-package :traveller)
;;;; Generate other world types beyond Mainworlds.

(defclass planetoids (world)
  ((size :initform (make-instance 'size :code 0))
   (atmosphere :initform (make-instance 'atmosphere :code 0))
   (hydrographics :initform (make-instance 'hydrographics :code 0))))
