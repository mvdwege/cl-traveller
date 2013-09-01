(in-package :traveller)

(defclass system ()
  ((mainworld)
   (primary)
   (orbits :initform (make-array 19 :initial-element nil))))
