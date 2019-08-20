(in-package :traveller)

(defclass skill-type ()
  ((level :initarg :level :accessor level))
  (:documentation "Abstract base class for Skill Groups, Skills and Knowledges"))

(defgeneric gain (skill-type sophont)
  (:documentation "Multi-method, takes a skill-type and a sophont and adds it to the sophont skill list. Signals an error of type 'interaction-required if passed a Skill Group, a Skill containing Knowledges or a Knowledge containing Specializations."))

(defclass skill-group (skill-type)
  ((level :initform nil)
   (skills :initarg :skills :reader skills :allocation :class))
  (:documentation "Sophonts never receive an instance of this class. Instead the dispatch will trigger an 'interaction-required condition. Instances of this class only exist for dispatch purposes"))

(defclass skill (skill-type)
  ((%times-received :initform 0 :accessor times-received)
   (knowledges :initarg :knowledges :reader knowledges)))

(defclass knowledge (skill-type)
  ((specializations :initarg :specializations :reader specializations)))
