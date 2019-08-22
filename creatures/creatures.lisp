;;;; Creature base class
(in-package :traveller)

;;; creature class metaobject
(defclass creature-class (standard-class)
  ((homeworld :initarg :homeworld
          :accessor homeworld)
   (native-terrain :initarg :native-terrain
           :reader native-terrain
           :reader native-terrain-mod)
   (locomotion :initarg :locomotion
               :reader locomotion)
   (niche :initarg :niche
          :accessor niche)
   (ecological-niche :initarg :ecological-niche
             :accessor ecological-niche))
  (:documentation "Creature base class. This is a metaclass object holding the data of the *species*. The way to use it is to randomly generate or build an instance of this class to create a class for the species. Once you have that, instantiations of that class will represent individual specimens of that species"))


;; Keep the compiler happy. Especially SBCL insists on this method
;; existing. We're not doing really deep MOP things, so a simple "Yes,
;; this is supposed to be a subclass of standard-class" will suffice.
(defmethod closer-mop:validate-superclass ((class creature-class) (super standard-class))
  t)

;;; Homeworld generation
(defmethod slot-unbound (class (creature creature-class) (slot (eql 'homeworld)))
  (setf (slot-value creature slot) (make-world)))

;;; Determine Native Terrain, Locomotion, and Ecological Niche.
(defvar *terrain-types*
  '(mountain desert exotic rough-wood rough clear forest wetlands
  wetland-woods ocean ocean-depths))

(defvar *locomotion-types*
  '((walker walker amphib amphib amphib walker walker amphib amphib flyphib aquatic)
    (walker walker walker walker walker walker walker aquatic walker swim diver)
    (walker walker walker walker walker walker walker walker walker swim diver)
    (walker walker walker walker walker walker walker walker walker swim diver)
    (walker walker flyphib walker walker walker walker triphib triphib aquatic diver)
    (flyer flyer flyer flyer flyer walker walker flyer flyphib diver diver)))

(defvar *basic-niche*
  '(producer producer herbivore herbivore omnivore omnivore omnivore
    omnivore omnivore carnivore carnivore scavenger scavenger))

(defvar *ecological-niche*
  '(herbivore (grazer grazer grazer intermittent intermittent
           intermittent intermittent grazer grazer grazer grazer
           grazer filter)
    omnivore (hunter hunter hunter hunter hunter gatherer
          hunter-gatherer gatherer gatherer gatherer gatherer
          gatherer eater)
    carnivore (pouncer pouncer pouncer pouncer pouncer pouncer chaser
           chaser chaser chaser trapper siren killer)
    scavenger (carrion-eater carrion-eater carrion-eater hijacker
           hijacker hijacker intimidator intimidator intimidator
           intimidator intimidator reducer reducer)
    producer (collector collector collector collector collector
          collector basker basker basker basker basker basker basker)))

;; Only four different conditions, so just a few ifs will do, no need
;; to break out method dispatch.
(defmethod terrain-modifiers ((creature creature-class))
  (+
   (if (>= (atmosphere (homeworld creature)) 8)
       -2
       0)
   (if (<= (size (homeworld creature)) 5)
       -1
       0)
   (if (>= (hydrographics (homeworld creature)) 6)
       1
       0)
   (if (>= (hydrographics (homeworld creature)) 9)
       1
       0)))

(defmethod slot-unbound (class (creature creature-class) (slot (eql 'native-terrain)))
  (setf (slot-value creature slot)
    (flux-on *terrain-types* :dm (terrain-modifiers creature))))

(defmethod native-terrain-mod ((creature creature-class))
  (- (position (native-terrain creature) *terrain-types*) 5))

(defmethod slot-unbound (class (creature creature-class) (slot (eql 'locomotion)))
  (let ((row (+ 5 (native-terrain-mod creature)))
        (column (roll 1 :dm (terrain-modifiers creature))))
    (nth row (nth column *locomotion-types*))))

(defmethod slot-unbound (class (creature creature-class) (slot (eql 'niche)))
  (setf (slot-value creature 'niche) (flux-on *basic-niche*)))

(defmethod slot-unbound (class (creature creature-class) (slot (eql 'ecological-niche)))
  (with-slots ((basic-niche niche)) creature
    (if (eq basic-niche 'producer)
        (setf (slot-value creature 'locomotion) 'immobile))
    (setf (slot-value creature slot)
	  (%validate-niche
           (list basic-niche
                 (flux-on
                  (getf *ecological-niche* basic-niche)
                  :dm (native-terrain-mod creature)))))))

;; If plain setting ecological-niche, make sure niche is sane. Should
;; work either as an accessor or with an initarg, so create a helper
;; function which we can call.
(defun %validate-niche (value)
  ;; This condition always returns a list of the format (basic-niche
  ;; ecological-niche), or it throws an error. So as a side effect, we
  ;; can simply set niche to be the car of this list.
  (cond ((atom value)
         (list (find-if
                (lambda (x) (find value (getf *ecological-niche* x)))
                *basic-niche*)
               value))
        ;; if it's not an atom, check if the car is a valid basic
        ;; niche, *and* if the second element (cadr) is a valid
        ;; ecological niche for this basic niche; if yes, return
        ;; value.
        ((and (eql (length value) 2)
              (find (car value) *basic-niche*)
              (find (cadr value) (getf *ecological-niche* (car value))))
         value)
        ;; If neither, throw an error.
        (t (error "Invalid value. Look in *ecological-niche* to see valid values"))))

(defmethod (setf ecological-niche) :around (value (creature creature-class))
  "Just call this method with (setf (ecological-niche creature)
specialized-niche) and let the validation figure out the basic
niche. If you insist you can call it with a list (basic-niche
specialized-niche)"
  (let ((validated-value (%validate-niche value)))
    (setf (niche creature) (car validated-value))
    (call-next-method validated-value creature)))
