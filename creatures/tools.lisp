(in-package :traveller)

;; sophont constructor
(defmacro defsophont (&rest initargs)
  `(make-instance 'sophont-class
		  :direct-superclasses (list (find-class 'sophont))
		  ,@initargs))

;;;; Standard sophonts
;;; Humaniti
(setf (find-class 'human)
      (defsophont
	:name 'human
	:genetic-profile "SDEIES"
	:characteristic-dice '(2 2 2 2 2 2)
	:homeworld (make-world :uwp "A877B99-D" :name "Earth")
	:native-terrain 0
	:locomotion 'walker
	:ecological-niche '(omnivore hunter-gatherer)
	:gender-structure 'dual
	:gender-table '(female male female male female male female male female male female)
	:gender-differences '(male (0 0 0 0 0 0) female (0 0 0 0 0 0))
        :life-stages '((infant . 1/2) (child . 2) (teen . 2) (young-adult . 2) (adult . 2) (peak . 2) (mid-life . 2) (senior . 2) (elder . 2) (retirement . 2))))
;;; Aslan
(setf (find-class 'aslan)
      (defsophont
	  :name 'aslan
	:genetic-profile "SDSIES"
	:characteristic-dice '(2 2 2 2 2 2)
	:homeworld (make-world :uwp "A876986-E")
	:native-terrain 1 :locomotion 'walker
	:ecological-niche '(carnivore pouncer)
	:gender-structure 'dual
	:gender-table '(male male male male female female female female female female female)
	:gender-differences '(male (2 0 2 0 0 -3) female (0 0 0 0 0 0))))
;;; K'kree
;; TODO: K'kree have Caste, but caste generation is not finished yet.
(setf (find-class 'k-kree)
      (defsophont
	  :name 'k-kree
	:genetic-profile "SDEIEK"
	:characteristic-dice '(4 2 4 2 2 2)
	:homeworld (make-world :uwp "B863A03-F")
	:native-terrain 0
	:locomotion 'walker
	:ecological-niche '(herbivore grazer)
	:gender-structure 'FMN
	:gender-table '(female male neuter female female neuter male male male neuter male)
	:gender-differences '(male (3 0 3 0 0 0) female (0 0 0 0 0 0) neuter (0 0 0 0 0 0))))
;;; Vargr
(setf (find-class 'vargr)
      (defsophont
	  :name 'vargr
	:genetic-profile "SDVIEC"
	:characteristic-dice '(2 2 3 2 2 2)
	:homeworld (make-world :uwp "A8859B9-F")
	:native-terrain 0
	:locomotion 'walker
	:ecological-niche '(carnivore chaser)
	:gender-structure 'dual
	:gender-table '(female male female male female male female male female male female)
	:gender-differences '(male (1 -1 1 0 0 -0) female (0 0 0 0 0 0))))
