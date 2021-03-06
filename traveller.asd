(in-package :asdf)

(defsystem "traveller"
  :depends-on ("closer-mop")
  :components ((:file "common")
	       (:module "systems" 
			:depends-on ("common")
			:components
			((:file "mainworlds")
			 (:file "other-worlds")
			 (:file "stars" :depends-on ("other-worlds"))
			 (:file "systems" :depends-on ("stars" "mainworlds" "other-worlds"))
			 (:file "sectors" :depends-on ("systems"))))
	       (:module "items"
			:depends-on ("common")
			:components
			((:file "items")
			 (:module "weapons" 
				  :depends-on ("items")
				  :components
				  ((:file "weapons")))))
	       (:module "sophonts"
			:depends-on ("systems")
			:components
			((:file "sophonts")
			 (:file "specimen" :depends-on ("sophonts"))
			 (:file "tools" :depends-on ("sophonts" "specimen"))
			 ))))

