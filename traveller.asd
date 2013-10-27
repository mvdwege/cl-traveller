(in-package :asdf)

(defsystem "traveller"
  :components ((:file "common")
	       (:module "systems" 
			:depends-on ("common")
			:components
			((:file "mainworlds")
			 (:file "other-worlds" :depends-on ("mainworlds"))
			 (:file "uwp-attributes")
			 (:file "stars")
			 (:file "systems" :depends-on ("stars" "mainworlds" "other-worlds"))
			 (:file "sectors" :depends-on ("systems"))))
	       (:module "weapons" 
			:depends-on ("common")
			:components ((:file "weapons")))))
