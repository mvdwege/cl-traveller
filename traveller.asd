(in-package :asdf)

(defsystem "traveller"
  :components ((:file "common")
	       (:module "systems" 
			:depends-on ("common")
			:components
			((:file "mainworlds")
			 (:file "other-worlds" :depends-on ("mainworlds"))
			 (:file "uwp-attributes")
			 (:file "stars" :depends-on ("other-worlds"))
			 (:file "systems" :depends-on ("stars" "mainworlds" "other-worlds"))
			 (:file "sectors" :depends-on ("systems"))))))

