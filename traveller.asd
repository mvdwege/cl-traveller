(in-package :asdf)

(defsystem "traveller"
  :components ((:file "common")
	       (:module "systems" 
			:depends-on ("common")
			:components
			((:file "worlds")
			 (:file "uwp-attributes")
			 (:file "stars" :depends-on ("worlds"))
			 (:file "systems" :depends-on ("worlds"))))
	       (:module "weapons" 
			:depends-on ("common")
			:components ((:file "weapons")))))
