(in-package :asdf)

(defsystem "traveller"
  :components ((:file "common")
	       (:module "systems" 
			:depends-on ("common")
			:components 
			((:file "stars")
			 (:file "worlds" :depends-on ("stars"))
			 (:file "systems" :depends-on ("worlds"))))
	       (:module "weapons" 
			:depends-on ("common")
			:components ((:file "weapons")))))
