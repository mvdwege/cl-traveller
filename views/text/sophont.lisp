(in-package :traveller)

(defmethod describe-object ((sophont sophont-class) (stream t))
  (with-slots 
	(homeworld native-terrain ecological-niche locomotion
	characteristics characteristic-dice gender-structure)
      sophont
    (if (class-name sophont)
	(format stream "Name: ~a~%" (symbol-to-name (class-name sophont))))
    (format stream "Homeworld: ~a~%Native Terrain: ~a~%Ecological Niche: ~{~a ~}~%Movement mode: ~a~%"
	    (uwp homeworld)
	    (native-terrain sophont) 
	    (mapcar #'symbol-to-name ecological-niche)
	    (symbol-to-name locomotion))
    (mapcar 
     #'(lambda (characteristic dice) 
		(format t "~a:~14T~aD~%" (symbol-to-name characteristic) dice)) 
     characteristics characteristic-dice)
    (format stream "Gender structure: ~a, " gender-structure)
    (format stream "distribution: ~{~a: ~a%~^, ~}~%"
	    (let* ((genders (getf *gender-structures* (gender-structure sophont)))
		   (gender-distribution))
	      (mapc #'(lambda (x) (setf (getf gender-distribution x) 0))
		      genders)
	      (dotimes (gender-pos (length (gender-table sophont)))
		(incf 
		 (getf gender-distribution (nth gender-pos (gender-table sophont))) 
		 (nth gender-pos *frequencies*)))
	      (mapcar #'(lambda (x)
			  (if (getf gender-distribution x)
			      (setf (getf gender-distribution x)
				    (round (* (/ (getf gender-distribution x) 36) 100)))))
		      genders)
	      gender-distribution))))
