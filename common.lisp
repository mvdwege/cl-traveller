;;;; General utility functions for Traveller. Dice rolls, result tables etc.
(defpackage :traveller
  (:use :common-lisp)
  (:export :make-world-with-uwp
	   :world
	   :star))

(in-package :traveller)
;;; Data directory is a subdirectory of the system directory.
(defvar *data* (merge-pathnames 
		#P"data/" 
		(asdf:system-source-directory :traveller)))

;;; EHex (extended Hex). The letters I and O are omitted to avoid
;;; confusion with the numbers 1 and 0. Thus A-H translates to (ASCII
;;; Code - 55), J-N to (ASCII Code - 56), and P-Z to (ASCII Code -
;;; 57).
(defun to-ehex (num)
  (cond
    ((<= num 9)
     (write-to-string num))
    ((<= num 17)
     (string (code-char (+ num 55))))
    ((<= num 22)
     (string (code-char (+ num 56))))
    ((<= num 33)
     (string (code-char (+ num 57))))
    (t
     (error "Ehex Digit out of range"))))

(defun to-number (hex)
  ;; If we get an unquoted number, return it.
  (when (numberp hex) (return-from to-number hex))
  ;; If the Ehex digit is 0-9, just return it.
  (let ((num (parse-integer hex :junk-allowed t)))
    (when num
      (return-from to-number num)))
  ;; Else, parse it and return the number.
  (let ((num (- (char-code (coerce (string-upcase hex) 'character)) 55)))
    (cond
      ((or (= num 18) (= num 24))
       (error "Illegal digit"))
      ((>= num 25)
       (- num 2))
      ((>= num 19)
       (- num 1))
      (t num))))
      

;;; Dice rolling mechanics
(defun roll (n &key (DM 0))
  (+
   (apply #'+
	  (loop repeat n collect
	       (+ 1 (random 6))))
   DM))

(defun flux (&key good bad)
;; Note on Flux: if both :good and :bad are set to t, Flux will be Good.
  (let ((flux-dice (loop repeat 2 collect (+ 1 (random 6)))))
    (cond
      (good (apply #'- (sort flux-dice #'>)))
      (bad (apply #'- (sort flux-dice #'<)))
      (t (apply #'- flux-dice)))))
