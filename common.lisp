;;;; General utility functions for Traveller. Dice rolls, result tables etc.
(defpackage :traveller
  (:use :common-lisp)
  (:export 
   :roll
   :flux
   :roll-on
   :flux-on
   :world
   :climate
   :starport
   :size
   :atmosphere
   :hydrographics
   :population
   :government
   :law
   :tech-level
   :uwp
   :trade-classifications
   :make-world
   :star
   :primary-star
   :secondary-star
   :habitable-zone))


(in-package :traveller)

;;; Data directory is a subdirectory of the system directory.
(defvar *data* (merge-pathnames 
		#P"data/" 
		(asdf:system-source-directory :traveller)))

;;; Dice frequencies (used to calculate percentages, as in Caste or
;;; Gender frequencies). Based on a 2D roll.
(defvar *frequencies* '(1 2 3 4 5 6 5 4 3 2 1))

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
"Roll n dice and add DM to the end result."
  (+
   (apply #'+
	  (loop repeat n collect
	       (+ 1 (random 6))))
   DM))

(defun flux (&key good bad)
"Roll two dice and subtract one from the other, giving results from -5
to +5 for Flux, 0 to +5 for Good Flux and -5 to 0 for Bad
Flux. takes :good (t|nil) or :bad (t|nil) as arguments. Defaults to
normal Flux."
;; Note on Flux: if both :good and :bad are set to t, Flux will be Good.
  (let ((flux-dice (loop repeat 2 collect (+ 1 (random 6)))))
    (cond
      (good (apply #'- (sort flux-dice #'>)))
      (bad (apply #'- (sort flux-dice #'<)))
      (t (apply #'- flux-dice)))))

(defun flux-on (table &key (shift 5) (dm 0))
  "Roll flux on a list and return the rolled item."
  (let ((lower-bound 0) (upper-bound (- (length table) 1)))
    (nth (max lower-bound (min upper-bound
			       (+ (flux) shift dm))) 
	 table)))

(defun roll-on (table &key (dice 1) (dm 0))
"Roll dice number of dice on a list, and return the list item
indicated by the roll adjusted by dm."
  (let ((lower-bound 0) 
	(upper-bound (- (length table) 1))
	(shift dice))
    (nth (max lower-bound (min upper-bound
			       (- (roll dice :dm dm) shift))) table)))
    
;;; Symbol representation functions. If the input argument is already
;;; of the right type, silently return it, otherwise convert.
(defun name-to-symbol (name-string)
  (if (symbolp name-string)
      name-string
      (intern (substitute #\- #\Space (string-upcase name-string)))))

(defun symbol-to-name (symbol)
  (if (stringp symbol)
      symbol
      (string-capitalize (substitute #\Space #\- (symbol-name symbol)))))
