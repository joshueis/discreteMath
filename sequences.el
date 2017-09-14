(let ((letters "abc"))
  (list (symbol-name 'letters) (symbol-value 'letters) letters))


(let ((letters "abc") (numbers "123"))
  (list (concat letters numbers) (concat numbers letters)))


;;;;;;;;;;;;;;

(let ((a 1)
      (b 2)
      (c 1))
  (list (= 1 2) (= a b) (= ?a ?A) (= a c)))


(loop for i in '(0 1 2 3)
      do (let ((iExp (expt 2 i)))
	   (loop for j in '(0 1 2)
		 do (let ((jExp (expt 3 j)))
		      (loop for k in '(0 1)
			    do (let ((kExp (expt 5 k)))
				 (princ (format "2^%d * 3^%d * 5^%d = "))))))))
(defun is-there-a-flight (from to)
  "Is there a flight would determine if a flight is available
   with a given origin and destination in a small airline"
  (or (and (eq from 'SanFrancisco) (eq to 'Lyon))
      (and (eq from 'Atlanta) (eq to 'Lyon))
      (and (eq from 'NewYork) (eq to 'Lyon))
      (and (eq from 'Berlin) (eq to 'Lyon));to lyon
      (and (eq from 'SanFrancisco) (eq to 'Quebec))
      (and (eq from 'Atlanta) (eq to 'Quebec))
      (and (eq from 'NewYork) (eq to 'Quebec))
      (and (eq from 'Berlin) (eq to 'Quebec)); quebec
      (and (eq from 'SanFrancisco) (eq to 'Santiago))
      (and (eq from 'Atlanta) (eq to 'Santiago))
      (and (eq from 'NewYork) (eq to 'Santiago))
      (and (eq from 'Berlin) (eq to 'Santiago));santiago
      (and (eq from 'SanFrancisco) (eq to 'Quito))
      (and (eq from 'NewYork) (eq to 'Quito))
      (and (eq from 'Atlanta) (eq to 'Quito))
      (and (eq from 'Berlin) (eq to 'Quito));quito
      (and (eq from 'Lyon) (eq to 'NewYork))
      (and (eq from 'Lyon) (eq to 'Berlin))
      (and (eq from 'Lyon) (eq to 'Atlanta))
      (and (eq from 'Lyon) (eq to 'SanFrancisco))
      (and (eq from 'Quito) (eq to 'NewYork))
      (and (eq from 'Quebec) (eq to 'Lyon))
      (and (eq from 'Santiago) (eq to 'Atlanta))
      (and (eq from 'Quebec) (eq to 'NewYork))))
;for all for some all big airports can flight to 
;some (quebec) to all ..for some for all
;for all for some
;


(is-there-a-flight 'Quito 'SanFrancisco)
