;How to write functions

(defun (name (parameters) "comments" body) (name parameters)

;example

(defun averageNum (n1 n2 n3 n4) ( / (+ n1 n2 n3 n4) 4 ))

(averageNum 6 8 56 4) 



(defun area-circle (rad)
 "Calculate area of a circle with a given radius"
(* 3.141593 rad rad))

(area-circle 10)


(setq l '(1 2 3 4 5))
(setq o ' (1 3 5))

(setq u (append l o))
(delete-dups u)
