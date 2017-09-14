(let (c s)
  (block nested-loops
    (do ((a 1 (1+ a))) ((= a 999))
      (do ((b a (1+ b))) ((= b 999))
        (setf c (sqrt (+ (expt a 2) (expt b 2)))
              s (+ a b c))
        (if (= s 1000)
            (return-from nested-loops (* a b c)))))))


(defun make-sequence (lo hi)
  "display the all the numbers in between lo and hi using a recursive 
   function "
  (if (= lo hi) (list hi)
(cons lo (make-sequence (1+ lo) hi))))

(make-sequence 0 10)

;division theorem /fundamental theorem of division
(defun divide (n d)
  (let ((q (/ n d))
	(r ( % n d)))
    (list q r)))

(divide -18 4)

;map string values to cast them to integers

(map 'list (lambda (n) (- n ?0)) '(?1 ?2 ?3 ?4))

(require 'cl)

(defun gcd (a b)
  (let ((x a) (y b) q r)
    (while (> y 0);use instead (not (zerop y))
      (setq q (/ a b) ; x y 
	    r (% a b) ; x y
	    x y
	    y r))
    x))

(gcd 125 55)
(gcd 98 56)
;with recursion
(defun gcdr (a b)
  (if (zerop b) a 
    (gcdr b (% a b))))
(gcdr 125 55)
(gcdr 98 56)
;alternate base representation
(defun abr (n b)
  (let ((d (/ n b))
	(m (% n b)))
    (if (zerop d)
	(list m)
      (append (abr d b) (list m)))))

(abr 13 2)

(defun coprime (a b)
  (= (gcd a b) 1))

(defun coprimes-in
  (let ((a 1) (b 1) (c 0))
    (while (<= a 1000)
      (while (<= b 1000)
	do (if (coprime a b) 
	       (setq c (1+)))
	do (setq b (1+)))
      do (setq a (1+)))))

(coprimes-in)

(defun count-coprime (n)
  (let ((count 0))
    (loop for a from 1 to n
	  do (loop for b from 1 to n
		   do (if (coprime a b)
			  (setq count (1+ count)))))
    count))

(count-coprime 1000)

(defun testPrint (num num2) 
  (if ((> num num2)
	do (princ ("true")))
      princ("false")))
      
(testPrint)

(defun cool ()
  (let* ((args (getenv "ARGS"))
         (args-list (split-string args " " t)))
    (case (length args-list)
      (0 (princ "To run this program: type 'coolness' followed by either one or four\
 arguments")
         (princ "\ncorresponding to the diffent possible tests:\n")
         (princ "TAA, FAA, TSS, FSS, TSA, FSA, TAS, FAS\n"))
      (1 (case (intern (nth 0 args-list))
           (TAA (princ "here\n"))))     ;"the expected result is TRUE"))             
      )))

(require 'cl)
(require 'calc)
(defun from-base (arg)
  (let* ((x 0)
	 (y (reverse arg)))
    (loop for i from 0 to (1- (length y))
	  do (setq x (+ (* (pow 27 i) (nth i y)) x)))
    x))
(setq b '(18 9 3 11 0 14 5 6 6))
(from-base b)

(let* (d (car b)))

(defun from-base-num (num-list base)
  (if (null num-list)
      0
    (+ (car num-list)
       (* base (from-base-num (cdr num-list) base)))))

(from-base-num (reverse '(18 9 3 11 0 14 5 6 6)) 27)


(defun get (message)
  (let* ((x '()))
    (setq x (mapcar (lambda (n) (-  n ?@))
		    (append message nil)))
    x))
(get "RICK@NEFF")
(- (string-to-char "R") ?@)

(defun from-base-27 (message)
  "NOT THE RIGHT bigint as a string"
  (let* ((big '()))
    (setq big (reverse message))
    (from-base-num big 27)))

(from-base-27 b)
"5179195214304"

 (18 9 3 11 0 14 5 6 6))
(mapconcat #'identity (split-string "hello wold" " ") "@")
(map 'list (lambda (n) (- n ?@)) "RICK@NEFF")

(mapcar (lambda (n) (-  n ?@))
	(append '(18 9 3 11 0 14 5 6 6) nil))
(map 'list (lambda (n) (- n ?@)) "RICK@NEFF")

(defun from-base-27 (message)
  "NOT THE RIGHT bigint as a string"
  (let* ((big '()))
    (setq big (mapcar (lambda (n) (-  n ?@))
                      (append message nil)))
    (from-base (reverse big) 27)))

(defun from-base-27-num (message)
  (let* ((big '()))
    (setq big (mapcar (lambda (n) (-  n ?@))
                      (append message nil)))
    (number-to-string (from-base-num (reverse big) 27))))

(mapconcat #'identity (split-string "RICK NEFF" " ") "@")

(from-base-27-string "RICK@NEFF")
"5179195214304"
"5179195214304"
(from-base-27 '(18 9 3 11 0 14 5 6 6))


(setq alist '((1 . "one") (2 . "two") (3 . "three")))
(cdr (assoc 3 alist))

(integerp "d")
(abs 0)

(setq mymap (make-hash-table :test 'equal))
(puthash "one" "red" mymap)
(puthash "two" "blue" mymap)
(puthash "three" "green" mymap)

(maphash (lambda (key val) (print (format "%s : %s \n" key val))) mymap)
