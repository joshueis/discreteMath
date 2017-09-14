(require 'cl) ; for loop macro
(require 'ment) ; for true and false

(defun for-all (pred domain)
  "for-all is the Universal Quantification of a predicate (pred),
   the proposition that is true if and only if pred is true for all
   items in a universe of discourse (domain).
   This function loops across domain (which has finite size)
   to see if pred is always true.
   If it encounters a single item for which pred is false,
   then the loop short-circuits and returns false from for-all.
   Otherwise for-all returns true."
  (block 'outer-for-all
    (loop for item across domain
          do (if (null (funcall pred item))
                 (return-from 'outer-for-all false)))q
    true))

(defun for-some (pred domain)
  "for-some is the Existential Quantification of a predicate (pred),
   the proposition that is true if and only if pred is true for at
   least one item in a universe of discourse (domain).
   This function loops across domain (which has finite size)
   to see if pred is ever true.
   If it encounters a single item for which pred is true,
   then the loop short-circuits and returns true from for-all.
   Otherwise for-all returns false."
  (block 'outer-for-some
    (loop for item across domain
          do (if (funcall pred item)
                 (return-from 'outer-for-some true)))
    false))

;Author : Israel Carvajal

(defun inner-for-all (pred item domain)
  "This function would implement the Universal Quantification of a predicate
   (pred). The preposition that is true if and only if pred is true for all the
   items in  domain (domain). Pred needs two elements to be evaluated, 
   therefore this function would evaluate each element in domain with item     
   (argument passed). 
   If prep encounters a single item for which pred is false, then the loop 
   would stop and would return false. Otherwise inner-for-all returns true."
  (block 'inner-for-all
    (loop for item2 across domain
	  do (if (null (funcall pred item item2))
		 (return-from 'inner-for-all false)))
    true))

(defun inner-for-some (pred item domain)
  "Inner-for-some function would implement the Universal Quantification 
  'for some' of a predicate (pred). The preposition is true if and only 
  if pred is true for all the items in  domain (domain). Pred needs two 
  elements to be evaluated, therefore this function would evaluate each 
  element in domain with item (argument passed). 
  If prep encounters a single item for which pred is true, then the loop 
  would stop and would return true. Otherwise inner-for-all returns false."
  (block 'inner-for-some
    (loop for item2 across domain
	  do (if(funcall pred item item2)
		 (return-from 'inner-for-some true)))
    false))


(defun for-some-for-some (pred domain domain2)
  "for-some-for-some is the nested Universal Quantification of a predicate 
   (pred, which is a binary function), the preposition that is true if and 
   only if pred is true for at least one item in a universe of discourse(domain)
   when evaluated with another universe of discourse(domain2). 
   If prep encounters a single element for which pred is true, the loop is
   stopped and would return true. Otherwise for some for some returns false"
  (block 'outer-for-some-for-some
    (loop for item across domain
	  do (if (inner-for-some pred item domain2)
		 (return-from 'outer-for-some-for-some true)))
    false))

(defun for-all-for-all (pred domain domain2)
  "for-all-for-all is the nested Universal Quantification of a predicate 
   (pred, which is a binary function), the preposition that is true if and 
   only if pred is true for all items in a universe of discourse(domain)
   when evaluated with another universe of discourse(domain2). 
   If prep encounters a single element for which pred is false, the loop is
   stopped and would return false. Otherwise for some for some returns true"
  (block 'outer-for-all-for-all
    (loop for item across domain
	  do (if (null (inner-for-all pred item domain2))
		 (return-from 'outer-for-all-for-all false)))
    true))

(defun for-some-for-all (pred domain domain2)
  "for-some-for-all is the nested Universal Quantification of a predicate 
   (pred, which is a binary function), the preposition that is true if and 
   only if for all items in a universe of discourse(domain2) there is at 
   least one item in another universe of discourse(domain) for which pred 
   is true. 
   If it encounters a single item in domain2 for which there is not at least
   one item in domain, the loop stops and returns false. 
   Otherwise for some for all returns true"
  (block 'outer-for-some-for-all
    (loop for item across domain
	  do (if (inner-for-all pred item domain2)
		 (return-from 'outer-for-some-for-all true)))
    false))

(defun for-all-for-some (pred domain domain2)
  "for-all-for-some is the nested Universal Quantification of a predicate 
   (pred, which is a binary function), the preposition that is true if and 
   only if for at least one item in a universe of discourse(domain2) all the
   items in another universe of discourse(domain) pred is true. 
   If it encounters a single item in domain2 for which all items in domain 
   pred is true, the loop stops and returns true. 
   Otherwise for some for all returns false"
  (block 'outer-for-all-for-some
    (loop for item across domain
	  do (if (null (inner-for-some pred item domain2))
		 (return-from 'outer-for-all-for-some false)))
    true))


  ;predicates to be tested

;(for-all-for-some 'greater-than [10 20 30 40 5] [1 2 3 4 5])
;(for-all-for-some 'greater-than [1 2 3 4 5] [10 20 30 40 50])

;(for-some-for-all 'greater-than [10 20 30 40 5] [1 2 3 4 5])
;(for-some-for-all 'greater-than [1 2 3 4 5] [10 20 30 40 50])

;(for-some-for-some 'greater-than [1 2 3 4 5] [10 20 30 40 50])
;(for-some-for-some 'greater-than [1 20 3 40 5] [10 2 30 4 50])

;(for-all-for-all 'greater-than [3 4] [5 6])
;(for-all-for-all 'greater-than [10 20 30 40 50] [1 2 3 4 5])


(defun is-there-a-flight (from to)
  "Is-there-a-flight would determine if a flight is available
   with a given origin and destination in a simulated small airline"
  (or (and (eq from 'SFO) (eq to 'LYS)) 
      (and (eq from 'ATL) (eq to 'LYS)) 
      (and (eq from 'NYC) (eq to 'LYS))
      (and (eq from 'TXL) (eq to 'LYS)) ;to lyon
      (and (eq from 'SFO) (eq to 'YQB))
      (and (eq from 'ATL) (eq to 'YQB))
      (and (eq from 'NYC) (eq to 'YQB))
      (and (eq from 'TXL) (eq to 'YQB)); quebec      
      (and (eq from 'SFO) (eq to 'SCL))
      (and (eq from 'ATL) (eq to 'SCL))
      (and (eq from 'NYC) (eq to 'SCL))
      (and (eq from 'TXL) (eq to 'SCL));santiago                                
      (and (eq from 'SFO) (eq to 'UIO))
      (and (eq from 'NYC) (eq to 'UIO))
      (and (eq from 'ATL) (eq to 'UIO))
      (and (eq from 'TXL) (eq to 'UIO));quito
      (and (eq from 'LYS) (eq to 'NYC))
      (and (eq from 'LYS) (eq to 'TXL))
      (and (eq from 'LYS) (eq to 'ATL))
      (and (eq from 'LYS) (eq to 'SFO))
      (and (eq from 'UIO) (eq to 'NYC))
      (and (eq from 'YQB) (eq to 'LYS))
      (and (eq from 'UIO) (eq to 'ATL))
      (and (eq from 'YQB) (eq to 'TXL))
      (and (eq from 'SCL) (eq to 'ATL))
      (and (eq from 'SFO) (eq to 'ATL))
      (and (eq from 'SFO) (eq to 'NYC))
      (and (eq from 'SFO) (eq to 'TXL))
      (and (eq from 'ATL) (eq to 'SFO))
      (and (eq from 'ATL) (eq to 'NYC))
      (and (eq from 'ATL) (eq to 'TXL))
      (and (eq from 'NYC) (eq to 'TXL))
      (and (eq from 'NYC) (eq to 'SFO))
      (and (eq from 'NYC) (eq to 'ATL))
      (and (eq from 'TXL) (eq to 'ATL))
      (and (eq from 'TXL) (eq to 'SFO))                                             
      (and (eq from 'TXL) (eq to 'NYC))                                             
      (and (eq from 'YQB) (eq to 'NYC))))



(defun coolness (&optional args-string)
  (let* ((args (or args-string (getenv "ARGS") "default"))
	 (arg-list (split-string args " " t)))
    (if (zerop (length arg-list))
	(princ (concat
		"To run this program: type 'coolness' followed by"
		" keyword/s corresponding to the\ndiffent possible tests:\n"
		"TAA, FAA, TSS, FSS, TSA, FSA, TAS, FAS\n"))
      (dolist (arg arg-list)
	(print "The following test has been run:\nis-there-a-flight from to")
	(case (intern arg)
	  (TAA (print (concat
		       "FOR ALl FOR ALL with the following domains:\n"
		       "[ATL NYC TXL SFO] [YQB LYS SCL]\n"
		       "Expected result: TRUE\n"
		       "Actual result:   "
		       (if (for-all-for-all 'is-there-a-flight 
					    [ATL NYC TXL SFO] [YQB LYS SCL])
			   "TRUE" "FALSE"))))
	  (FAA (print (concat 
		      "FOR ALl FOR ALL with the following domains:\n"
		      "[ATL NYC UIO SCL] [ATL NYC TXL YQB])\n"
		      "Expected result: FALSE\n"
		      "Actual result:   "
		      (if (for-all-for-all 'is-there-a-flight 
					   [ATL NYC UIO SCL] [ATL NYC TXL YQB])
			  "TRUE" "FALSE"))))
	  (TSS (print (concat
		       "FOR SOME FOR SOME with the following domains:\n"
		       "[ATL NYC UIO SCL] [LYS SFO YQB TXL]\n"
		       "Expected result: TRUE\n"
		       "Actual result:   "
		       (if (for-some-for-some 'is-there-a-flight
					 [ATL NYC UIO SCL] [LYS SFO YQB TXL])
			   "TRUE" "FALSE"))))
	  (FSS (print (concat
		       "For SOME FOR SOME with the following domains\n"
		       "[UIO SCL] [LYS SFO TXL YQB]\n"
		       "Expected result: FALSE\n"
		       "Actual result:   "
		       (if (for-some-for-some 'is-there-a-flight 
					      [UIO SCL] [LYS SFO TXL YQB])
			   "TRUE" "FALSE"))))
	  (TSA (print (concat
		       "For SOME FOR ALL with the following domains\n"
		       "[ATL NYC TXL SFO] [LYS UIO YQB SCL]\n"
		       "Expected result: TRUE\n"
		       "Actual result:   "
		       (if (for-some-for-all 'is-there-a-flight 
					   [ATL NYC TXL SFO] [LYS UIO YQB SCL])
			   "TRUE" "FALSE"))))
	  (FSA (print (concat
		       "FOR SOME FOR ALL with the following domains\n"
		       "[ATL NYC TXL SCL] [LYS UIO YQB SCL]\n"
		       "Expected result: FALSE\n"
		       "Actual result:   "
		       (if (for-all-for-some 'is-there-a-flight 
					   [ATL NYC TXL SCL] [LYS UIO YQB SCL])
			   "TRUE" "FALSE"))))
	  (TAS (print (concat
		       "For ALl FOR SOME with the following domains\n"
		       "[ATL NYC TXL SFO] [LYS UIO SCL]\n"
		       "Expected result: TRUE\n"
		       "Actual result:   "
		       (if (for-all-for-some 'is-there-a-flight 
					     [ATL NYC TXL SFO] [LYS UIO SCL])
			   "TRUE" "FALSE"))))
	  (FAS (print (concat 
		       "For ALl FOR SOME with the following domains\n"
		       "Expected result: FALSE\n"
		       "Actual result:   "
		       (if (for-all-for-some 'is-there-a-flight 
					[ATL NYC TXL LYS] [YQB UIO SCL])
			   "TRUE" "FALSE"))))
	  (otherwise (print "I did not create that one ;-), try again"))))
      t)))

