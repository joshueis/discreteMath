(require 'bigint) ; for most straightforward big integer functions
(require 'calc)   ; for tricky big integer functions
(require 'cl)

(defvar *zero* (bigint-string-to-bigint "0"))
(defvar *one* (bigint-string-to-bigint "1"))
(defvar *two* (bigint-string-to-bigint "2"))
(defvar *three* (bigint-string-to-bigint "3"))
(defvar *twenty-seven* (bigint-string-to-bigint "27"))

;;; For your convenience, there is a string-based version (e.g. abr-string)
;;; of most functions (e.g. abr) that take bigints that does the
;;; bigint-string-to-bigint conversion and then calls the bigint-based version.
;;; Only the bigint-based versions are documented.

(defun abr-string (n b)
  (let* ((base-str (if (numberp b) (number-to-string b)
                     (if (stringp b) b "1")))
         (base (bigint-string-to-bigint base-str))
         (num (if (stringp n) (bigint-string-to-bigint n) n)))
    (abr num base)))

(defun abr (n b)
  "Recursively finds the alternate base representation of n.
   The base b can't be too big."
  (let* ((qr (bigint-divide n b))
         (q (nth 0 qr))
         (r (nth 1 qr)))
    (if (bigint-eq-zero q)
        (list (bigint-to-int r))
      (append (abr q b) (list (bigint-to-int r))))))
(abr (bigint-string-to-bigint "8") (bigint-string-to-bigint "2"))

; TODO Flesh out this function, using abr and *twenty-seven*.
(defun to-base-27 (big-number)
  "This function would find the alternative base representation
   of big-number with '27' being its new base, to simplify the 
   code, we would call 'abr' passing a number and the desired base"
  (mapconcat (lambda (n) (char-to-string (+ n ?@)))
             (abr big-number *twenty-seven*) "")) ;RICK@NEFF

; TODO Flesh out this function, undoing what to-base-27 does.
(defun from-base-27 (message)
  "This function would take a list of the exponents, corresponding to the 
   abr(base 27) of the original message, it would return the original 
   number(it would decript and decode"
  (let* ((big '()))
    (setq big (reverse message))
    (bigint-to-string (from-base big 27))))

(defun from-base-27-string (message)
  "from-base-27 would decode the message that was sent"
  (from-base-27 (map 'list (lambda (n) (- n ?@)) message)))
  
(defun from-base (big-number big-base)
  (if (null big-number)
      *zero*
    (bigint-add (bigint-int-to-bigint (car big-number))
                (bigint-multiply  (bigint-int-to-bigint big-base)
                                  (from-base (cdr big-number) big-base)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gcdr-string (a b)
  (bigint-to-string (gcdr (bigint-string-to-bigint a) (bigint-string-to-bigint b))))

(defun gcdr (a b)
  "Computes the greatest common divisor of bigint a and bigint b recursively.
   Named gcdr to avoid shadowing the built-in gcd function."
  (if (bigint-eq-zero b)
      a
    (let* ((qr (bigint-divide a b))
           (q (nth 0 qr))
           (r (nth 1 qr)))
      (gcdr b r))))

(defun coprime-string (a b)
  (string= "1" (gcdr-string a b)))

(defun coprime (a b)
  "Returns t if bigint a and bigint b are coprime, nil otherwise."
  (bigint-eq-one (gcdr a b)))

(defun pow-mod-string (b n m)
  (bigint-to-string (pow-mod (bigint-string-to-bigint b)
                             (bigint-string-to-bigint n)
                             (bigint-string-to-bigint m))))

(defun pow-mod (b n m)
  "Computes b^n%m (modular exponentiation) with bigints b, n and m."
  (let* ((x *one*)
         (qr (bigint-divide b m))
         (power (nth 1 qr)))
    (while (not (bigint-eq-zero n))
      (setq qr (bigint-divide n *two*)
            n (nth 0 qr))
      (if (bigint-eq-one (nth 1 qr))
          (setq x (bigint-multiply x power)
                qr (bigint-divide x m)
                x (nth 1 qr)))
      (setq power (bigint-multiply power power)
            qr (bigint-divide power m)
            power (nth 1 qr))
      )
    x))

(defun egcd-string (a b)
  (mapcar 'bigint-to-string
          (egcd (bigint-string-to-bigint a) (bigint-string-to-bigint b))))

(defun egcd (a b)
  "Computes the greatest common divisor of bigint a and bigint b recursively.
   Extended version returns a list of the gcd, x and y, where gcd(a, b) = ax + by."
  (if (bigint-eq-zero b)
      (list a *one* *zero*)
    (let* ((qr (bigint-divide a b))
           (q (nth 0 qr))
           (r (nth 1 qr))
           (dxy (egcd b r))
           (d (nth 0 dxy))
           (x (nth 1 dxy))
           (y (nth 2 dxy)))
      (list d y (bigint-subtract x (bigint-multiply q y))))))

(defun find-inverse-string (a m)
  (bigint-to-string (find-inverse (bigint-string-to-bigint a)
                                  (bigint-string-to-bigint m))))

(defun find-inverse (a m)
  "Finds TUMMI (The Unique Modular (mod m) Multiplicative Inverse) of a."
  (let* ((dxy (egcd a m))
         (d (nth 0 dxy))
         (x (nth 1 dxy)))
    (if (not (bigint-eq-one d))
        (error "%s has no inverse mod %s" a m)
      (while (not (bigint-ge-zero x))
        (setq x (bigint-add x m))))
    x))

(defun find-e-string (totient)
  (bigint-to-string (find-e (bigint-string-to-bigint totient))))

(defun find-e (totient)
  (let ((e *three*))
    (while (not (coprime e totient))
      (setq e (bigint-add e *two*)))
    e))

; TODO Improve this function by finding two primes JUST big enough for m.
(defun find-p-and-q (m)
  (let* ((np (calc-eval "nextprime(sqrt($))" nil m))
         (nnp (calc-eval "nextprime($)" nil np)))
    (list np nnp)))

; TODO Document and test the following four functions.
; Note the lack of a separate -string version, as both bigint and
; number-string representations of the message are needed.
(defun rsa-encrypt (message &optional raw)
  "rsa-encrypt would receive 'message' as a parameter. The purpose of the function is to perfom
   the following operation: number^e % n, where number is the encoded message expressed in
   its alternative base 27 representation. 'n'is found by multiplying two primes(p & q) of the 
   same magnitude of number and 'e' needs to be > 1 but < than the totient and be a coprime 
   of n. The totient is the product of p-1 and q-1."
  (let* ((message-as-number-string (if raw message (from-base-27-string message)))
         (message-as-bigint (bigint-string-to-bigint message-as-number-string))
         (p-and-q (find-p-and-q message-as-number-string))
         (p (bigint-string-to-bigint (nth 0 p-and-q)))
         (q (bigint-string-to-bigint (nth 1 p-and-q)))
         (n (bigint-multiply p q))
         (totient (bigint-multiply (bigint-subtract p *one*) (bigint-subtract q *one*)))
         (e (find-e totient))
         (encrypted (pow-mod message-as-bigint e n)))
    (list encrypted p q e)))

(defun rsa-decrypt (encrypted-p-q-e &optional raw)
  "rsa-decrypt what decrypt the message that was given originally to rsa-encrypt.
   The function receives a list of numbers corresponding to the encrypted message,
   p, q and e respectively. It would find the totient of n and its inverse. 
   the final output would be encrypted-message^d % n and this would, which is the 
   original message."
  (let* ((encrypted (nth 0 encrypted-p-q-e))
         (p (nth 1 encrypted-p-q-e))
         (q (nth 2 encrypted-p-q-e))
         (e (nth 3 encrypted-p-q-e))
         (n (bigint-multiply p q))
         (totient (bigint-multiply (bigint-subtract p *one*) (bigint-subtract q *one*)))
         (d (find-inverse e totient))
         (message (pow-mod encrypted d n)))
    (if raw
        (list n totient d message)
      (to-base-27 message))))


(defun round-trip (message-user)
  "round-trip would receive message as a a string, it would replace all spaces with '@'.
   It would set message to be the result of decrypting the encryption of the message.
   The output should always be the original message."
  (setq message (mapconcat #'identity (split-string message-user " ") "@"))
  (string= message-user (rsa-decrypt (rsa-encrypt message))))
(round-trip "RICK NEFF")

(defun test-rsa (&optional arg-string)
  "this function would test if the rsa-encrypt and decrypt functions are working 
   correctly. It would encode and encrypt the message, then it would decrypt and 
   decode the message. The function receives the message from the command line and
   it would print in the terminal the different steps of the process"
  (let* ((message-with-spaces (or arg-string (getenv "ARGS")))
         (message (mapconcat #'identity (split-string message-with-spaces " ") "@"))
         (encrypted (rsa-encrypt message))
         (encrypted-as-strings (mapcar 'bigint-to-string encrypted))
         (raw t)
         (decrypted (rsa-decrypt encrypted raw))
         (decrypted-message (to-base-27 (nth 3 decrypted)))
         (decrypted-message-with-spaces
          (mapconcat #'identity (split-string decrypted-message "@") " "))
         (success (string= message-with-spaces decrypted-message-with-spaces)))
    (setq decrypted (mapcar 'bigint-to-string decrypted))
    (princ (format "\"%s\"\nwas encoded and encrypted as\n%s\nthen decrypted as\n%s\nand decoded as\n\"%s\"\n"
                   message-with-spaces encrypted-as-strings decrypted decrypted-message-with-spaces))
    (princ (if success "" "un"))
    (princ "successfully.\n")
    success))
