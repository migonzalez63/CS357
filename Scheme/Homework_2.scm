;; Miguel Gonzalez
;; Homework #2

;; Problem 1
;; Exercise 4.4
;; Define a procedure deepen-1 with parameters ls that wraps a pair of
;; parantheses around each top level item in ls.
;; Test procedure one:
;;         (deepen-1 '(a b c d)) => ((a) (b) (c) (d))
;;         (deepen-1 '((a b) (c (d e) f))) => (((a b)) ((c (d e))) (f))
;;         (deepen-1 '()) => ()

;; Procedure called deepen-l that wraps an extra pair of parantheses around
;; every top leve item. Checks if the list is null, then recursively
;; creates a list of car ls with '() and calls deepen-l on the rest of the
;; the list.
(define deepen-l
  (lambda (ls)
    (if (null? ls) '()
	(cons (cons (car ls) '()) (deepen-l (cdr ls))))))

;; Exercise 4.6
;; Define a procedure insert-left-all with call structure (insert-left-all
;; new old ls) that inserts the item to the left of each occurance of the
;; item old in the list ls.
;; Test your procedure on:
;; (insert-left-all 'z 'a '(a ((b a) ((a (c)))))) => (z a ((b z a) ((z a (c)))))
;; (insert-left-all 'z 'a '(((a)))) => (((z a)))
;; (insert-left-all 'z 'a '()) => ()


;; Defines a procedure called insert-left-all that adds a new item to the left
;; of the old item. If the list is null, it will return the empty list.
;; If car ls is eaqual to the old item, cons the new item to the left of the
;; car ls and recurse through the rest of the list.
;; If the car of the ls is a pair, we will recursively iterate through both
;; the car and the cdr of the ls.
;; If it's not any of that, we will recreate the ls as it was and recurse
;; through the rest of the list.
(define insert-left-all
  (lambda (new old ls)
    (cond ((null? ls) '())
	  ((equal? (car ls) old)
	   (cons new (cons (car ls) 
		       (insert-left-all new old (cdr ls)))))
	  ((pair? (car ls))
	   (cons
	    (insert-left-all new old (car ls))
	    (insert-left-all new old (cdr ls))))
	  (else (cons (car ls) (insert-left-all new old (cdr ls)))))))

;; Exercise 4.10
;; Define a procedure leftmost that takes a nonempty list as its argument and
;; return the leftmost atomoic item in the list.
;; Test your procedure on:
;; (leftmost '((a b) (c (d e)))) => a
;; (leftmost '((((c ((e f) g) h))))) => c
;; (leftmost '(() a)) => ()

;; Defines a procedure called leftmost that returns the leftmost atomic
;; member of the list.
;; If the car of ls is null, we will return the empt list ().
;; If the car of ls is a list, we will recursively call leftmost on it
;; Else, we have found the leftmost item, thus we will return the car of ls
(define leftmost
  (lambda (ls)
    (cond ((null? (car ls)) '())
	  ((pair? (car ls)) (leftmost (car ls)))
	  (else (car ls)))))

;; Exercise 4.11
;; Define a procedure rightmost that takes a nonempty list as its argument and
;; returns the rightmost atomic item in the list.
;; Test you procedure on:
;; (rightmost '((a b) (d (c d (f (g h) i) m n) u) v)) => v
;; (rightmost '((((((b (c)))))))) => c
;; (rightmost '(a ())) => ()

(define rightmost
  (lambda (ls)
    (cond ((null? ls) '())
	  ((symbol? ls) ls)
	  ((null? (cdr ls)) (rightmost (car ls)))
	  (else (rightmost (cdr ls))))))

;; Exercise 4.18
;; Write an iterative version lenght-it of the procedure length that computes
;; the lenght of a list.

;; Defines a procedure called length-it that calculates the length of a given
;; list. length-it is an iterative procedure since it takes in both a list and
;; an accumulator, and adds one to the accumulator during each iteration.
;; length-it has to be called with accumulator = 0 to get proper results.
;; (length-it '() 0) => 0
;; (length-it '(a b c d) 0) => 4
;; (length-it '(a (b c) (d (e (f))) g) 0) => 4
(define length-it
  (lambda (ls acc)
    (if (null? ls)
	acc
	(length-it (cdr ls) (+ 1 acc)))))

;; Exercise 4.19
;; Write an iterative procedure mk-asc-list-of-ints that, for any integer n,
;; produces a list of the integers from 1 to n in ascending order. then
;; write an iterative procedure mk-desk-list-of-ints that, for any integer
;; n, produces a list of integers from n to 1 in descending order

;; Defines a procedure called mk-asc-list that creates a list of ascending
;; values from 1 to n.
;; (mk-asc-list-of-ints 10) => (1 2 3 4 5 6 7 8 9 10)
;; (mk-asc-list-of-ints 0) => ()
(define mk-asc-list-of-ints-it
  (lambda (n ls)
    (if (zero? n)
	ls
	(mk-asc-list-of-ints-it (- n 1) (cons n ls)))))

;; Procedure that calls mk-asc-list-of-ints-it without having the user
;; worry about inputing the empty list as the accumulator
(define mk-asc-list-of-ints
  (lambda (n)
    (mk-asc-list-of-ints-it n '())))

;; Defines a procedure called mk-desc-list-of-ints-it that creates a list of
;; descending valies from n to 1. Works the same as mk-asc-list-of-ints,
;; except that it appends n to the empty list and we must make the n a
;; list to apply appends.
;; (mk-desc-list-of-ints 10) => (10 9 8 7 6 5 4 3 2 1)
;; (mk-desc-list-of-ints 0) => ()

(define mk-desc-list-of-ints-it
  (lambda (n ls)
    (if(zero? n)
       ls
       (mk-desc-list-of-ints-it (- n 1) (append ls (list n))))))


;; Defines a procedure that calls mk-desc-list-of-ints-it without having
;; the user input the empty list.
(define mk-desc-list-of-ints
  (lambda (n)
       (mk-desc-list-of-ints-it n '())))

;; Exercise 4.20
;; Define both recursive and iterative version of a procedre occurs that
;; counts the number of time an item occurs at the top level in a list.
;; Call the iterative version occurs-it. Test your procedure by counting how
;; many times the item a occurs at top level in each of the following lists:
;;                          (a b a c a d)
;;                          (b c a (b a) c a)
;;                          (b (c d))

;; Defines a recursive procedure called occurs that counts the number of
;; times an item appears in a list.
;; (occurs 'a '(a b a c a d)) => 3
;; (occurs 'a '(b c a (b a) c a) => 2
;; (occurs 'a '(b (c d))) => 0
(define occurs
  (lambda (item ls)
    (cond ((null? ls) 0)
	  ((equal? item (car ls))
	   (+ 1 (occurs item (cdr ls))))
	  (else (occurs item (cdr ls))))))

;; Defines a iterative procedure called occurs-it that counts the number
;; of times an item appears in a list.
;; (occurs 'a '(a b a c a d)) => 3
;; (occurs 'a '(b c a (b a) c a) => 2
;; (occurs 'a '(b (c d))) => 0
(define occurs-it
  (lambda (item ls acc)
    (cond ((null? ls) acc)
	  ((equal? item (car ls))
	   (occurs-it item (cdr ls) (+ 1 acc)))
	  (else (occurs-it item (cdr ls) acc)))))

;; Problem 2
;; Write a function, calculator, which takes an infix epression and evaluates
;; it. For example:
;;        > (calculator 42)
;;        42
;;        > (calculator '(1 + 2))
;;        3
;;        > (calculator '(1 + (2 * 8)))
;;        17
;;        > (calcultor '((((2 + 3) * 2) / 5) + (17 - 1)))
;;        18

;; Defines a procedure called calculator that takes in a list of infix operations and applies the proper
;; calculation  to the operands. Checks if the list is empty, if it is a number, if it consist of a single item,
;; and then calculates the appropiate operation. Only define the '+', '-', '*', '/'. operands.

(define calculator
  (lambda (ls)
    (cond ((null? ls) 0)
	  ((number? ls) ls)
	  ((null? (cdr ls)) (car ls))
	  ((equal? (cadr ls) '+)
	   (+ (calculator (car ls)) (calculator (caddr ls))))
	  ((equal? (cadr ls) '-)
	   (- (calculator (car ls)) (calculator (caddr ls))))
	  ((equal? (cadr ls) '*)
	   (* (calculator (car ls)) (calculator (caddr ls))))
	  ((equal? (cadr ls) '/)
	   (/ (calculator (car ls)) (calculator (caddr ls)))))))

;; Problem 3
;; Write a functions, infix->prefix, which takes an infix arethmetic expression and
;; returns the correspondin prefix expression.
;; Test procedure on:
;;                  (infix->prefix 42) => 42
;;                  (infix->prefix '(1 + 2)) => (+ 1 2)
;;                  (infix->prefix '(1 + (2 * 8))) => (+ 1 (* 2 8))
;;                  (infix->prefix '((((2 + 3) * 2) / 5) + (17 - 1))) => (+ (/ (* (+ 2 3) 2) 5) (- 17 1))

;; Defines a procedure called infix->prefix that turns any infix expression to prefix.
;; Procedure checks if list is empty or if list is singleton and returns the appropiate values.
;; Then it will cons the operation symbol to the operands it will be executed on.
(define infix->prefix
  (lambda (ls)
    (cond ((null? ls) '())
	  ((number? ls) ls)
	  ((null? (cdr ls)) (car ls))
	  ((symbol? (cadr ls))
	   (cons (cadr ls) (cons (infix->prefix (car ls)) (cons (infix->prefix (caddr ls)) '())))))))
	   
;; Problem 4
;; Define a function iota-iota that take an integer i as its argument and return a list
;; integers such that:
;;     > (iota-iota 1)
;;       ((1 . 1))
;;     > (iota-iota 2)
;;        ((1 . 1) (1 . 2) (2 . 1) (2 .2))
;;     > (iota-iota 3)
;;       ((1 . 1) (1 . 2) (1 . 3) (2 . 1) (2 . 2) (2 . 3)
;;        (3 . 1) (3 . 2) (3 . 3))

;; Defines a procedure called iota-iota that prints out a list of integers.
(define iota-iota
  (lambda (i)
    (letrec
	((make-pair
	  (lambda (n acc ls)
	    (if (equal? i acc)
		(list (cons n acc))
		(append (list (cons n acc)) (make-pair n (+ acc 1) ls)))))
	 (iota-helper
	  (lambda (n ls)
	    (if (zero? n)
		  ls		
		 (iota-helper (- n 1) (append (make-pair n 1 '()) ls))))))
      (iota-helper i '()))))

;; Problem 5
;; Define a tail-recursive funstion digits->number that takes a list of digits and returns the number
;; represented by those digits. For example,
;;           (digits->number '(7 6 1 5)) => 7615
;;           (digits->number '(3 8 7 4 5)) => 38745
;;           (digits->number '(0 0 0 0 0 0)) => 0

;; Defines a procedure called digits->number that converts a list of integers into its proper intepretation.
(define digits->number
  (lambda (ls)
    (letrec
	((converter
	  (lambda (ls acc)
	    (if (null? ls)
		acc
		(converter (cdr ls) (+ (* acc 10) (car ls)))))))
      (converter ls 0))))

;; Problem 6
;; Write a function, cond->if, which takes a cond expression, and transforms it into a set of nested if
;; expressions. For example:
;; (cond->if '(cond ((> x y) (- x y)) ((< x y) (- y x)) (else 0) => (if (> x y) (- x y) (if (< x y) (- y x) 0))

;;(define cond->if
;;  (lambda (ls)
;;    (letrec
;;	((if-cond
;;	  (lambda (exp)
;	    (map car (cadr exp))))
;;	 (if-body
;;	  (lambda (exp)
;;	    (map cdr (cadr exp))))
;;	 (if-builder
;;	  (lambda (ls1 ls2)
;;	    (cond ((null? ls1) ls2)
;;		(append (list 'if (if-cond ls1) (if-body ls1) (cond->if (cddr;; ls1)) ls2))))))
;;      (if-builder ls '()))))

(define cond->if
  (lambda (ls)
    (cond ((null? ls) ls)
	  (

;; Problem 7
;; Write a tail-recursive function, cos, which takes a number, x, as its
;; argument and returns cos(x). Your function should approximate cos(x) by
;; summing the first 100 terms of the following Taylor series:
;;         cos(x) = x^0/0! - x^2/2! +x^4/4! - x^6/6! + x^8/8! - ...
;; Any helper functions you need should be defined within the body of sin
;; using letrec. Note: There is a good way and a bad way to do this. The
;; good way avoids computing the factorial and the power of x which appear
;; in each term in the series from scratch each time. In other words, do not
;; use or define fact or expt.

;; Defines a procedure called cos-t that takes the first 100 terms of the taylor expansion
;; of cos and sum them up to get a good approximation for the value of cos(x), where x is
;; any real number.
;; sqr-mult defines a procedure that keeps multiplying the denominator by x^2, since thats
;; the common term in the expansion.
;; fact-it calculates the factorial used in the denominator by taking the previous value
;; and multiplying the next two values.
;; cos-taylor takes in the number of steps, current steps, inintial numerator, initial
;; denominator and an accumulator. We will take take the mod 2 of each step in order to
;; properly apply the alternating + and - behavior that the taylor series has.
;; (cos-t 1) => 0.5403023058681398 (exact to vuilt in cos funtion)
;; (cos-t 2) => -0.4161468365471424
;; (cos-t 3) => 1.000000000000002 (Not exact, but close enough)
;; Note: I named the procedure cos-t since it uses the taylor series expansion and in order
;; to not kill the built in cos procedure, in case user might want to use as comparison

(define cos-t
  (lambda (x)
    (letrec
	((sqr-mult
	  (lambda (acc)
	    (* (* x x) acc)))
	 (fact-it       
	  (lambda (n acc)
 	    (if (= 0 n)
		2
		(* (* (sub1 n) n) acc))))
	 (cos-taylor
	  (lambda (n i num dem acc)
	    (cond ((> i n) acc)
		  ((= 1 i) (cos-taylor n (add1 i) num (fact-it (* i 2) dem) (add1 acc)))
		  ((= 1 (modulo i 2))
		   (cos-taylor n (add1 i) (sqr-mult num) (fact-it (* i 2) dem) (+ acc (/ num dem))))
		  (else (cos-taylor n (add1 i) (sqr-mult num) (fact-it (* i 2) dem) (- acc (/ num dem))))))))
      (exact->inexact (cos-taylor 100 1 (* x x) 1 0)))))

