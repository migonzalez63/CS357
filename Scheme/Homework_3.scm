;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Homework 2
;; Miguel Gonzalez
;; March 2, 2019
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part 1

;; Exercise 7.2
;; Use the procedure compose to define a procedure compose3 that takes as arguments three
;; procedures f, g, and h, and returns the composition k such that for each argument
;; x, k(x) = f(g(h(x))).

;; definintion given to us by the book
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; Defines a porcedure that applies three functions to a single value x
;; Ex: ((compose3 add1 add1 add1) 2) => 5
;;     ((compose3 add1 sub1 sub1) 4) => 3
(define compose3
  (lambda (f g h)
    (lambda (x)
      ((compose f (compose g h)) x))))

;; Exercise 7.3
;; Use the unrestricted lambda to define a composition procedure compose-many that forms
;; the composition of arbitrarily many procedures of one argument. Test your procedure on:
;;          ((compose-many add1 add1 add1 add1) 3) => 7
;;          ((compose-many sqrt abs sub1 (lambda (n) (* n n))) 0.6) => 0.8
;;          (let ((f (lambda (n) (if (even? n) (/ n 2) (addl n)))))
;;                ((compose-many f f f f f f) 21))

;; Defines a procedure called compose-many that composes an arbitrary number of procedures to
;; a value of x, defined by the user
(define compose-many
  (lambda proc
    (if (null? proc)
	(lambda (x) x)
	(compose (car args) (apply compose-many (cdr args))))))

;; Exercise 7.6
;; Define a procedure, map-first-two, that works exactly like map except that the procedure
;; argument is always a procedure of twi arguments instead of just one argument. Use the first
;; and second elements of the list as the first pair of arguments to the procedure, then the
;; second and third elements, then the third and fourth elements, ans so on, until the end of
;; the list is reached. If there are fewer than two elements in the list, the empty list is the value.
;; Test your procedure on:
;;               (map-first-two + '(2 3 4 5 7)) => (5 7 9 12)
;;               (map-first-two max '(2 4 3 5 4 1)) => (4 4 5 5 4)

;; Defines a procdure called map-first-two the maps a procedure to the first and second item, the second and
;; third item, and so on, and creates a list with the evaluation of the procedure
(define map-first-two
  (lambda (proc ls)
    (if (null? (cdr ls))
	'()
	(cons (apply proc (car ls) (cadr ls) '()) (map-first-two proc (cdr ls))))))

;; Exercise 7.7
;; Define a procedure, reduce, that has two parameters, proc and ls. The procedure proc takes two
;; arguments. The procedure reduce reduces the list ls by succesively applying this operation:
;; it builds a new list with the first two elements of the preceding list replaced by the value
;; obtaines when the proc is applied to them. When the list is reduced to containing only two
;; elements, the value returned is the value proc applied to these two elements. If the original
;; list ls contains fewr than two elements, an error is reported. Here is how the succesive stages in
;; the reduction look when proc + and ls is (3 5 7 9)
;;                            (3 5 7 9) => (8 7 9) => (15 9) => 24
;; Test your procedure on:
;;           (reduce + '(1 3 5 7 9)) => 25
;;           (reduce max '(2 -4 6 8 3 1)) => 8
;;           (reduce (lambda (x y) (and x y)) '(#t #t #t #t)) => #t

(define reduce
  (lambda (proc ls)
    (cond ((null? (cdr ls))
	   (error "Error:" ls "contains less than 2 items"))
	  ((null? (cddr ls))
	   (apply proc (car ls) (cadr ls) '()))
	  (else
	   (apply proc (car ls) (reduce proc (cdr ls)) '())))))

;;Exercise 7.8
;; Defien a predicate andmap that takes two arguments, a one-argument predicate pred and a list ls.
;; The value returned by andmap is true when pred applied to each of the elements of ls is true. If
;; pres applied to any one of the elements of ls is false, and map return false.
;; Test your predicate on:
;;        (andmap positive? '(3 4 6 9)) => #t
;;        (andmap positive? '(3 -1 4 8)) => #f
;;        (let ((not-null? (compose not null?))) (andmap not-null? '((a b) (c) (c d e)))) => #t

;; Defines a procedure called andmap that applies a one argument predicate to the car of the list and
;; checks if its true or false. If it is true, we will continue to iterate through the cdr of the list
;; else we return false.
(define andmap
  (lambda (pred ls)
    (let ((predicate (apply pred (car ls) '())))
      (cond ((null? (cdr ls)) predicate)
	    ((equal? #t predicate)
	     (andmap pred (cdr ls)))
	    (else #f)))))

;; Exercise 7.12
;; Curry the procedure * to get a procedure curried* and use it to define the procedure times10
;; that multiplies its argument by 10. Test your procedure on:
;;                 ((curried* 25) 5) => 125
;;                 (time 125) => 1250

;; Defines a procedure called curried* that curries the * operator
(define curried*
  (lambda (x)
    (lambda (y)
      (* x y))))

;; Defines a procedure called times10 that multiplies its argument by 10. Implemented using
;; curried*.
(define times10
  (lambda (x)
    ((curried* x) 10)))

;; Exercise 7.18
;; Define a predicate between? that has three number x, y, and z, as parameters and returns true
;; when y is strickly between x and z, that is, when x < y < z. Then define between?-c, a curried
;; version of between?, where each of the procedures has only one parameter. That is, between?-c
;; has the parameter x and returns a procedure that has the parameter y, which in turn returns a
;; procedure with the parameter z, that test whether y is strickly between x and z. Test your
;; procedure on:
;;              (((between?-c 5) 6) 7) => #t
;;              (((between?-c 5) 5) 7) => #f
;;              (((between?-c 5) 4) 7 => #f

;; Defines a predicate called between that checks to see if y is strickly between x and z, i.e. x < y < z
(define between?
  (lambda (x y z)
    (if (and (< x y) (< y z))
	#t
	#f)))

;; Curried version of between?
(define between?-c
  (lambda (x)
    (lambda (y)
      (lambda (z)
	(if (and (< x y) (< y z))
	    #t
	    #f)))))

;; Exercise 7.22
;; In Exercise 3.1, we called a list of numbers an n-tuple. Using flat-recur, define a procedure
;; mult-by-scalar that takes as its argument a number c and returns a procedure that takes
;; as its argument an n-tuple ntpl and multiples each component of ntpl by the number c.
;; Test your procedure on:
;;                  ((mult-by-scalar 3) '(1 -2 3 -4)) => (3 -6 9 -12)
;;                  ((mult-by-scalar 5) '()) => (3 -6 9 -12)

;; Procedure defined in the book
(define flat-recur
  (lambda (seed list-proc)
    (letrec
	((helper
	  (lambda (ls)
	    (if (null? ls)
		seed
		(list-proc (car ls) (helper (cdr ls)))))))
      helper)))


;;Defines a procedure called mult-by-scalar that takes a number c and multiplies it to
;; every top level element in the n-tuple ntpl by using flat-recur
(define mult-by-scalar
  (lambda (c)
      (flat-recur '() (lambda (x y) (cons (* c x) y)))))

;; Exercise 7.30
;; The procedure reverse-all was defined in Program 4.10. Define it using deep-recur.

;; Procedure defined in the book
(define deep-recur
  (lambda (seed item-proc list-proc)
    (letrec
	((helper
	  (lambda (ls)
	    (if (null? ls)
		seed
		(let ((a (car ls)))
		  (if (or (pair? a) (null? a))
		      (list-proc (helper a) (helper (cdr ls)))
		      (item-proc a (helper (cdr ls)))))))))
      helper)))


;; Defines a procedure that takes a list of items and reverses every element on it using deep-recur
;; Test procedure on:
;;            (reverse-all '(((a b c)) d e ((f (g (h)))))) => (((((h) g) f)) e d ((c b a)))
(define reverse-all
    (deep-recur '() (lambda (x y) (append y (list x))) (lambda (x y) (append y (list x)))))

;; Exercie 7.31
;; Define flat-recur using deep-recur

;; Defines a procedure called flat-recur that iterates through every top level item in the list
;; Test procedure on:
;;              ((mult-by-scalar 3) '(1 -2 3 -4)) => (3 -6 9 -12)  <= Use flat-recur as building block

(define flat-recur
  (lambda (seed list-proc)
    (letrec
	((helper
	  (lambda (ls)
	    (if (null? ls)
		seed
		(list-proc (car ls) (helper (cdr ls)))))))
      helper)))

(define flat-recur
  (lambda (seed list-proc)
    (deep-recur seed list-proc (lambda (x y) (append (list x) y))))) 

;; Part 2

;; Problem 1
;; Consider the following three exmaples:
;; Example 1
;;(define fact
;;  (lambda (x)
;;    (letrec
;;	((loop
;;	  (lambda (x acc)
;;	    (if (= x 0)
;;		acc
;;		(loop (sub1 x) (* x acc))))))
;;    (loop x 1))))
;; Example 2
;;(define reverse
;;  (lambda (x)
;;    (letrec
;;	((loop
;;	  (lambda (x acc)
;;	    (if (null? x)
;;		acc
;;		(loop (cdr x) (cons (car x) acc))))))
;;    (loop x ’()))))
;; Example 3
;;(define iota
;;  (lambda (x)
;;    (letrec
;;	((loop
;;	  (lambda (x acc)
;;	    (if (= x 0)
;;		acc
;;		1
;;		(loop (sub1 x) (cons x acc))))))
;;   (loop x ’()))))
;; The higher-order function tail-recur takes the following arguments:
;;     -bpred: a function of x which returns true if the terminating condition is satisfied
;;             and false otherwise
;;     -xproc: a function of x which updates x
;;     -aproc: a function of x and acc which updates acc
;;     -acc0: an initial value for acc
;; and returns a tail recursice function of x.
;;     (a) Give a definition for tail-recur
;;     (b) Use tail-recur to define reverse
;;     (c) Use tail-recur to define iota

;; (a)
;; Defines a procedure tail-recur that abstracts the tail recurions process
(define tail-recur
  (lambda (bpred xproc aproc acc0)
    (letrec
	((helper
	  (lambda (x acc)
	    (if (bpred x)
		acc
		(helper (xproc x) (aproc x acc)))))
	 (helper2
	  (lambda (x)
	    (helper x acc0))))
      helper2)))

;; (b)
;; Defines the procedure reverse that reverses every top level item in a list
;; Test procedure on:
;;           (reverse '(1 2 3)) => (3 2 1)
;;           (reverse '(((1 2) (3 4)) 5 6 (7 (8)))) => ((7 (8)) 6 5((1 2) (3 4)))
(define reverse
    (tail-recur null? cdr (lambda (x y) (cons (car x) y)) '()))

;; (c)
;; Defines the procedure iota that creates a list of numbers up to the specified n
;; Test procedure on:
;;              (iota 10) => (1 2 3 4 5 6 7 8 9 10)
(define iota
    (tail-recur (lambda (x) (= x 0)) sub1 cons '()))

;; Problem 2
;; Write a function, disjunction2, which takes two predicates as arguments and returns
;; the predicate which return #t if either predicate does not return #f.
;; For example:
;;         ((disjuntion2 symbol? procedure?) +) => #t
;;         ((disjuntion2 symbol? procedure?) (quote +)) => #t
;;         (filter (disjuntion2 even? (lambda (x) (< x 4))) (iota 8)) => (1 2 3 4 6 8)

(define disjunction2
  (lambda (pred1 pred2)
    (letrec
	((helper
	  (lambda (x)
	    (if (or (pred1 x) (pred2 x))
		#t
		#f))))
      helper)))

;; Problem 3
;; Now write disjuntion, which takes an arbitrary number of predicates as arguments

;; Defines a predicate disjunction that takes any number of predicates as arguments and tests
;; them with one item. Return true if either return #t
;; Test procedure on:
;; ((disjunction symbol? procedure? number?) +) => #t
;; ((disjunction symbol? procedure? number?) 3) => #t
(define disjunction
  (lambda pred
    (letrec
	((helper
	  (lambda (x ls)
	    (cond ((null? ls) #f)
		  (((car ls) x) #t)
		  (else (helper x (cdr ls))))))
	 (helper2
	  (lambda (x)
	    (helper x pred))))
      helper2)))
		
;; Problem 4

;; A matrix can be represented in Scheme as a list of lists: ((1 2) (3 4)). Without using recursion,
;; write a function, matrix-map, which takes a funtion, f, and a matrix A, as arguments and returns
;; the matrix B, consisting of f applied to the elements of A, i.e, Bij = f(Aij)
;; Test procedure on:
;;        (matrix-map (lambda (x) (* x x)) '((1 2) (3 4))) => ((1 4) (9 16))

(define matrix-map
  (lambda (f A)
    (map cons (map f (map car ls)) (map list (map f (map cadr ls))))))

;; Problem 5
;; Consider the following defnition for fold (called flat-recur in your text):
;;(define fold
;;  (lambda (seed proc)
;;    (letrec
;;	((pattern
;;	  (lambda (ls)
;;	    (if (null? ls)
;;		seed
;;		(proc (car ls)
;;		      (pattern (cdr ls)))))))
;;    pattern)))
;; (a) Use fold to write a function delete-duplicates which deletes all duplicate items from a
;; list. For example,
;; > (delete-duplicates ’(a b a b a b a b))
;;  (a b)
;;> (delete-duplicates ’(1 2 3 4))
;;  (1 2 3 4)
;; >
;; (b) Use fold to write a function assoc which takes an item and a list of pairs as arguments
;; and returns the first pair in the list with a car car which is equal to item. If there is no
;; such pair then assoc should return false. For example,
;; > (assoc ’b ’((a 1) (b 2)))
;; (b 2)
;; > (assoc ’c ’((a 1) (b 2)))
;; #f

(define fold
  (lambda (seed proc)
    (letrec
	((pattern
	  (lambda (ls)
	    (if (null? ls)
		seed
		(proc (car ls)
		      (pattern (cdr ls)))))))
      pattern)))

;; Defines a procedure called delte-duplicates which deletes all duplicate
;; items of a list after the first one.
(define delete-duplicates
    (letrec
	((delete-c
	  (lambda (item)
	    (fold '() (lambda (x y) (if (eq? x item) y (cons x y))))))
	 (delete
	  (lambda (item ls)
	    ((delete-c item) ls))))
    (fold '() (lambda (x y) (cons x (delete x y))))))
	 
;; Defines a procedure called assoc that takes a key and a list and returns the list associates with
;; the key. If no list exist, then #f is returned
(define assoc
  (lambda (key ls)
    ((fold #f (lambda (x y) (if (eq? (car x) key) x y))) ls)))

;; Part 3
;; Using the functions apply, select, map, filter, outer-product, and iota, and without using recursion
;; give definitions for the following functions:

(define iota
  (lambda (n)
    (letrec
	((loop
	  (lambda (m acc)
	    (if (= m 0)
		acc
		(loop (sub1 m) (cons m acc))))))
      (loop n '()))))

(define outer-product
  (lambda (proc)
    (lambda (us vs)
      (map (lambda (u)
	     (map (lambda (v) (proc u v)) vs)) us))))

(define select
  (lambda (pred)
    (lambda (ls0 ls1)
      (map cdr
	   (filter (lambda (x) (pred (car x))) (map cons ls0 ls1))))))

;; 1. lenght- returnd the length of a list

;; (lenght '(5 8 7 4 6 2 1 8)) => 8
;; (length '(a b d g e r t s l q)) => 10
(define length
  (lambda (ls)
    (apply + (map car (map (lambda (x) (cons 1 x)) ls)))))

;; 2. sum-of-squares - returns the sum of the squares of its arguments

;; (sum-of-square 1 3 4 5 6 8 7) => 200
(define sum-of-squares
  (lambda args
    (apply + (map car (map (lambda (x) (cons (* x x) x)) args)))))

;; 3. avg - returns the average of its arguments

;; (avg 1 2 3 4 5 6 7 8 9) => 5
(define avg
  (lambda args
    (/ (apply + args) (length args))))

;; 4. avg-odd - returns the average of its odd arguments

;; (avg-odd 1 2 3 5 4 6 7 8 9) => 24/5
(define avg-odd
  (lambda args
    (let ((odds ((select odd?) (iota (length args)) args)))
      (/ (apply + odds) (length odds)))))

;; 5. shortest - returns the shortest of its list arguments


;; 6. avg-fact - returns the average of the factorials of its arguments

;; (avg-fact 2 5 8 7) => 22741/2
(define avg-fact
  (lambda args
    (let ((fact (lambda(x) (apply * (iota x)))))
      (/ (apply + (map fact args)) (length args)))))

;; 7. tally - takes a predicate and a list and returns the number of list
;;    elements which satisfy the predicate

;;((tally even?) (iota 8))
(define tally
  (lambda (pred)
    (lambda (ls)
      (apply + (map (lambda (x) (if (pred x) 1 0)) ls)))))

;; 8. list-ref - takes a list and an integer, n, and returns the
;;    n-th element of the list

;; (list-ref 3 '(a b c d e f g)) => '(c)
;; (list-ref 6 '(a b c d e f g)) => '(f)
(define list-ref
  (lambda (n ls)
    ((select (lambda (x) (= x n))) (iota (length ls)) ls)))
