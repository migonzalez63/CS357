;; Miguel Gonzalez
;; Homework 1
;; January 23, 2019

;; Exercise 1.2
;; Assume that the followinf definitions have been made in the given order:
;;                    (define big-number 105009000)
;;                    (define small-number 0.00000025)
;;                    (define cheshire 'cat)
;;                    (define number1 big-number)
;;                    (define number2 'big-number)
;;
;; What values are returned when the following are enetered in response to the
;; prompt:
;; a. big-number: 105009000, since it is referencing the symbol that is assigned
;;                to the number 105009000.
;; b. small-number: 0.00000025, since it is referencing the symbol that is
;;                  assigned to the number 0.00000025.
;; c. 'big-number: big-number, since the quote tells Scheme to print the literal
;;                 value "big-number".
;; d. cheshire: 'cat, since the symbol "chesire" is assigned to the literal value
;;              "cat".
;; e. 'cheshire: 'cheshire, since the quote tells Scheme to print the literal value
;;               "cheshire"
;; f. number1: 105009000, since the symbol "number1" is assigned to the symbol
;;             big-number, which evaluates to the number 10500900.
;; g. number2: 'big-number, since the symbol "number2" is assigned to the literal
;;             value "big-number".
;; h. 'number1: 'number1, since the quote tells Scheme to print the literal value
;;              "number1".

;; Exercise 1.3
;; What is the result of entering each of the following expressions in response to the
;; Scheme prompt?
;; a. (-10 (- 8 (- 6 4))): 4
;; b. (/ 40 (* 5 20)): 2/5
;; c. (/ 2 3): 2/3
;; d. (+ (* 0.1 20) (/ 4 -3): 2/3
;;
;; Exercise 1.4
;; Write a Scheme expression that denote the same calculation as the folloqing arithmetic expressions.
;; a. (4 x 7) - (13 + 5)
(- (* 4 7) (+ 13 5))
;; b. (3 x (4 + (-5 - -3)))
(* 3 (+ 4 (- -5 -3)))
;; c. (2.5 / (5 x (1 / 10)))
(/ 2.5 (* 5 (/ 1 10)))
;; d. 5 x ((537 x (98.3 + (375 - (2.5 x 153)))) + 255)
(* 5 (+ 255 (* 537 (+ 98.3 (- 375 (* 2.5 153))))))

;; Exercise 1.5
;; If a, b, c are any three numbers, translate each of the following Scheme expressions into the usual
;; arithmetical expressions. (Note: changed alpha, beta and gamma to a, b, and c, respectively.)
;; a. (+ a (- (+ b c) a)) translates to a + ((b + c) - a).
;; b. (+ (* a b) (* c b)) translates to (a + b) + (c * b).
;; c. (/ (- a b) (- a c)) translates to (a-b) / (a-c).

;; Exercise 1.6
;; Using the symbols 'one' and 'two' and the procedore 'cons', we can construct the list "(one two)" by
;; typing "(cons 'one (cons 'two '()))". Using the symbols 'one', 'two', 'three', and 'four' and the
;; constructor 'cons', construct the following list without using quoted lists (you may use quoted
;; symbols and the empty list).
;; a. (one two three four)
(cons 'one (cons 'two (cons 'three (cons 'four '()))))
;; b. (one (two three four))
(cons 'one (cons (cons 'two (cons 'three (cons 'four '()))) '()))
;; c. (one (two three) four)
(cons 'one (cons (cons 'two (cons 'three '())) (cons 'four '())))
;; d. ((one two) (three four))
(cons (cons 'one (cons 'two '())) (cons (cons 'three (cons 'four '())) '()))
;; e. (((one)))
(cons (cons (cons 'one '()) '()) '())

;; Exercise 1.10
;; If the operands a and b are evaluated to any values, what is:
;; a. (symbol? (cons a b)) evaluates to #f since the list (a b) is not a symbol, it is an actual
;;    data value.
;; b. (pair? (cons a b)) evaluates to #t, since a list compromises of the first item, car, and the
;;    rest of the list, cdr, forming a pair.
;; c. (null? (cons a b)) evaluates to #f for any value including the empty list, since using cons
;;    on two empty list creates a list with the empty list inside it, making it not null.
;; d. (null? (cdr (cons a '()))) evaluates to #t since the cdr of this list will always be the
;;    empty list.

;; Exercise 1.14
;; Decide whether the following expressions are true or false:
;; a. (symbol? (car '(cat mouse))) evaluates to #t, since the car of the literal (cat mouse) is a
;;    symbol.
;; b. (symbol? (cdr '((cat mouse)))) evaluates to #f since the cdr of the literal ((cat mouse)) is
;;    the empty list
;;
;; c. (symbol? (cdr '(cat mouse))) evaluates to #f since the cdr of the literal (cat mouse) is
;;    'mouse', which is a symbol.
;; d. (pair? (cons 'hound '(dog))) evaluates to #t since cons constructs a list that contains a car
;;    and a cdr, making it a pair by definintion.
;; e. (pair? (car '(cheshire cat))) evaluates to #f since the car of the literal (cheshire cat) is
;;    chesire, which is a symbol.
;; f. (pair? (cons '() '())) evaluates to #t since cons constructs a list that contains a car and a
;;    cdr, making it a pair by definintion.

;; Exercise 2.1
;; Define a procedure called second that takes as its argument a list and that
;; return the second item in the list. Assume that the list contains at least
;; two items.

;; Defines a function called second that takes a list as an argument. It will
;; then take the cdr of the list, since a list consist of the first item, car,
;; and the rest of the list, cdr. Taking tht cdr will isolate the second item,
;; Thus we can take the car of the cdr to return the second value of the
;; list.
;; (second '(1 2)) => 2
;; (second '(a b)) => b
(define second (lambda (list) (car (cdr list))))

;; Exercise 2.3
;; The procedure firsts-of-both is defined as follows
;;             (define firsts-of-both
;;               (lambda (list-1 list-2)
;;                 (make-list-of-two (car list-1) (car list-2))))
;; Determine the value of the following expressions:
;; a. (firsts-of-both '(1 3 5 7) '(2 4 6)) will produce (1 2), since first-of-both makes a list of two
;;    items, where the two items will be the first values of each list.
;; b. (first-of-both '((a b) (c d)) '((e f) (g h))) will produce ((a b) (e f)), since first-of-both
;;     makes a list of two items, where the two items will be the first values of each list.

;; Exercise 2.4
;; Define a procedure juggle that rotates a three-element list. The procedure juggle returns a list
;; that is a rearrangment of the input list so that the first element becomes the second, the second
;; element becomes the third, and the third element becomes the first. Test your procedure one:
;;                        (juggle '(jump quick spot)) ==> (spot jump quick)
;;                        (juggle '(dog bites man)) ==> (man dog bites)

;; Defines a function juggle that takes a three-item list as an argument. It will construct a new list
;; where the first item will be the car of the cdr of the cdr of the list, which will evaluate to the
;; third item, the car of the list, which will be the first item, and the car of the cdr of the list,
;; which will be the second item.

(define juggle
  (lambda (list)
    (cons (car (cdr (cdr list))) (cons (car list) (cons (car (cdr list)) '())))))

;; Exercise 2.6
;; Assume that a, b, and c are exoressions that evaluate to #t and that e and f are expressions that
;; evaluate to #f. Decide whether the following expression are true or false.
;; a. (and a (or b e)) will evaluate to true, since  b is true, (or b e) will short circuit to true
;;    and a is also true.
;; b. (or e (and (not f) a c)) will evaluate to true since (not f) evaluates to true and both a ans
;;    c evaluate to true, making (and (not f) a c) true.
;; c. (not (or (not a) (not b))) evaluates to true since (not a) and (not b) will both evaluate to
;;    false. (or (not a) (not b)) will evaluate to false as well, but since it is encapsulates in
;;    a not expression, the evaluation will be changed to true.
;; d. (and (or a f) (not (or b e))) will evaluate to false since (or a f) evaluates to true, but
;;    since (or b e) evaluates to true, the nor expression will change it to false, making the and
;;    expression false as well.

;; Exercise 2.7
;; Decide whether the following expression are true or false if expr is some boolean expression.
;; a. (or (symbol? expr) (not (symbol? expr))) will evaluate to true since a boolean expression
;;    is not a symbol, giving us #f for (symbol? expr) but #t for (not (symbol? expr)).
;; b. (and (null? expr) (not (null? expr))) will give us false since boolean expression are not
;;    of void type, making (null? expr) evaluate to #f and short circuit the and expression to #f
;; c. (not (and (or expr #f) (not expr))) will evalute to true since no matter what boolean value
;;     expr evaluates to, (and (or expr #f) (not expr)) will always evaluate to #f, making the
;;     not expression change it to #t.
;; d. (not (or expr #t)) will evlautate to false since no matter what boolean value expr evaluates
;;    to, the #t in (or expr #t) will always yeild true for that expression, making the not
;;    change the value to #f.

;; Exercise 2.10
;; Rewrite the definition of the three procedures last-item, member? and remove-1st with the cond
;; expression replaced by if expressions.

;; Defines a function called last-item that returns the last item of any arbitrary
;; list.
;; (last-itea '(12345)) => 5
;; (last-item '(a b (c d))) => (c d)
;; (last-item '(cat)) => cat
;; (last-item '((cat))) => (cat)

(define last-item
  (lambda (ls)
    (if (null? (cdr ls))
	(car ls)
	(last-item (cdr ls)))))

;; Defines a function called member that checks if an item is a member of the given
;; list.
;; (member? 'cat '(dog hen cat pig)) ==> #t
;; (member? 'fox '(dog hen cat pig)) => #f
;; (member? 2 '(1 (2 3) 4)) => #f
;; (member? ' (2 3) ' (1 (2 3) 4)) => #t
;; (member? 'cat '()) => #f

(define member?
  (lambda (item ls)
    (if (null? ls) #f
	(or (equal? (car ls) item)
	    (member? item (cdr ls))))))

;; Defines a function that removes the first occurence of the given item on the
;; given list.
;; (remove-lst 'fox '(hen fox chick cock)) => (hen chick cock)
;; (remove-lst 'fox '(hen fox chick fox cock)) => (hen chick fox cock)
;; (remove-lst 'fox '(hen (fox chick) cock)) => (hen (fox chick) cock)
;; (remove-lst 'fox '()) => ()
;; (remove-lst '(1 2) '(1 2 (1 2) ((1 2)))) —> (1 2 ((1 2)))

(define remove-1st
  (lambda (item ls)
    (if (null? ls) '()
	(if (equal? (car ls) item) (cdr ls)
	    (cons (car ls) (remove-1st item (cdr ls)))))))

;; Exercise 2.12
;; Thee following procedure, named mystery, takes as its argument a list that contains
;; at least two top-level items.
;;                         (define mystery
;;                           (lambda (ls)
;;                             (if (null? (cdr ls))
;;                                 (cons (car ls) '())
;;                                 (cons (car ls) (mystery (cdr ls))))))
;; What is the value of (mystery '(1 2 3 4 5))? Descrive the general behavior of
;; mystery. Suggest a good name for the procedure mystery.
;;
;; The procedure mystery defines a procedure that takes a list as an argument and
;; checks to see if its cdr is '(). If it is, then it forms a list with the car
;; of the list and return. If it is not, it foms a list with the car of the list
;; and runs the procedure on the cdr of the list. Essentially, what the procedure is
;; doing is that it takes the first item of the list and makes a list that cosists
;; of that item and all subsequent items, effectively just copying the list.
;; The procedure (mystery '(1 2 3 4 5)) will return (1 2 3 4 5), which is the list
;; we inserted as the argument. A good name for this procedure would be "copy".

;; Exercise 2.13
;; Define a procedure subs-1st that takes three parameters: an item new, an item
;; old, and a list of item ls. The procedure subst-1st looks for the first top-level
;; occurrence of the item old in ls and replaces it with the item new. Test your
;; procedure on:
;;
;; (subst-1st 'dog 'cat '(my cat is clever)) => (my dog is clever)
;; (subst-1st 'b 'a '(c a b a c)) => (c b b a c)
;; (subst-1st '(0) '(*) '((*) (1) (*) (2)) => ((0) (1) (*) (2))
;; (subst-1st 'two 'one '()) => ()
;;
;; In order to be able to include list as possible arguments to which the parameters
;; new and old are boun, use equal? to test for sameness. Also define procedures
;; substq-1st and substv-1st that uses eq? and eqv? respectively, instead of equal?
;; to test for sameness.

;; Defines a procedure named subs-1st that subtitutes the first instance of an item
;; in a list with another given item. Since we only need to remove the first instance
;; of the item, we can recursively call our procedure and change the old parameter
;; to the empty list in order to not change other instances of the item in the list.
;; (subst-1st 'dog 'cat '(my cat is clever)) => (my dog is clever)
;; (subst-1st 'b 'a '(c a b a c)) => (c b b a c)
;; (subst-1st '(0) '(*) '((*) (1) (*) (2)) => ((0) (1) (*) (2))
;; (subst-1st 'two 'one '()) => ()
(define subst-1st
  (lambda (new old ls)
    (cond ((null? ls) '())
	  ((equal? (car ls) old) (cons new (cons (cdr ls) '())))
	  (else (cons (car ls) (subst-1st new old (cdr ls)))))))

(define substq-1st
  (lambda (new old ls)
    (cond ((null? ls) '())
	  ((eq? (car ls) old) (cons new (cons (cdr ls) '())))
	  (else (cons (car ls) (substq-1st new old (cdr ls)))))))

(define substv-1st
  (lambda (new old ls)
    (cond ((null? ls) '())
	  ((eqv? (car ls) old) (cons new (cons (cdr ls) '())))
	  (else (cons (car ls) (substv-1st new old (cdr ls)))))))

;; Exercise 2.14
;; The procedire insert-right-1st is like remove-1st except that instead of removing
;; the item that it is searching for, it inserts a new item to its right. For example,
;; (insert-right-1st 'not' 'does '(my dog does have fleas)) => (my dog does not have fleas)
;; The definition of insert-right-1st is
;;           (define insert-right-1st
;;             (lambda (new old ls)
;;               (cond
;;                 ((null? ls) '())
;;                 ((equal? (car ls) old
;;                  (cons old (cons new (cdr ls)))))
;;                 (else (cons (car ls)
;;                             (insert-right-1st new old (cdr ls)))))))
;; Define a procedure insert-left-1st that is like insert-right-1st except that instead of
;; inserting a new item to the right of the item it is searching for, it inserts it ot its left
;; Test your procedure on
;;              (insert-left-1st 'hot 'dogs '(I eat dogs) => (I eat hot dogs)
;;              (insert-left-lst 'fun 'games '(some fun)) => (some fun)
;;              (insert-left-lst 'a 'b '(a b c a b c)) => (a a b c a b c)
;;              (insert-left-lst 'a 'b '()) => ()

;; Define a procedure called insert-left-1st that adds an item to the left of the first instance
;; of an item on a list, if it exist. Iterates through the list to check if the given item exist
;; and then creates a new list with the new item added to the left of the old item.
(define insert-left-1st
  (lambda (new old ls)
    (cond ((null? ls) '())
	  ((equal? (car ls) old) (cons new (cons old (cdr ls))))
	  (else (cons (car ls) (insert-left-1st new old (cdr ls)))))))

;; Exercise 2.15
;; Define a procedure list-of-first-items that takes as its arguments a list composed of nonempty
;; list of items. Its value is a list composed of the first top-level item in each of the sublists.
;; Test your procedure on:
;;                   (list-of-first-items '((a) (b c d) (e f))) => (a b e)
;;                   (list-of-first-items '((1 2 3) (4 5 6))) => (1 4)
;;                   (list-of-first-items '((one))) => (one)
;;                   (list-of-first-items '()) => ()

;; Defines a procedure names list-of-first-items that takes a list of list and then contructs
;; a new list of the first item of every list in the given list.
(define list-of-first-items
  (lambda (ls)
    (cond ((null? ls) '())
	  (else (cons (car (car ls)) (list-of-first-items (cdr ls)))))))

;; Exercise 2.16
;; Define a procedure replace that replaces each top-level item in a list of items ls by a
;; given item new-item. Test your procedure on:
;;              (replace 'no '(will you do me a favor)) => (no no no no no no)
;;              (replace 'yes '(do you like ice cream)) => (yes yes yes yes yes)
;;              (replace 'why '(not)) => (why)
;;              (replace 'maybe '()) => ()

;; Defines a function replace that changes every item in the given list with the given item.
(define replace
  (lambda (item ls)
    (if (null? ls) '()
	(cons item (replace item (cdr ls))))))

;; Exercise 2.18
;; Define a procedure remove-last that removes the last top-level occurence of a given element
;; item in a list ls. Test your procedure on:
;;               (remove-last 'a '(b a n a n a s)) => (b a n a n s)
;;               (remove-last 'a '(b a n a n a)) => (b a n a n a)
;;               (remove-last 'a '()) => ()

;; Defines a procedure called remove-1st. Helper procedure for the procedure remove-last.
;; Removes the first occurence of the given item on the given list.
;; (remove-1st 'a '(a b c)) => (b c)
;; (remove-1st 'a '(b a c a)) => (b c a)
;; (remove-1st 'a '()) => ()
(define remove-1st
  (lambda (item ls)
    (cond ((null? ls) '())
	  ((equal? (car ls) item) (cdr ls))
	  (else (cons (car ls) (remove-1st item (cdr ls))))))) 

;; Defines a procedure called remove-last. Removes the last occurence of the given item in
;; a given list. The procedure uses the built in function reverse to reverse the list and
;; runs the procedure remove-1st, which is an easier problem to solve. Once it has remove
;; the first item in the reversed list, the list is reversed back to its original state,
;; effectively removeing the last occurance of the given item.
(define remove-last
  (lambda (item ls)
    (if (null? ls) '()
	(reverse (remove-1st item (reverse ls))))))
