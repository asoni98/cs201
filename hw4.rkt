#lang racket

(provide 
         entry entry? entry-key entry-value
         e-not e-not? e-not-arg
         e-and e-and? e-and-args
         e-or e-or? e-or-args
         tt tt-vars tt-rows tt?
	 hours
         lookup
         f-not f-and f-or
         boolean-exp?
	 all-vars
	 eval-in-env
	 all-keys
	 truth-table	 	 	 
         classify equivalent?
	 find-exp
         simplify)

; Please do not modify lines above this one.

; ****************************************************************
; CS 201 HW #4  DUE 11:59 pm Wednesday, March 1, 2017
; using the submit command on the Zoo.
; ****************************************************************
; Name: Arun Soni
; Email address: arun.soni@yale.edu
; ****************************************************************
; ** problem 0 ** (1 easy point)
; Please modify the following definition to reflect the number of
; hours you spent on this assignment.  (Must be non-zero to earn credit.)

(define hours 14)

; ****************************************************************
; Please DO NOT use require or mutators (set! and its relatives)
; in your programs.  Otherwise you may use any Racket constructs.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problems.  Please include
; a comment for each one explaining its input and results.

; Topics:
; Racket: deep recursion on a recursively defined data structure.
; Computer Science: Boolean functions, expressions, environments,
; truth tables, logical equivalence, tautology, contradiction,
; contingency, simplification.

; ****************************************************************
; We define a table as a list of entries,
; where each entry is given by the following structure.

(struct entry (key value) #:transparent)

; Recall that a struct defines a constructor, selector(s), and a type predicate.
; In this case, the constructor is entry, the selectors are
; entry-key and entry-value, and the type predicate is entry?.

; Here are two examples of tables.

(define table1
  (list
   (entry "second" 2)
   (entry "first" 1)
   (entry "fifth" 5)))

(define table2
  (list
   (entry 'x 0)
   (entry 'z 1)
   (entry 'y 1)
   (entry 'z 0)))

; ****************************************************************
; ** problem 1 ** (5 points)
; Write a procedure to deal with tables as follows.

; (lookup key table)

; returns #f if no entry in the table has a key equal? to key
; otherwise, returns the value of the first entry whose key is equal? to key.

; Examples
; (lookup "first" table1) => 1
; (lookup "third" table1) => #f
; (lookup 'z table2) => 1
; ****************************************************************

(define (lookup key table)
  (cond
    [(empty? table) #f]
    [(equal? (entry-key (first table)) key)  (entry-value (first table))]
    [else (lookup key (rest table))]))


; ****************************************************************
; ** problem 2 ** (5 points)
; Write three procedures to compute Boolean functions as follows.

; (f-not val)
; (f-and lst)
; (f-or lst)

; (f-not val) takes a single Boolean value represented by 0 or 1
; and returns the negation (NOT) of it.

; (f-and lst) takes a list lst of Boolean values represented by 0 or 1
; and returns the conjunction (AND) of them all.  If lst is empty, then
; the value returned should be 1.

; (f-or lst) takes a list lst of Boolean values represented by 0 or 1
; and returns the disjunction (OR) of them all.  If lst is empty, then
; the value returned should be 0.

; Examples
; (f-not 0) => 1
; (f-and '()) => 1
; (f-and '(1 1 0 1)) => 0
; (f-or '()) => 0
; (f-or '(1)) => 1
; ****************************************************************

(define (f-not val)
  (cond
    [(= val 0) 1]
    [(= val 1) 0]))

(define (f-and lst)
  (cond
    [(empty? lst) 1]
    [(equal? (first lst) 1) (f-and (rest lst))]
    [else 0]
    ))

(define (f-or lst)
  (cond
    [(empty? lst) 0]
    [(equal? (first lst) 1) 1]
    [else (f-or (rest lst))]
    ))

; ****************************************************************
; Our representation of Boolean expressions will use the following
; struct definitions, for the operations of NOT, AND, OR.

(struct e-not (arg) #:transparent)
(struct e-and (args) #:transparent)
(struct e-or (args) #:transparent)

; These define constructors, selectors, and type predicates as follows
; e-not, e-not-arg, e-not?
; e-and, e-and-args, e-and?
; e-or, e-or-args, e-or?

; A Boolean expression is defined recursively as follows.
; 1) the constants 0 and 1 are Boolean expressions
; 2) any identifier is a Boolean expression, where the variable x
; is represented by the symbol 'x
; 3) if <E> is any Boolean expression, its negation (NOT) is represented
; by (e-not <E>).
; 4) if <E1>, ..., <En> are any Boolean expressions, their conjunction (AND)
; is represented by (e-and (list <E1> ... <En>)).  If the list is empty,
; then the AND expression is equivalent to the constant 1.
; 5) if <E1>, ..., <En> are any Boolean expressions, their disjunction (OR)
; is represented by (e-or (list <E1> ... <En>)).  If the list is empty,
; then the OR expression is equivalent to the constant 0.

; Some examples of Boolean expressions:

; The expression 0'
(define exp1 (e-not 0))

; The expression (x + y)
(define exp2 (e-or (list 'x 'y)))

; The expression (x * y * z)
(define exp3 (e-and (list 'x 'y 'z)))

; The expression (w * (x' + 0 + y)))
(define exp4 (e-and (list 'w (e-or (list (e-not 'x) 0 'y)))))

; The expression (x + x')
(define exp5 (e-or (list 'x (e-not 'x))))

; ****************************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (boolean-exp? val)

; (boolean-exp? val) takes an arbitrary Racket value val
; and tests to see whether it is a Boolean expression according
; to the definition above, returning #t if so and #f if not.

; The predicate (symbol? arg) will test whether its argument
; is an identifier.

; Hint: deep recursion on the structure of Boolean expressions.

; Examples
; (boolean-exp? 0) => #t
; (boolean-exp? 2) => #f
; (boolean-exp? exp1) => #t
; (boolean-exp? (e-and (list 'x "hi!"))) => #f
; (boolean-exp? (e-and (list 'x 0 'y))) => #t
; (boolean-exp? (e-or (list 'x (e-and (list 'y #t))))) => #f
; ****************************************************************

; e-not, e-not-arg, e-not?
; e-and, e-and-args, e-and?
; e-or, e-or-args, e-or?

(define (boolean-exp? val)
  (cond
    [(equal? val empty) #t]
    [(or (symbol? val) (equal? val 0) (equal? val 1)) #t]
    [(e-not? val) (and (check-values (e-not-arg val)))]
    [(e-or? val) (and (check-values (e-or-args val)))]
    [(e-and? val) (and (check-values (e-and-args val)))]
    [else #f]
    ))
;(cuts off the e-or, e-and, e-not for checking the list or individual values in check-values). only allows individual values to be computed in boolean-exp.
;lists aren't allowed to be computed in boolean-exp

(define (check-values val)
  (cond
    [(empty? val) #t]
    [(list? val) (and (check-values (first val)) (check-values (rest val)))]
    [(or (equal? val 1) (equal? val 0)) #t]
    [(symbol? val) #t]
    [(or (e-not? val) (e-and? val) (e-or? val)) (boolean-exp? val)]
    [else #f]
    ))
;check the values of boolean exp (which cuts off the e-or, e-and, e-not)

;edge case this    

;is (boolean-exp? 'x) #t

; ****************************************************************
; ** problem 4 ** (10 points)
; Write a procedure

; (all-vars bexp)

; that takes a Boolean expression bexp 
; and makes a list containing all the variables
; that occur in bexp.  The list should not contain duplicates,
; and should have the variables in the order of their
; first appearance in bexp (scanning left to right.)

; Hint: deep recursion on the structure of Boolean expressions.

; Note that there is a Racket procedure: remove-duplicates

; Examples

; (all-vars 0) =>'()
; (all-vars 'x) => '(x)
; (all-vars (e-not (e-and (list 'x (e-or (list 'y (e-not 'z) 'x)))))) => '(x y z)
; (all-vars (e-and (list 1 (e-or (list 0 (e-not 'u)))))) => '(u)
; (all-vars (e-and (list (e-or (list 'x 'y)) (e-or (list (e-not 'y) (e-not 'x)))))) => '(x y)
; (all-vars (e-or (list 'c 'b 'a (e-and (list 'a 'b 'c))))) => '(c b a)
;*************************************************

(define (all-vars-helper val)
  (cond
    [(equal? val empty) '()]
    [(symbol? val) val]
    [(e-not? val) (all-vars-helper (e-not-arg val))]
    [(e-or? val) (all-vars-helper (e-or-args val))]
    [(e-and? val) (all-vars-helper (e-and-args val))]
    [(list? val) (cons (all-vars-helper (first val)) (all-vars (rest val)))]
    [else '()]
  ))

(define (one-list lst)
  (cond
    [(empty? lst) '()]
    [(not (list? lst)) (list lst)]
    [(list? (first lst)) (append (one-list (first lst)) (one-list (rest lst)))]
    [else (cons (first lst) (one-list (rest lst)))]))
  
(define (all-vars val)
  (cond
   [(symbol? val) (list val)]
   [else (remove-duplicates (one-list (all-vars-helper val)))]))
    
;(remove-duplicates1 '(1 2 3 (1 2 3) (4 5 6) 1 2))


; ****************************************************************
; We represent an environment as table each entry of which
; has a key that is a Racket symbol and a value that is 0 or 1,
; which specifies the truth value of that variable in the environment.
; For example:

(define environ1
  (list
   (entry 'x 0) (entry 'y 1) (entry 'z 0)))
  
(define environ2
  (list
   (entry 'u 0) (entry 'x 1) (entry 'w 1) (entry 'y 0) (entry 'z 1)))

; ****************************************************************
; ** problem 5 ** (10 points)
; Write a procedure 

; (eval-in-env bexp env)

; that takes a Boolean expression bexp and an environment env
; (represented as described above) and returns 0 or 1 giving 
; the value of the expression in the environment.

; If the Boolean expression contains variables that do not
; occur in the environment, (eval-in-env bexp env) should
; return the string: "variable unspecified in environment".
; (You may want to check for this condition first.)

; Hint: deep recursion on the structure of Boolean expressions.

; Examples

; (eval-in-env 1 environ1) => 1
; (eval-in-env (e-or (list 0 0 0)) '()) => 0
; (eval-in-env 'x environ1) => 0
; (eval-in-env 'x environ2) => 1
; (eval-in-env (e-not 'z) environ1) => 1
; (eval-in-env (e-or (list 'y (e-not 'x))) environ2) => 0
; (eval-in-env (e-and (list (e-or (list 'y 'x)) (e-or (list 'w 'y)))) environ2) => 1
; (eval-in-env exp5 environ1) => 1
; (eval-in-env (e-and (list 'x 'y 'z)) (list (entry 'x 1) (entry 'z 0))) => "variable unspecified in environment"
; ****************************************************************

;(define (boolean-exp? val)
 ; (cond
  ;  [(equal? val empty) #t]
   ; [(or (symbol? val) (equal? val 0) (equal? val 1)) #t]
;    [(e-not? val) (and (check-values (e-not-arg val)))]
;    [(e-or? val) (and (check-values (e-or-args val)))]
;    [(e-and? val) (and (check-values (e-and-args val)))]
;    [else #f]
;    ))

; e-not, e-not-arg, e-not?
; e-and, e-and-args, e-and?
; e-or, e-or-args, e-or?

; entry-key and entry-value, and the type predicate is entry?.
;(lookup key table)
;(lookup (valueinlst) env)

(define (run bexp env)
  (checkvariables (all-vars bexp) env))

(define (checkvariables lst env)
  (cond
    [(empty? lst) #t]
    [else (and (lookup (first lst) env) (checkvariables (rest lst) env))]
     ))

(define (eval-in-env-h bexp env)
  (cond
    [(empty? bexp) '()]
    [(equal? bexp 1) 1]
    [(equal? bexp 0) 0]
    [(e-not? bexp) (f-not (eval-in-env-h (e-not-arg bexp) env))]
    [(e-or? bexp)  (f-or (eval-in-env-h (e-or-args bexp) env))]
    [(e-and? bexp) (f-and (eval-in-env-h (e-and-args bexp) env))]
    [(list? bexp) (cons (eval-in-env-h (first bexp) env) (eval-in-env-h (rest bexp) env))]
    [(symbol? bexp) (lookup bexp env)]
    ))

(define (eval-in-env bexp env)
  (cond
    [(not (run bexp env)) "variable unspecified in environment"]
    [else (eval-in-env-h bexp env)]))

;EDGE CASE


; ****************************************************************
; We define a truth table as represented by the following struct

(struct tt (vars rows) #:transparent)

; whose fields contain the following
; (1) a (possibly empty) list of n distinct variables, and
; (2) a table containing an entry for each row of the truth table:
; the key of an entry is a list of n 0's and 1's, and the value is the
; Boolean value (0 or 1) of the function on that row of the table.

; Note that the entries in a truth table should be in increasing order of
; their keys, considered as binary numbers.

; Examples of truth tables for the functions given by
; x', (x * y), (a NAND b), (u XOR v)

(define tt-not (tt '(x)
                   (list
                    (entry '(0) 1)
                    (entry '(1) 0))))

(define tt-and (tt '(x y)
                   (list 
                    (entry '(0 0) 0)
                    (entry '(0 1) 0)
                    (entry '(1 0) 0)
                    (entry '(1 1) 1))))
                    
 (define tt-nand (tt '(a b)
                   (list
                    (entry '(0 0) 1)
                    (entry '(0 1) 1)
                    (entry '(1 0) 1)
                    (entry '(1 1) 0))))
  
(define tt-xor (tt '(u v)
                   (list
                    (entry '(0 0) 0)
                    (entry '(0 1) 1)
                    (entry '(1 0) 1)
                    (entry '(1 1) 0))))

; Here is a truth table for a function of three arguments a, b, c.

(define tt-f1 (tt '(a b c)
                  (list
                   (entry '(0 0 0) 0)
                   (entry '(0 0 1) 0)
                   (entry '(0 1 0) 1)
                   (entry '(0 1 1) 1)
                   (entry '(1 0 0) 0)
                   (entry '(1 0 1) 1)
                   (entry '(1 1 0) 0)
                   (entry '(1 1 1) 1))))

(define tttest (tt '(a b c d)
                  (list
                   (entry '(0 0 0) 0)
                   (entry '(0 0 1) 0)
                   (entry '(0 1 0) 1)
                   (entry '(0 1 1) 1)
                   (entry '(1 0 0) 0)
                   (entry '(1 0 1) 1)
                   (entry '(1 1 0) 0)
                   (entry '(1 1 1) 1))))

; ****************************************************************
; ** problem 6 ** (10 points)
; Write a procedure 

; (all-keys n)

; that takes a non-negative integer n and creates the list of all 
; lists of n 0's or 1's in the *specific order* required for
; a truth table.  In other words, the lists, interpreted as
; binary numbers, should be in increasing order.

; Hint: if a recursive call gives the correct answer
; for (all-keys 2), what needs to happen to it
; to give the correct answer for (all-keys 3)?
; (Compare bit-strings from lecture and all-subsets from hw #2.)

; Use let or let* to avoid recomputing the recursive call!

; Examples
; (all-keys 0) => '(())
; (all-keys 1) => '((0) (1))
; (all-keys 2) => '((0 0) (0 1) (1 0) (1 1))
; (all-keys 3) => '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))
; ****************************************************************
;0 added first to everyone of the last then 1

 (define (put-first x lsts)
	    (map (lambda (lst)
	           (cons x lst))
                 lsts))

  (define (all-keys n)
            (cond
              [(= n 0) '(())]
              [(= n 1) '((0) (1))]
              [else
               (let ((previous (all-keys (- n 1))))
	          (append
                    (put-first 0 previous)
	            (put-first 1 previous)))]))

; ****************************************************************
; ** problem 7 ** (10 points)
; Write a procedure

; (truth-table bexp)

; that takes a Boolean expression bexp and returns the truth table for bexp
; where the variables for the table are extracted from bexp using all-vars, 
; and the function value for each row is obtained by evaluating bexp 
; in the corresponding environment.  Notice that all-vars specifies
; the order of variables for the truth table.

; Examples:

;> (truth-table exp1)
;(tt '() (list (entry '() 1)))

;> (truth-table exp2)
;(tt
; '(x y)
; (list (entry '(0 0) 0) (entry '(0 1) 1) (entry '(1 0) 1) (entry '(1 1) 1)))

;>  (truth-table exp4)
;(tt
; '(w x y)
; (list
;  (entry '(0 0 0) 0)
;  (entry '(0 0 1) 0)
;  (entry '(0 1 0) 0)
;  (entry '(0 1 1) 0)
;  (entry '(1 0 0) 1)
;  (entry '(1 0 1) 1)
;  (entry '(1 1 0) 0)
;  (entry '(1 1 1) 1)))
; ****************************************************************

(define (environment allvars firstallkeys)
  (cond
    [(empty? allvars) '()]
    [(empty? firstallkeys) '()]
    [else (cons (entry (first allvars) (first firstallkeys))
                (environment (rest allvars) (rest firstallkeys)))]))
;creates an environment defining the (first (first key value)) to as the value for the first variable and continuing for the rest of the variables

(define (evaluate bexp allvars allkeys)
  (cond
    [(empty? allkeys) '()]
    [else (cons (entry (first allkeys) (eval-in-env bexp (environment (all-vars bexp) (first allkeys)))) (evaluate bexp allvars (rest allkeys)))]))
;creates an entry table with the keys evaluated

(define (truth-table bexp)
  (tt (all-vars bexp) (evaluate bexp (all-vars bexp) (all-keys (length (all-vars bexp))))))

; ****************************************************************
; ** problem 8 ** (10 points)
; Write two procedures

; (classify bexp)
; (equivalent? bexp1 bexp2)

; (classify bexp) takes a Boolean expression and classifies it, returning one of the
; identifiers: 'tautology, 'contradiction, or 'contingent.
; The expression is a tautology if it is true in every environment,
; a contradiction if it is false in every environment, and contingent
; if it is true in some environments and false in some environments.

; (equivalent? bexp1 bexp2) takes two Boolean expressions and returns #t if
; they are logically equivalent, and #f if they are not logically equivalent.
; Two expressions are logically equivalent if, for every environment that
; assigns Boolean values to ALL the variables that appear in either expression,
; the two expressions have the same value.

; For example, the expression 'a is not equivalent to the expression 'b,
; because in the environment (list (entry 'a 0) (entry 'b 1)),
; the first expression takes the value 0, but the second expression takes the value 1.

; These procedures will be tested on expressions with few enough
; variables that generating truth tables WILL BE a feasible approach.

; Examples
; (classify 0) => 'contradiction
; (classify (e-or (list 'x (e-not 'x)))) => 'tautology
; (classify exp2) => 'contingent
; (classify exp3) => 'contingent
; (classify (e-and '())) => 'tautology

; (equivalent? 0 (e-and (list 'a 0))) => #t
; (equivalent? 'a 'b) => #f
; (equivalent? (e-not (e-or (list 'a 'b 'c))) (e-and (list (e-not 'c) (e-not 'b) (e-not 'a)))) => #t
; ****************************************************************

(define (table-create bexp)
  (evaluate bexp (all-vars bexp) (all-keys (length (all-vars bexp)))))

(define (classify bexp)
  (classify-output bexp (table-create bexp)))

(define (classify-evaluate bexp table)
  (cond
    [(empty? table) 0]
    [(equal? (entry-value (first table)) 0) (+ 1 (classify-evaluate bexp (rest table)))]
    [else (classify-evaluate bexp (rest table))]))

(define (classify-output bexp table)
  (cond
    [(equal? (length table) (classify-evaluate bexp table)) 'contradiction]
    [(equal? (classify-evaluate bexp table) 0) 'tautology]
    [else 'contingent]))

(define (listcheck lst1 lst2)
    (cond
      [(empty? lst1) #t]
      [(and (member (first lst1) lst2) (listcheck (rest lst1) lst2))]
      [else #f]))

(define (listcheckfull lst1 lst2)
  (if (equal? (length lst1) (length lst2))
   (listcheck lst1 lst2) #f))

(define (equivalent? bexp1 bexp2)
  (let ((gatexor (e-or (list (e-and (list (e-not bexp1) bexp2)) (e-and (list bexp1 (e-not bexp2)))))))
  (cond
    [(and (equal? (classify bexp1) 'contradiction) (equal? (classify bexp2) 'contradiction)) #t]
    [(and (equal? (classify bexp1) 'tautology) (equal? (classify bexp2) 'tautology)) #t]
    [(equal? (classify gatexor) 'contradiction) #t]
    [else #f])))

(define (equivalent1? bexp1 bexp2)
  (cond
    [(and (equal? (classify bexp1) 'contradiction) (equal? (classify bexp2) 'contradiction)) #t]
    [(and (equal? (classify bexp1) 'tautology) (equal? (classify bexp2) 'tautology)) #t]
    [(and (listcheckfull (all-vars bexp1) (all-vars bexp2)) (equal? (table-create bexp1) (table-create bexp2))) #t]
    [else #f]
  ))

;both all 0 then true
;both all 1 then true
;one indecisive then false 

; ****************************************************************
; ** problem 9 ** (20 points)
; Write a procedure

; (find-exp tt)

; This procedure takes a truth table
; and returns a Boolean expression 
; for the given truth table.

; You may choose to use the sum-of-products algorithm
; from lecture, or some other method, but it must
; return a Boolean expression with the given truth table.

; Examples
; (find-exp tt-and) => (e-or (list (e-and '(x y))))

; (find-exp tt-nand) => (e-or
;                        (list
;                         (e-and (list (e-not 'a) (e-not 'b)))
;                         (e-and (list (e-not 'a) 'b))
;                         (e-and (list 'a (e-not 'b)))))

; (find-exp tt-xor) =>(e-or (list (e-and (list (e-not 'u) 'v)) (e-and (list 'u (e-not 'v))))

; (find-exp tt-f1) => (e-or
;                      (list
;                       (e-and (list (e-not 'a) 'b (e-not 'c)))
;                       (e-and (list (e-not 'a) 'b 'c))
;                       (e-and (list 'a (e-not 'b) 'c))
;                       (e-and '(a b c))))

; ****************************************************************

(define (decompose allvars table tt)
  (cond
    [(empty? table) '()] ;put e-or at front
    [(equal? (entry-value (first table)) 1)
     (append (list (e-and (put allvars (entry-key (first table))))) (decompose allvars (rest table) tt))]
    [else (decompose allvars (rest table) tt)]))
    
(define (put allvars firsttable)
  (cond
    [(empty? firsttable) '()]
    [(equal? (first firsttable) 0)
     (append (list (e-not (first allvars))) (put (rest allvars) (rest firsttable)))]
    [(equal? (first firsttable) 1)
     (append (list (first allvars)) (put (rest allvars) (rest firsttable)))]))

(define (find-exp tt)    
  (e-or (decompose (tt-vars tt) (tt-rows tt) tt)))

;(entry-key (first table))
;(entry-value (first (tt-rows tt-not)))
;(e-not (first (tt-vars tt-f1))) (e-not 'a)
             ;(tt-vars tt-not) '(x)
;(tt-rows tt-not) (list (entry '(0) 1) (entry '(1) 0))
;(entry-key (first (tt-rows tt-not))) '(0)
;(entry-value (first (tt-rows tt-not))) 1

; ****************************************************************
; ** problem 10 ** (9 points)
; Write a procedure

; (simplify bexp)

; that takes a Boolean expression bexp and returns
; an equivalent Boolean expression that is
; simplified as much as possible using the following rules:

; x + 0 -> x
; 0 + x -> x
; x + 1 -> 1
; 1 + x -> 1
; x * 0 -> 0
; 0 * x -> 0
; x * 1 -> x
; 1 * x -> x
; 0' -> 1
; 1' -> 0
; (x')' -> x

; Also note that
; (e-or '()) should be simplified to 0
; (e-and '()) should be simplified to 1
; (e-or (list bexp)) should be simplified to bexp - fail
; (e-and (list bexp)) should be simplified to bexp - fail

; Examples
; (simplify 0) => 0
; (simplify 'x) => 'x
; (simplify (e-not (e-not 'x))) => 'x
; (simplify (e-not 'y)) => (e-not 'y)
; (simplify (e-or (list 0 (e-and (list 1 (e-not 0) 1))))) => 1
; (simplify (e-and (list 'x 1))) => 'x
; (simplify (e-or (list 0 'z 0))) => 'z
; (simplify (e-or (list (e-and (list 'x 1)) (e-or (list (e-not 1) 'z))))) => (e-or '(x z))
; ****************************************************************

; e-not, e-not-arg, e-not?
; e-and, e-and-args, e-and?
; e-or, e-or-args, e-or?

(define (or-search)
  (lambda (x) (cond
                [(equal? x 0) #f]
                [else #t])))
(define (and-search)
  (lambda (x) (cond
                [(equal? x 1) #f]              
                [else #t])))

(define (enot val)
(cond
  [(equal? val 0) 1]
  [(equal? val 1) 0]
  [(symbol? val) (e-not val)]
  [else (e-not val)]))

(define (simplify-helper val)
  (cond
    [(equal? (classify val) 'contradiction) 0]
    [(equal? (classify val) 'tautology) 1]

    [(e-not? val)
     (cond
       [(e-not? (e-not-arg val)) (simplify-helper (e-not-arg (e-not-arg val)))]
       [else (enot (simplify-helper (e-not-arg val)))])]
    [(e-or? val) (let ((x (filter (or-search) (map simplify-helper (e-or-args val)))))
     (cond
       [(empty? (e-or-args val)) 0]
       [(= (length x) 1) (first x)]
       [else (e-or x)]
       ))]
    [(e-and? val) (let ((y (filter (and-search) (map simplify-helper (e-and-args val)))))
                    (cond
                      [(member 0 y) 0]
                      [(empty? (e-and-args val)) 1]
                      [(= (length y) 1) (first y)]
                      [else (e-and y)]))]
    [(list? val) (first val)]
    [else val]
    ))

(define (simplify val)
  (cond
    [(empty? (e-or? (simplify-helper val))) 0]
    [(empty? (e-and? (simplify-helper val))) 1]
;    [(and (e-or? val) (list? (e-or-args val))) (if (e-or? (simplify-helper val)) (e-or-args (simplify-helper val)) (simplify-helper val))]
 ;   [(and (e-and? val) (list? (e-and-args val))) (if (e-and? (simplify-helper val)) (e-and-args (simplify-helper val)) (simplify-helper val))]
    [else (simplify-helper val)]))

    ;(e-or (list bexp)) should be simplified to bexp - fail
; (e-and (list bexp))
    
   

; **************** end of hw #4 *********************************