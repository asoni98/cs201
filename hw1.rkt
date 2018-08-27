#lang racket

(provide hours
         general-last-digit
         what-beats
         result
         digit-sum
	 reduce
	 top-count
	 diff-sequence
	 top-replace
         prefix?
         all?
	 exists?
         skip-every-other
	)

; Please do not modify the lines above this one.
; ********************************************************
; CS 201 hw #1  DUE Wednesday, 2/1/2017 at 11:59 pm
;                via the submit system on the Zoo
; ********************************************************
; Name: Arun Soni
; Email address: arun.soni@yale.edu
; ********************************************************

; This file may be opened in DrRacket.
; Lines beginning with semicolons are comments.
; Please do not use comment boxes for your comments.

; If you are asked to write a procedure, please make sure it has 
; the specified name, and the specified number and order of arguments.  
; The names of the formal arguments need not be the same
; as in the problem specification.

; For each problem, the intended inputs to your procedures
; are specified (for example, "positive integers") 
; and your procedures need not do anything reasonable 
; for other possible inputs.

; You may write auxiliary procedures in addition to
; the requested one(s) -- for each of your auxiliary 
; procedures, please include a comment explaining
; what it does, and giving an example or two.

; You may also use procedures you have written 
; elsewhere in this assignment or previous assignments.
; They only need to be defined once somewhere within this file.

; Please use the predicate equal? to test equality
; of values that may not be numbers.  To test equality
; of numbers, you can use =.

; Reading: Chapters 1 and 2 of the Racket Guide.

; ********************************************************
; ** problem 0 ** (1 easy point) 
; Replace the number 0 in the definition below to indicate
; the number of hours you spent doing this assignment
; Decimal numbers (eg, 6.237) are fine.  Exclude time
; spent reading.

(define hours 3)

; ********************************************************
; ** problem 1 ** (11 points)
; Write a procedure

; (general-last-digit n base)

; that takes a nonnegative integer n
; and a positive integer base (at least 2)
; and returns the value of the last (i.e., rightmost) digit
; of the number n when written in the given base.

; This generalizes the procedure (last-digit n) from
; lecture, which returned the last decimal (base 10)
; digit of n.

; Note that for bases larger than 10, the "digits" of n
; may be numbers larger than 10.

; Examples
; (general-last-digit 0 10) => 0
; (general-last-digit 13 2) => 1
; (general-last-digit 20 3) => 2
; (general-last-digit 458 10) => 8
; (general-last-digit 67 4) => 3
; (general-last-digit 187 16) => 11
; ********************************************************

(define (general-last-digit n base)
  (remainder n base))
  
; ********************************************************
; ** problem 2 ** (11 points)
; Write two procedures

; (what-beats x)
; (result x y)

; where x and y are each one of the three strings:
; "rock", "paper", or "scissors"
; (what-beats x) returns the string representing the unique choice
; that beats x in the game of rock-paper-scissors
; (result x y) returns one of the strings "win", "tie", or "lose",
; indicating, respectively, whether move x beats move y,
; or move x is the same as move y
; or move y beats move x in the game of rock-paper-scissors

; Examples
; (what-beats "rock") => "paper"
; (what-beats "scissors") => "rock"
; (result "rock" "paper") => "lose"
; (result "scissors" "scissors") => "tie"
; (result "scissors" "paper") => "win"
; ********************************************************

(define (what-beats x)
  (cond
    [(equal? x "paper") "scissors"]
    [(equal? x "rock") "paper"]
    [(equal? x "scissors") "rock"]
    ))

(define (result x y)
  (cond
    [(equal? x y) "tie"]
    [(and (equal? x "rock") (equal? y "scissors")) "win"]
    [(and (equal? x "scissors") (equal? y "paper")) "win"]
    [(and (equal? x "paper") (equal? y "rock")) "win"]
    [else "lose"]
    ))

; ********************************************************
; ** problem 3 ** (11 points)
; Write two procedures

; (digit-sum n)
; (reduce n)

; that each take a nonnegative integer n as input
; (digit-sum n) returns the sum of the decimal digits of n
; (reduce n) repeatedly sums the decimal digits of n until
; a one-digit result is obtained.

; Hint: think about (remainder n 10) and (quotient n 10).

; Examples
; (digit-sum 3) => 3
; (reduce 3) => 3
; (digit-sum 223) => 7
; (reduce 223) => 7
; (digit-sum 437) => 14
; (reduce 437) => 5  (because 4+3+7 = 14 and 1+4 = 5)
; ********************************************************

(define (digit-sum n)
  (if (< n 10)
      n
      (+ (remainder n 10) (digit-sum (quotient n 10)))))

(define (reduce n)
  (if (< (digit-sum n) 10)
      (digit-sum n)
      (+ (remainder (digit-sum n) 10) (digit-sum (quotient (digit-sum n) 10)))))
       
; ********************************************************
; ** problem 4 ** (11 points)
; Write a procedure

; (top-count exp lst)

; that takes an arbitrary expression exp and a list lst
; and returns the number of top-level members of the
; list lst that are equal? to the value of exp.

; Examples:
; (top-count 4 '(4 3 2 4 4 3 0)) => 3
; (top-count '(1 2 3) '((1 1 2) (3 2 1) (1 2 3) ((1) 2 3))) => 1
; (top-count "hi!" '("maybe" "not" "here?")) => 0
; ********************************************************

(define (top-count exp lst)
  (if (empty? lst)
      0
      (if (equal? (first lst) exp) (+ 1 (top-count exp (rest lst))) (top-count exp (rest lst)))))

; ********************************************************
; ** problem 5 ** (11 points)
; Write a procedure

; (diff-sequence lst)

; that takes a list lst of at least two integers
; and returns a list consisting of the differences between
; each two consecutive elements of the list.

; Examples:
; (diff-sequence '(4 9)) => '(5)
; (diff-sequence '(1 3 5 7)) => '(2 2 2)
; (diff-sequence '(9 16 25 36 49)) => '(7 9 11 13))
; (diff-sequence '(3 2 1 0 -1)) => '(-1 -1 -1 -1)
; ********************************************************

(define (diff-sequence lst)
      (cond
        [(empty? lst) '()]
        [(= (length lst) 1) '()]
        [else (cons (- (first (rest lst)) (first lst)) (diff-sequence (rest lst)))]))

;(define 

; ********************************************************
; ** problem 6 ** (11 points)
; Write a procedure 

; (top-replace old new lst)

; that replaces every top-level value in lst
; that is equal? to old with the value of new.

; Examples
; (top-replace 3 7 '(4 3 2 4 4 3 0)) => '(4 7 2 4 4 7 0)
; (top-replace "a" "x" '("f" "a" "f" "a")) => '("f" "x" "f" "x")
; (top-replace 2 3 '(4 5 4)) => '(4 5 4)
; (top-replace 5 '(3 2) '(5 5 (5 5))) => '((3 2) (3 2) (5 5))
; ********************************************************
 
(define (top-replace old new lst)
(if (empty? lst) '()
  (if (equal? old (first lst))
      (cons new (top-replace old new (rest lst)))
      (cons (first lst) (top-replace old new (rest lst)))
      )))

; ********************************************************
; ** problem 7 ** (11 points)
; Write a procedure (in fact, a predicate)

; (prefix? lst1 lst2)

; that takes two lists lst1 and lst2
; and returns #t if each element of lst1 is equal? to
; the corresponding element of lst2, and #f otherwise

; Examples
; (prefix? '() '(2 3 1)) => #t
; (prefix? '(2 3) '(2 3 1)) => #t
; (prefix? '(3 1) '(2 3 1)) => #f
; (prefix? '("a" "x") '("a" "x")) => #t
; (prefix? '(2 2 2) '(2 2 2 2)) => #t
; (prefix? '(2 2 2) '(2 2)) => #f
; (prefix? '() '()) => #t
; ********************************************************

;DOUBLE CHECK THIS
;car and cdr

;(define (prefix? lst1 lst2)
;(if
 ;(<= (length lst1) (length lst2))
  ;(if (empty? lst1) #t
   ;   (if (equal? (first lst1) (first lst2))
    ;      (prefix? (rest lst1) (rest lst2))
     ;   #f))
  ;#f))

(define (prefix? lst1 lst2)
  (cond
    [(empty? lst1) #t]
    [(> (length lst1) (length lst2)) #f]
    [else (and (equal? (first lst1) (first lst2)) (prefix? (rest lst1) (rest lst2)))]))

; ********************************************************
; ** problem 8 ** (11 points)
; Write two procedures (actually, predicates)

; (all? pred? lst)
; (exists? pred? lst)

; where pred? is a predicate of one argument
; and lst is a list of values which are
; acceptable arguments for pred?  

; If pred? is #t for every top-level element of
; lst, then (all? pred? lst) returns #t, otherwise
; it returns #f.
; If pred? is #t for at least one top-level element
; of lst, then (exists? pred? lst) returns #t, otherwise
; it returns #f.

; Examples:
; (all? odd? '(3 5 7)) => #t
; (all? odd? '()) => #t
; (all? odd? '(5 7 8 9)) => #f
; (all? (lambda (x) (or (boolean? x) (number? x))) '(#t 1 2 #f 33)) => #t
; (exists? even? '()) => #f
; (exists? even? '(3 4 6 8)) => #t
; (exists? list? '(1 2 (3 4) 5 6)) => #t
; ********************************************************

(define (all? pred? lst)
  (if (empty? lst) #t
      (and (pred? (first lst)) (all? pred? (rest lst)))))

(define (exists? pred? lst)
   (if (empty? lst) #f
      (or (pred? (first lst)) (exists? pred? (rest lst)))))

; ********************************************************
; ** problem 9 ** (11 points)
; Write a procedure

; (skip-every-other lst)

; that takes as input a list lst
; and returns the list of every other
; element of lst, starting with the first
; element of lst.

; Examples:
; (skip-every-other '()) => '()
; (skip-every-other '(13 14 15 16 17)) => '(13 15 17)
; (skip-every-other '(2 2 3 3 2 3)) => '(2 3 2)
; (skip-every-other '("this" "is" "a" "list" "of" "strings")) =>
;     '("this" "a" "of")
; ********************************************************

;DOUBLE CHECK THIS
(define (skip-every-other lst)
  (cond
    [(empty? lst) '()]
    [(= (length lst) 1) (list (first lst))]
    [(= (length lst) 2) (list (first lst))]
    [else (cons (first lst) (skip-every-other (rest (rest lst))))]))

; ********************************************************
; **** end of hw #1