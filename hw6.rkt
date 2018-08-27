#lang racket

(provide con con? con-arg1 con-arg2
         alt alt? alt-arg1 alt-arg2
         rep rep? rep-arg
         ques ques? ques-arg
         reg-exp?
         flip try-flips pick
         reg-exp-gen
         dfa dfa? dfa-alph dfa-states dfa-start dfa-acc dfa-trans
         entry entry? entry-key entry-value
         dfa-accepts?
         cfg cfg? cfg-nonterminals cfg-start cfg-rules
         rule rule? rule-lhs rule-rhs
         cfg-gen
         my-cfg
         reg-exp->cfg)


; Please do not change lines above this one.

;************************************************************
; CS 201a HW #6  DUE Wednesday, April 19 at 11:59 pm, 
; via the submit system on the Zoo.
;************************************************************
; Name: Arun Soni
; Email address: arun.soni@yale.edu

(define hours 7)
;************************************************************

; Computer science topics: strings, languages, regular expressions,
; deterministic finite state acceptors and context free grammars.
; Racket: operations on strings and characters.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

; The following structs are used in the representation of Regular Expressions.

(struct con (arg1 arg2) #:transparent)
(struct alt (arg1 arg2) #:transparent)
(struct rep (arg) #:transparent)
(struct ques (arg) #:transparent)

; A string is a Racket string.  Consult the Racket Reference
; for information and procedures for strings and characters.

; A Regular Expression is defined recursively as follows:
; (1) a Racket string is a Regular Expression,
; (2) If exp1 and exp2 are Regular Expressions, then so are
; the following:
; (con exp1 exp2), the concatenation of exp1 and exp2
; (alt exp1 exp2), the "or" of exp1 and exp2, that is (exp1 | exp2)
; (rep exp1), zero or more repetitions of exp1, that is, (exp1)*
; (ques exp1), zero or one repetitions of exp1, that is (exp1)?

; Examples of Regular Expressions

;************************************************************
; ** problem 1 ** (10 points)
; Write a procedure

; (reg-exp? value)

; The procedure (reg-exp? value) takes an arbitrary Racket value
; and returns #t if it is a Regular Expression according to the
; definition given above, and #f otherwise.
; Hint: write a recursive procedure patterned after the recursive definition

; Examples
; (reg-exp? exp1) => #t
; and similarly for exp2 through exp6
; (reg-exp? 'a) => #f
; (reg-exp? '(a b b a b)) => #f
;****************************

(define (reg-exp? value)
  (cond
    [(string? value) #t]
    [(con? value) (and (reg-exp? (con-arg1 value)) (reg-exp? (con-arg2 value)))]
    [(alt? value) (and (reg-exp? (alt-arg1 value)) (reg-exp? (alt-arg1 value)))]
    [(rep? value) (reg-exp? (rep-arg value))]
    [(ques? value) (reg-exp? (ques-arg value))]
    [else #f]))

;************************************************************
; ** problem 2 ** (10 points)
; Write three procedures

; (flip bias)
; (try-flips count bias)
; (pick lst)

; The procedure (flip bias) simulates flipping a biased coin, 
; where the bias is specified as a number between 0 and 1,
; and the result is #t with probability equal to the bias 
; and #f with probability (1 - bias). 

; The procedure (try-flips count bias)
; calls (flip bias) the number of times specified by count
; and returns a list of two integers:
; the first is how many times the flip returned #t
; and the second is how many times the flip returned #f

; Note that the racket procedure (random) returns a random
; number between 0 and 1.

; The procedure (pick lst) takes a list lst and returns
; a randomly chosen element of lst.  If lst is empty, the
; value returned should be #f.  You can test it by picking 10000
; times from a list with 10 elements, and making sure that
; each element is picked about 1000 times.

;Examples (your results will randomly vary)
;> (flip .9)
;#f
;> (flip .9)
;#t
;> (try-flips 1000 .4)
;'(427 573)
;> (try-flips 1000 .69)
;'(686 314)
;> (pick '(a b c d e))
;'c
;> (pick '(a b c d e))
;'e
;************************************************************

(define (flip bias)
  (let ([x (random 1000)])
  (cond
    [(> (* bias 1000) x) #t]
    [else #f]
  )))

(define (test count bias)
  (if (= count 0) '() (cons (flip bias) (test (- count 1) bias))))

(define (try-flips count bias)
  (let ([lst (test count bias)])
 (cond
   [(< count 1) '(0 0)]
   [else (list
          (length (filter (lambda (x) (equal? #t x)) lst))
          (length (filter (lambda (x) (equal? #f x)) lst)))]
  )))

(define (pick lst)
  (let ([x (random (length lst))])
    (list-ref lst x)
  ))

(define (picktest count lst)
  (if (= count 0) '() (cons (pick lst) (picktest (- count 1) lst))))

;(define (run count lst)
;  (list (length (filter (lambda (x) (equal? (list-ref lst 0) x)) (picktest count lst)))
;        (length (filter (lambda (x) (equal? (list-ref lst 1) x)) (picktest count lst)))
;        (length (filter (lambda (x) (equal? (list-ref lst 2) x)) (picktest count lst)))
;        (length (filter (lambda (x) (equal? (list-ref lst 3) x)) (picktest count lst)))
;        (length (filter (lambda (x) (equal? (list-ref lst 4) x)) (picktest count lst)))
;        ))

;************************************************************
; ** problem 3 ** (20 points)
; Write a procedure

; (reg-exp-gen exp)

; that takes a Regular Expression exp and
; returns a random string in the language
; denoted by exp.  Every string in the language
; must have a positive probability of being chosen,
; and every string not in the language must have a
; probability of 0 of being chosen.

; Examples (yours may randomly differ):
;> (reg-exp-gen exp1)
;""
;> (reg-exp-gen exp2)
;"abbab"
;> (reg-exp-gen exp3)
;"abc"
;> (reg-exp-gen exp4)
;"001111110000111111111100"
;> (reg-exp-gen exp4)
;""
;> (reg-exp-gen exp5)
;"the mouse slept"
;> (reg-exp-gen exp6)
;"yoghurt"
;> (reg-exp-gen exp6)
;"yogurt"

;************************************************************
(define (randombignumber percentage number)
  (cond
    [(flip percentage) number]
    [else (randombignumber (* percentage 0.95) (+ number 1))])) 

(define (combine count exp)
  (if (= count 0) "" (string-append (reg-exp-gen exp) (combine (- count 1) exp))))

(define (reg-exp-gen exp)
  (cond
    [(string? exp) exp]
    [(alt? exp) (if (equal? (flip 0.5) #t) (reg-exp-gen (alt-arg1 exp))  (reg-exp-gen (alt-arg2 exp)))]
    [(con? exp) (string-append (reg-exp-gen (con-arg1 exp)) (reg-exp-gen (con-arg2 exp)))]
    [(ques? exp) (if (equal? (flip 0.5) #t) (reg-exp-gen "") (reg-exp-gen (ques-arg exp)))]
    [(rep? exp) (combine (randombignumber 0.5 0) (rep-arg exp))] 
  ))
;string-append
 
;************************************************************
; A (possibly incomplete) Deterministic Finite State Acceptor (DFA)
; is represented by the following struct.

(struct dfa (alph states start acc trans) #:transparent)

; where 
; alph is a list of Racket characters -- the symbols of the alphabet
; states is a list of Racket symbols
; start is one of the elements of states (the start state)
; acc is a list containing some of the elements of states (the accepting states)
; and trans is a table whose entries
;    have a key that is a list containing a state and a member of the alphabet
;         a value that is a state

(struct entry (key value) #:transparent)

; Examples of DFAs.
; Here is a DFA for the language of all strings of a's and b's with
; an odd number of a's and any number of b's.

(define odd-as
  (dfa
    '(#\a #\b)
    '(even odd)
    'even
    '(odd)
    (list
     (entry '(even #\a) 'odd)
     (entry '(even #\b) 'even)
     (entry '(odd #\a) 'even)
     (entry '(odd #\b) 'odd))))

; Here is an (incomplete) DFA to accept the language of the
; regular expression c(a|d)(a|d)*r

(define car-cdr
  (dfa
   '(#\a #\c #\d #\r)
   '(start saw-c saw-a-or-d saw-r)
   'start
   '(saw-r)
   (list
    (entry '(start #\c) 'saw-c)
    (entry '(saw-c #\a) 'saw-a-or-d)
    (entry '(saw-c #\d) 'saw-a-or-d)
    (entry '(saw-a-or-d #\a) 'saw-a-or-d)
    (entry '(saw-a-or-d #\d) 'saw-a-or-d)
    (entry '(saw-a-or-d #\r) 'saw-r))))

;************************************************************
; ** problem 4 ** (20 points)
; Write a procedure 

; (dfa-accepts? mach str)

; to take a DFA mach and a Racket string str and determine whether the
; DFA accepts the string.  Note that if an undefined transition
; is encountered, the string is rejected.

; Examples
;> (dfa-accepts? odd-as "aababa")
;#f
;> (dfa-accepts? odd-as "bbabbb")
;#t
;> (dfa-accepts? car-cdr "cadar")
;#t
;> (dfa-accepts? car-cdr "card")
;#f
;> (dfa-accepts? odd-as "what?")
;#f
;************************************************************

;> (string->list "Apple")
;'(#\A #\p #\p #\l #\e)
  

(define (findsymbol symbol symboltable)
  (cond
    [(empty? symboltable) #f]
    [(equal? symbol (entry-key (first symboltable))) (entry-value (first symboltable))]
    [else (findsymbol symbol (rest symboltable))]))
;(findsymbol '(even #\a) (dfa-trans odd-as))
;-> 'odd

(define (dfa-accepts? mach str)
  (dfa-helper mach (string->list str) (dfa-start mach))
  )

(define (dfa-helper mach lst position)
  (cond
    [(empty? lst) (if (member position (dfa-acc mach)) #t #f)]
    [(not (empty? lst)) (dfa-helper mach (rest lst) (findsymbol (list position (first lst)) (dfa-trans mach)))]
    [else #f]
    )) 
;maybe need to add undefined transitions. would this handle it
     
;************************************************************
; A Context Free Grammar (CFG) is represented using the following.

(struct cfg (nonterminals start rules) #:transparent)

(struct rule (lhs rhs) #:transparent)

; where
; nonterminals is a list of Racket symbols
; start is an element of the nonterminal list
; rules is a list of rule structs -- each of which has
; a lhs that is an element of the nonterminals list, and
; a rhs that is a list of elements that may be from the nonterminals list
;   or may be Racket strings

; Examples of CFGs.
; Here is an example CFG from lecture.

(define grammar-mcd
  (cfg
   '(s np vp det n pn vi vt v3)
   's
   (list
    (rule 's '(np vp))
    (rule 'np '(det n))
    (rule 'np '(pn))
    (rule 'det '("a"))
    (rule 'det '("the"))
    (rule 'n '("mouse"))
    (rule 'n '("cat"))
    (rule 'n '("dog"))
    (rule 'pn '("it"))
    (rule 'vp '(vi))
    (rule 'vp '(vt np))
    (rule 'vp '(v3 "that" s))
    (rule 'vi '("slept"))
    (rule 'vi '("swam"))
    (rule 'vt '("chased"))
    (rule 'vt '("evaded"))
    (rule 'v3 '("dreamed"))
    (rule 'v3 '("believed")))))

; Here is the grammar for the set of strings consisting of
; n a's followed by n b's, for all nonnegative integers n.

(define grammar-anbn
  (cfg
   '(s)
   's
   (list
    (rule 's '(""))
    (rule 's '("a" s "b")))))

; Here is a grammar that generates some statements in MiniJava
; This is not the same as the MiniJava grammar distributed in class.

(define grammar-mj
  (cfg
   '(block statements statement expression boolean-expression term identifier integer-literal)
   'block
   (list
    (rule 'block '("{" statements "}"))
    (rule 'statements '(statement))
    (rule 'statements '(statement statements))
    (rule 'statement '(identifier "=" expression ";"))
    (rule 'statement '("while" "(" boolean-expression ")" block))
    (rule 'statement '("System.out.prntln" "(" expression ")" ";"))
    (rule 'expression '(expression "+" term))
    (rule 'expression '(expression "-" term))
    (rule 'expression '(term))
    (rule 'boolean-expression '(term "<" term))
    (rule 'term '(identifier))
    (rule 'term '(integer-literal))
    (rule 'identifier '("sum"))
    (rule 'identifier '("n"))
    (rule 'integer-literal '("0"))
    (rule 'integer-literal '("1"))
    (rule 'integer-literal '("6")))))
 
;************************************************************
; ** problem 5 ** (20 points)
; Write a procedure

; (cfg-gen grammar)

; that takes a CFG grammar and produces a randomly chosen element of the language of the grammar.
; Every element in the language of the grammar should have a non-zero
; probability of being generated, and every element not in the language
; should have probability 0 of being generated.

; Hint: one way to approach this is to write an auxiliary procedure
; that takes a grammar and a list of nonterminal symbols and strings, and
; (1) returns the list of strings if there are no nonterminal symbols
; (2) calls itself recursively after replacing the leftmost
; nonterminal in the list by the righthand side of 
; a randomly chosen grammar rule which has that nonterminal as its lefthand side.

; Examples
;> (cfg-gen grammar-mcd)
;'("it" "dreamed" "that" "a" "mouse" "slept")
;> (cfg-gen grammar-anbn)
;'("a" "" "b")
;> (cfg-gen grammar-mj)
;'("{" "System.out.prntln" "(" "0" "-" "6" ")" ";" "}")

; For better-looking outputs:
;> (string-join (cfg-gen grammar-mcd))
;"it evaded it"
;> (apply string-append (cfg-gen grammar-anbn))
;""
;> (apply string-append (cfg-gen grammar-anbn))
;"ab"
;> (apply string-append (cfg-gen grammar-anbn))
;"aaaaabbbbb"
;> (string-join (cfg-gen grammar-mj))
;"{ System.out.prntln ( 6 ) ; while ( n < 1 ) { System.out.prntln ( 1 + 1 - sum + sum ) ; n = sum ; } }"

;************************************************************

(define (findrule nonterminal rules)
  (cond
    [(empty? rules) '(#f)]
    [(equal? nonterminal (rule-lhs (first rules))) (cons (rule-rhs (first rules)) (findrule nonterminal (rest rules)))]
    [else (findrule nonterminal (rest rules))]))

(define (runfindrule nonterminal rules)
  (if (equal? (first (findrule nonterminal rules)) #f) #f (reverse (rest (reverse (findrule nonterminal rules))))))

(define (cfg-gen grammar)
  (cleanlist (cfg-helper grammar (cfg-start grammar) (cfg-nonterminals grammar) (cfg-rules grammar))) 
  )


(define (cfg-helper grammar position nonterminals rules)
  (cond
    [(string? position) (list position)]
    [(and (list? position) (not (empty? position)))
     (cons
      (cfg-helper grammar (first position) nonterminals rules)
      (cfg-helper grammar (rest position) nonterminals rules))]
    [(and (symbol? position) (member position nonterminals))
     (cfg-helper grammar (pick (runfindrule position rules)) nonterminals rules)]
    [else '()]
    ))

(define (cleanlist lst)
  (cond
    [(empty? lst) '()]
    [(string? lst) (list lst)]
    [(list? lst) (append (cleanlist (first lst)) (cleanlist (rest lst)))]
    [else (append (first lst) (cleanlist (rest lst)))]))

;************************************************************
; ** problem 6 ** (10 points)
; Define your own CFG named my-cfg of complexity at least as great as 
; that of grammar-mcd.  Give (as comments) some examples of sentences 
; generated by your grammar. 
; (Please do more than just copy grammar-mcd with a few changes!!!)
;************************************************************
(define my-cfg
  (cfg
   '(s a b c d e f g h)
   's
   (list
    (rule 's '(a b c d e))
    (rule 'a '("Dana Angluin"))
    (rule 'a '("Anyone but Stephen Krewson isn't"))
    (rule 'b '(" is"))
    (rule 'b '(" has"))
    (rule 'b '(" teaches"))
    (rule 'b '(c))
    (rule 'b '(d e))
    (rule 'c '(" a"))
    (rule 'c '(" the"))
    (rule 'c '(" "))
    (rule 'd '(d " very"))
    (rule 'd '(" "))
    (rule 'd '(" such"))
    (rule 'd '(d " really"))
    (rule 'e '(" fantastic teacher"))
    (rule 'e '(" wants to give me an A"))
    (rule 'e '(" such great lecture notes"))
    (rule 'e '(" great lecturer"))
    (rule 'e '(" fantastically")))))

;Dana Angluin is (ques such) a fantastic teacher
;Dana Angluin is the *very* best teacher
;Dana Angluin *really* wants to give me an A
;Dana Angluin has such great lecture notes
;Dana Angluin is a *very* great lecturer
;Dana Angluin teaches fantastically

;part a: noun
;Dana Angluin 
;part b: verb
;is, has, teaches
;part c:
;a, the, “”
;part d: 
;very*, (ques such), really*
;part e:
;fantastic teacher, best teacher, wants to give me an A, such great great lecture notes, great lecturer, fantastically

;examples:
;"Dana Angluin is  such wants to give me an A"
;"Dana Angluin teaches a  great lecturer"
;"Dana Angluin teaches a  great lecturer"
;"Dana Angluin has a  wants to give me an A"

;************************************************************
; ** problem 7 ** (10 points)
; Write a procedure

; (reg-exp->cfg exp)

; that takes a regular expression exp
; returns a context free grammar for the language denoted by exp

; Examples (yours may differ randomly)

;> (apply string-append (cfg-gen (reg-exp->cfg exp1)))
;""
;> (apply string-append (cfg-gen (reg-exp->cfg exp2)))
;"abbab"
;> (apply string-append (cfg-gen (reg-exp->cfg exp3)))
;"abd"
;> (apply string-append (cfg-gen (reg-exp->cfg exp4)))
;"1100"
;> (apply string-append (cfg-gen (reg-exp->cfg exp5)))
;"the mouse ran"
;> (apply string-append (cfg-gen (reg-exp->cfg exp6)))
;"yogurt"
;************************************************************

(define (reg-exp->cfg exp)
  (cfg (reverse (listmaker (findmax (regexphelper exp 0) 0))) 'a0 (regexphelper exp 0)))
;update abcdef eventually

(define (findmax lst num)
 (cond
    [(empty? lst) num]
    [(> (string->number (substring (symbol->string (rule-lhs (first lst))) 1 (string-length (symbol->string (rule-lhs (first lst)))))) num) (findmax (rest lst) (string->number (substring (symbol->string (rule-lhs (first lst))) 1 (string-length (symbol->string (rule-lhs (first lst)))))))]
    [else (findmax (rest lst) num)]))

(define (listmaker num)
  (cond
    [(< num 0) '()]
    [else (cons (c num) (listmaker (- num 1)))]))
;reverse this list

;(substring "a0" 1 (string-length (symbol->string 'a0)))
     ;(string->number "0")
(define (c count)
  (string->symbol (string-append "a" (number->string count))))

(define (regexphelper exp count)
  (cond
  [(string? exp) (list (rule (c count) (list exp)))]
    [(alt? exp)   (cond
                    [(and (string? (alt-arg1 exp)) (string? (alt-arg2 exp)))
                     (list (rule (c count) (list (alt-arg1 exp))) (rule (c count) (list (alt-arg2 exp))))]
                    [(and (string? (alt-arg1 exp)) (not (string? (alt-arg2 exp))))
                     (append
                      (list (rule (c count) (list (alt-arg1 exp))) (rule (c count) (list (c (+ count 1)))))
                      (regexphelper (alt-arg2 exp) (+ count 1 (countupfunction (alt-arg1 exp)))))]
                    [(and (not (string? (alt-arg1 exp))) (string? (alt-arg2 exp)))
                     (append
                      (list (rule (c count) (list (c (+ count 1)))) (rule (c count) (list (alt-arg2 exp))))
                      (regexphelper (alt-arg1 exp) (+ count 1)))]
                    [else
                     (append
                      (list (rule (c count) (list (c (+ count 1)))) (rule (c count) (list (c (+ count 1 (countupfunction (alt-arg1 exp)))))))
                      (regexphelper (alt-arg1 exp) (+ count 1))
                      (regexphelper (alt-arg2 exp) (+ count (countupfunction (alt-arg1 exp)) 1)))])]
    [(con? exp)     (cond
                       [(and (string? (con-arg1 exp)) (string? (con-arg2 exp)))
                        (list (rule (c count) (list (con-arg1 exp) (con-arg2 exp))))]
                       [(and (string? (con-arg1 exp)) (not (string? (con-arg2 exp))))
                        (append
                         (list (rule (c count) (list (con-arg1 exp) (c (+ count 1)))))
                         (regexphelper (con-arg2 exp) (+ count 1 (countupfunction (con-arg1 exp)))))]
                       [(and (not (string? (con-arg1 exp))) (string? (con-arg2 exp)))
                        (append
                         (list (rule (c count) (list (c (+ count 1)) (con-arg2 exp)))) 
                         (regexphelper (con-arg1 exp) (+ count 1)))]
                       [else
                        (append
                         (list (rule (c count) (list (c (+ count 1)) (c (+ count 1 (countupfunction (con-arg1 exp)))))))
                         (regexphelper (con-arg1 exp) (+ count 1))
                         (regexphelper (con-arg2 exp) (+ count (countupfunction (con-arg1 exp)) 1)))])]
    [(ques? exp)     (cond
                       [(string? (ques-arg exp))                    
                        (list (rule (c count) (list "")) (rule (c count) (list (ques-arg exp))))]
                       [else
                        (append 
                        (list (rule (c count) (list "")) (rule (c count) (list (ques-arg exp))))
                        (regexphelper (ques-arg exp) (+ count 1)))])]
    [(rep? exp)      (cond
                       [(string? (rep-arg exp))
                        (list (rule (c count) (list "")) (rule (c count) (list (rep-arg exp) (c count))))]
                       [else
                        (append
                         (list (rule (c count) (list "")) (rule (c count) (list (c (+ count 1)) (c count))))
                         (regexphelper (rep-arg exp) (+ count 1)))])]
    ))

(define (countupfunction exp)
  (cond
    [(string? exp) 0]
    [(alt? exp) (+ 1 (countupfunction (alt-arg1 exp)) (countupfunction (alt-arg2 exp)))]
    [(con? exp) (+ 1 (countupfunction (con-arg1 exp)) (countupfunction (con-arg2 exp)))]
    [(ques? exp) (+ 0 (countupfunction (ques-arg exp)))]
    [(rep? exp) (+ 0 (countupfunction (rep-arg exp)))]))


(define exp1 "")
(define exp2 "abbab")
(define exp3 (con "ab" (alt "c" "d")))
(define exp4 (rep (alt "00" "11")))
(define exp5 (con (alt "a" "the")
                  (con (alt " mouse" " cat") 
                       (alt " ran" " slept"))))
(define exp6 (con "yog" (con (ques "h") "urt")))
(define exp7 (alt (alt "00" "11") "10"))
(define exp8 (alt "00" "11"))
(define exp9 (alt "x" (alt "y" "z")))
(define exp10 (alt (alt (alt "x" "y") "z") (alt "a" "b")))
(define exp11 (alt (alt "y" "z") "x"))
(define exp12 (alt (alt "a" "b") (alt "c" "d")))
(define exp13 (con "ab" (con "c" "d")))
(define exp14 (con (con "c" "d") "ab"))
(define exp15 (con (con "c" "d") (con "e" "f")))
(define exp16 (con (con (con "a" "b") "c") (con "e" "f")))
(define exp17 (con "yog" (con (ques "h") "urt")))
(define exp51 (alt (alt (alt "a" "b") "c") (alt "d" "e")))

;************************************************************