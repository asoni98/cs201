#lang racket

(provide hours
         entry entry? entry-key entry-value
         symbol-table
         bin->n tc->n n->bin n->tc
         assemble-one assemble
         ram-read ram-write equal-rams?
         conf conf? conf-cpu conf-ram
         cpu cpu? cpu-acc cpu-pc cpu-rf cpu-aeb
         equal-configs? addr->pc add-to-pc 
         acc->mem mem->acc
         sum diff
         do-input do-output
         next-config
         init-config simulate run
         prog-sort-two prog-reverse)

;************************************************************
; CS 201 HW #5 due Wednesday, April 5, 2017 at 11:59 pm, 
; via the submit system on the Zoo.
;************************************************************
; Name: Arun Soni
; Email address: arun.soni@yale.edu
;************************************************************
; Computer science topics: TC-201 assembler and simulator,
; assembly language programs for sorting two numbers and
; reading in and printing out a list of numbers in reverse order.
; 
; ** You may solve the problem using any Racket constructs 
;  except mutators (set! and its relatives.)  Please do not use require.
; ** You may write auxiliary procedure(s) in addition to the one(s) 
; specified in the problem.  Please include a succint comment for
; each one specifying its intended inputs and return values.
; ** Please make your code as clear and readable as possible.

;************************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 18)

;************************************************************

; A table is a list of entries, where each entry has two fields: key and value.
; The constructor for entries is entry, the type predicate is entry?, and the
; two selectors are entry-key and entry-value.

(struct entry (key value) #:transparent)

;************************************************************
; We'll start by writing a TC-201 assembler, that is,
; a procedure that takes a symbolic assembly language
; program as input and returns as output the corresponding
; list of 16-bit words representing TC-201 instructions and data.

; As an example, here is a version of the program we wrote in lecture
; to sum up a zero-terminated sequence of numbers, output the sum, and halt.
; Note that it initializes sum to 0 before beginning the read loop.

(define prog-sum
  '((start:  load constant-0)
   (         store sum)
   (next:    input)
   (         skipzero)
   (         jump add-num)
   (         load sum)
   (         output)
   (         halt)
   (add-num: add sum)
   (         store sum)
   (         jump next)
   (sum:     data 0)
   (constant-0: data 0)))

; Here is the result of assembling this program

;> (assemble prog-sum)
;'((0 0 0 1 0 0 0 0 0 0 0 0 1 1 0 0)
;  (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
;  (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 1 1 1 0 0 0 0 0 0 0 0 1 0 0 0)
;  (0 0 0 1 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

; Here are two simpler test programs.
; First, a program with only instructions, 
; numeric addresses, and no labels.

(define prog1
  '((load 3)
    (store 4)
    (halt)))

; Second, a program with only data statements, three labels, and both numeric
; and symbolic data values

(define prog2
  '((x: data 7)
    (y: data -6)
    (z: data y)))

; Here are the values returned by assemble on these two programs.

;> (assemble prog1)
;'((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;  (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

;> (assemble prog2)
;'((0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1)
;  (1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

; Note the two's complement representation of -6, and
; the fact that the label y translates to the number 1
; in this example.

;************************************************************
; A symbolic assembly-language program is a list of lists, with
; each list representing one instruction or data statement.
; An instruction or data statement may optionally have a label,
; that is, a symbol ending in colon (:), which is the first
; element of the list.  The next symbol must be one of the
; opcodes (in the table opcode-table, below) or the symbol 'data.

; For the opcodes load, store, add, sub, jump, loadi, storei,
; there is one more field, the address field, which may be 
; a label (defined somewhere in the program) or a 
; decimal number between 0 and 4095 inclusive.
; For the other opcodes, there is no additional field.
; For the data directive, there is one more field, the
; value, which may be a label (defined somewhere in the
; program) or a decimal number between -32768 and 32767
; inclusive.

;************************************************************
; ** problem 1 ** (9 points)
; Write a procedure

; (symbol-table prog)

; that takes a symbolic assembly-language program prog
; as input, and returns a table with entries listing
; (in order) the labels defined in the program and their
; corresponding numeric values (instructions and data
; statements are numbered from 0.)

; Note that when they are defined, the labels have a colon (:)
; at the end, and when they are in the symbol table the
; final colon is removed.

; You will probably want to write one or more auxiliary
; procedures to deal with labels.  The procedures
; symbol->string, string->symbol, string-length, string-ref
; and substring will be useful, together with character representations.
; (See the Racket documentation.)

; Examples
;> (symbol-table prog1)
;'()

;> (symbol-table prog2)
;(list (entry 'x 0) (entry 'y 1) (entry 'z 2))

;> (symbol-table prog-sum)
;(list (entry 'start 0) (entry 'next 2) (entry 'add-num 8) (entry 'sum 11) (entry 'constant-0 12))
;************************************************************

(define (symbol-table prog)
  (test-first prog 0)
  )

;first of first contains : then include it otherwise don't

(define (test-first prog n)
  (cond
    [(empty? prog) '()]
    [(check-if-colon? (symbol->string (first (first prog))) 0)
     (cons (entry (string->symbol (check-if-colon? (symbol->string (first (first prog))) 0)) n) (test-first (rest prog) (+ n 1)))]
    [else (test-first (rest prog) (+ n 1))]
    ))

(define (check-if-colon? prog n)
  (cond
    [(equal? n (string-length prog)) #f]
    [(equal? (string-ref prog n) #\:) (substring prog 0 (- (string-length prog) 1))]
    [else (check-if-colon? prog (+ n 1))]))
     

;************************************************************
; Next we look at converting between decimal numbers and
; lists of binary digits representing integers in unsigned
; binary and two's complement representations.

;************************************************************
; ** problem 2 ** (10 points)
; Write four procedures:

; (bin->n lst)
; takes a list of binary digits and returns the nonnegative
; integer that they represent in unsigned binary in base 2.

; (tc->n lst)
; takes a list of k binary digits and returns the negative, zero, or
; positive number that they represent in k-bit two's complement representation.
; You may assume k is at least 2.

; (n->bin n len)
; takes a nonnegative integer n and returns a list of len binary digits
; representing n in unsigned binary.  If necessary, the representation 
; is padded on the left with 0's.  If the number n cannot be represented
; correctly in unsigned binary using len bits, the symbol 'error is returned.

; (n->tc n len)
; If the negative, zero, or positive integer n can be correctly represented
; in two's complement binary representation with len bits, a list of binary digits
; giving that representation is returned.  Otherwise, the symbol 'error is returned.

; Examples
;> (bin->n '(0 0 1 1))
;3
;> (bin->n '(1 1 1 1 1))
;31
;> (tc->n '(0 0 1 1))
;3
;> (tc->n '(1 1 0 1))
;-3
;> (tc->n '(1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1))
;-7
;> (n->bin 13 5)
;'(0 1 1 0 1)
;> (n->bin 13 3)
;'error
;> (n->tc 13 16)
;'(0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1)
;> (n->tc -6 16)
;'(1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0)
;> (n->tc 17 4)
;'error
;************************************************************

(define (bin->n lst)
  (binhelper lst (- (length lst) 1) 0))

(define (binhelper lst n l)
  (cond
    [(< n 0) 0]
    [(equal? (list-ref lst n) 0) (binhelper lst (- n 1) (+ l 1))]
    [(equal? (list-ref lst n) 1) (+ (expt 2 l) (binhelper lst (- n 1) (+ l 1)))]))

(define (tc->n lst)
  (cond
    [(equal? (first lst) 0) (bin->n lst)]
    [(equal? (first lst) 1) (* -1 (bin->n (reverse (addone (flip lst) (- (length lst) 1) 0))))]))
;flip all numbers. add one. then run through bin->n lst

(define (flip lst)
  (cond
    [(empty? lst) '()]
    [(equal? (first lst) 1) (cons 0 (flip (rest lst)))]
    [(equal? (first lst) 0) (cons 1 (flip (rest lst)))]))
;flips all the numbers in a list from 0 to 1 and 1 to 0

(define (addone lst n l)
  (cond
    [(and (< n 0) (= l 1)) '()]
    [(and (< n 0) (= l 0)) '(1)]
    [(equal? l 1) (cons (list-ref lst n) (addone lst (- n 1) 1))]
    [(equal? (list-ref lst n) 1) (cons 0 (addone lst (- n 1) 0))]
    [(equal? (list-ref lst n) 0) (cons 1 (addone lst (- n 1) 1))]))
;outputs a list + 1 but in reverse

;(n->bin 13 5)
;'(0 1 1 0 1)

(define (n->bin n len)
  (cond
    [(equal? (length (n->binh n len)) len) (reverse (n->binh n len))]
    [(> (length (n->binh n len)) len) 'error]
    [(< (length (n->binh n len)) len) (cons 0 (n->bin n (- len 1)))]))

(define (n->binh n len)
  (cond
    [(= n 0) '()]
    [(= (remainder n 2) 0) (cons 0 (n->binh (quotient n 2) len))]
    [(= (remainder n 2) 1) (cons 1 (n->binh (quotient n 2) len))]))


(define (n->tcerror n len)
  (cond
    [(>= n 0) (n->bin n len)]
    [(and (< n 0) (> (length (n->binh (* n -1) len)) len)) 'error]
 ;   [(and (< n 0) (> (expt 2 (- len 1)) (* n -1))) 'error]
    [else (reverse (addone (flip (n->bin (* -1 n) len)) (- (length (n->bin (* n -1) len)) 1) 0))]
  ))

(define (n->tc n len)
  (cond
    [(equal? 'error (n->tcerror n len)) 'error]
    [(equal? (tc->n (n->tcerror n len)) n) (n->tcerror n len)]
    [else 'error]))

;check this case (n->tc -3 2)

;************************************************************
; Now we create a procedure to assemble one
; line of a program (given the symbol table), and use that
; to assemble the whole program.

;************************************************************
; ** problem 3 ** (10 points)
; Write two procedures:

; (assemble-one line table)
; takes one line (instruction or data statement) from a program
; and a symbol table for the program
; and returns a list of 16 bits representing that line of the program.

; (assemble prog)
; takes a symbolic assembly-language program prog and returns
; a list of 16-bit lists, one for each line of the program, giving
; the machine language version of the program.

;************************************************************
; Here is a useful table of the TC-201 opcodes and their
; corresponding 4-bit representations.

(define opcode-table
  (list
   (entry 'halt '(0 0 0 0))
   (entry 'load '(0 0 0 1))
   (entry 'store '(0 0 1 0))
   (entry 'add '(0 0 1 1))
   (entry 'sub '(0 1 0 0))
   (entry 'input '(0 1 0 1))
   (entry 'output '(0 1 1 0))
   (entry 'jump '(0 1 1 1))
   (entry 'skipzero '(1 0 0 0))
   (entry 'skippos '(1 0 0 1))
   (entry 'skiperr '(1 0 1 0))
   (entry 'loadi '(1 0 1 1))
   (entry 'storei '(1 1 0 0))))
;************************************************************

;> (symbol-table prog-sum)
;(list (entry 'start 0) (entry 'next 2) (entry 'add-num 8) (entry 'sum 11) (entry 'constant-0 12))

;(define (assemble-one line table)
;  (cond
;  [(empty? table) '()]
;  [(empty? line) '()]
;  [(= (length (first (removecolon line)) 2)) (number? (first (rest (first prog))))
;  [(= (length (first (removecolon line)) 1))
;  [(not (empty? (symbol-table (list line)))) (assemble-one (rest line) table)]
;  [(equal? (first line) 'data) (n->tc (first (rest line)) 16)]
 ; [(equal? (first (rest line)) 'data) (n->tc (first (rest (rest line))) 16)]
;  [(and (equal? (first line) (entry-key (first table))) (empty? (rest line))) (append (entry-value (first table)) '(0 0 0 0 0 0 0 0 0 0 0 0))]
;  [(equal? (first line) (entry-key (first table))) (append (entry-value (first table)) (n->tc (first (rest line)) 12))]
;  [else (assemble-one line (rest table))]
;  ))

(define (assemble-one line table)
  (cond
    [(empty? line) '()]
    [else (first (helperassembly (removecolon (list line)) table 0))]))

;(define (assemble-one line table)
 ; (helperassembly (removecolon (list line)) (symbol-table (list line)) 0))
;all I have coded for is removing the first element if it has a ":".
;then if it is data I make the entire 16 bit word the number after data.
;otherwise I search the table for the functions like halt, load and put their opcode
;as the first four bytes and 12 0’s after if they don’t have a number after them

(define (findsymbol symbol symboltable)
  (cond
    [(empty? symboltable) #f]
    [(equal? symbol (entry-key (first symboltable))) (entry-value (first symboltable))]
    [else (findsymbol symbol (rest symboltable))]))

(define (removecolon prog)
  (cond
    [(empty? prog) '()]
    [(not (empty? (symbol-table (list (first prog))))) (cons (rest (first prog)) (removecolon (rest prog)))]
    [else (cons (first prog) (removecolon (rest prog)))]))


;(define prog2
 ; '((x: data 7)
 ;   (y: data -6)
 ;   (z: data y)))

;(define prog-sum
;  '((start:  load constant-0)
;   (         store sum)
;   (next:    input)
;   (         skipzero)
;   (         jump add-num)
;   (         load sum)
;   (         output)
;   (         halt)
;   (add-num: add sum)
;   (         store sum)
;   (         jump next)
;   (sum:     data 0)
;   (constant-0: data 0)))


(define (helperassembly prog symboltable n)
  (cond
    [(empty? prog) '()]
    [(= (length (first prog)) 2) (cond
                                   [(and (equal? 'data (first (first prog))) (number? (first (rest (first prog)))))
                                    (cons 
                                                                                 (n->tc (first (rest (first prog))) 16)
                                                                                (helperassembly (rest prog) symboltable 0))]
                                   [(and (equal? 'data (first (first prog))) (not (number? (first (rest (first prog))))))
                                    (cons (n->tc (findsymbol (first (rest (first prog))) symboltable) 16)
                                                                                (helperassembly (rest prog) symboltable 0))]
                                   [(and (not (equal? 'data (first (first prog)))) (number? (first (rest (first prog)))))
                                    (cons (append
                                                 (findsymbol (first (first prog)) opcode-table)
                                                 (n->tc (first (rest (first prog))) 12))
                                                                                (helperassembly (rest prog) symboltable 0))]
                                   [else (cons (append (findsymbol (first (first prog)) opcode-table)
                                                       (n->tc (findsymbol (first (rest (first prog))) symboltable) 12))
                                               (helperassembly (rest prog) symboltable 0))])]
    [(= (length (first prog)) 1) (cons (append (findsymbol (first (first prog)) opcode-table) '(0 0 0 0 0 0 0 0 0 0 0 0)) (helperassembly (rest prog) symboltable 0))]))

(define (assemble prog)
  (helperassembly (removecolon prog) (symbol-table prog) 0)
  )
;I WROTE THIS ONLY FOR OPCODETABLE. should I redo that



;(findsymbol 'y (symbol-table '((y: load -6) (store 3) (halt 4))))
;(assemble '((load -6) (store 3) (halt 4)))

;(define (helperassembler prog symboltable n)
 ; (cond
  ;  [(empty? prog) '()]
   ; [(not (empty? (symbol-table (list (first prog))))) (cons (helperassembler (first (rest prog)) symboltable 0) (helperassembler (rest prog) symboltable n))]
;    [(empty? (symbol-table (list (first prog)))) 
   ; [(empty? (rest (first prog))) (cons (assemble-one (first prog) opcode-table) (helperassembler (rest prog) symboltable 0))] 
   ; [(number? (rest (first prog))) (cons (assemble-one (first prog) opcode-table) (helperassembler (rest prog) symboltable 0))]
    ;[(findsymbol (first (rest (first prog))) opcode-table) (assemble-one (cons (first (first prog)) (findsymbol (first (rest (first prog))))) opcode-table)]
    ;[else (helperassembler (rest prog) symboltable 0)]
  ;))

;    [(equal? (rest (first prog)) (entry-key (first symboltable)))
 ;   [else (helperassembler prog (rest symboltable 0))]


 ;************************************************************
; Now that we can produce machine language from symbolic assembly-language
; programs, we'll create a simulator that can execute the machine
; language instructions step by step.  First, we specify a representation of
; of the random access memory (RAM) and procedures to read
; and write it.

;************************************************************
; Random access memory (RAM)

; The contents of RAM are represented by a table
; in which the key is a nonnegative integer in the range
; 0 through 4095 (the memory address), and the value is a list of 16 bits
; (the bits stored by the register with that address.)
; No address may appear twice.  The contents of any register
; whose address does not appear as a key is assumed to contain 16 zeroes.

;************************************************************
; ** problem 4 ** (10 points)
; Write three procedures

; (ram-read address ram)
; takes a memory address and a ram
; and returns a list of 16 bits giving the contents
; of the memory register in ram with the given address.

; (ram-write address contents ram)
; takes a memory address (address), a list of 16 bits (contents) and a ram,
; and returns a ram representing the result of copying the contents 
; into the memory register of ram specified by the memory address.

; (equal-rams? ram1 ram2)
; takes two rams and compares their contents, returning
; #t if they are equal and #f if they are unequal.

; Examples
(define ram1
  (list
   (entry 0 '(0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1))
   (entry 1 '(0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 0))
   (entry 2 '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
   (entry 3 '(1 1 1 1  0 0 0 0  1 1 1 1  0 0 0 0))
   (entry 4 '(0 0 0 1  0 0 1 1  0 1 1 1  1 1 1 1))))

(define ram2
  (list
   (entry 1 '(0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 0))
   (entry 4 '(0 0 0 1  0 0 1 1  0 1 1 1  1 1 1 1))
   (entry 0 '(0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1))
   (entry 3 '(1 1 1 1  0 0 0 0  1 1 1 1  0 0 0 0))))

(define ram3
  (list
   (entry 0 '(1 0 1 1  0 0 0 0  0 0 0 0  0 1 0 0))
   (entry 1 '(1 1 0 0  0 0 0 0  0 0 0 0  0 1 0 1))
   (entry 2 '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
   (entry 4 '(0 0 0 0  0 0 0 0  0 0 0 0  0 1 1 1))
   (entry 5 '(0 0 0 0  0 0 0 0  0 0 0 0  1 0 1 0))
   (entry 7 '(1 1 1 1  1 1 1 1  0 0 0 0  0 0 0 0))
   (entry 10 '(1 0 1 0  0 0 0 0  0 1 0 1  1 1 1 1))))

;> (ram-read 0 ram1)
;'(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;> (ram-read 2 ram2)
;'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;> (ram-write 5 '(1 1 0 0  0 0 1 1  1 1 0 0  0 0 1 1) ram2)
;(list
; (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
; (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
; (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
; (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
; (entry 5 '(1 1 0 0 0 0 1 1 1 1 0 0 0 0 1 1)))
;> (ram-write 10 '(1 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1) ram3)
;(list
; (entry 0 '(1 0 1 1 0 0 0 0 0 0 0 0 0 1 0 0))
; (entry 1 '(1 1 0 0 0 0 0 0 0 0 0 0 0 1 0 1))
; (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1))
; (entry 5 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
; (entry 7 '(1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0))
; (entry 10 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
;> (equal-rams? ram1 ram2)
;#t
;> (equal-rams? ram2 ram3)
;#f

;************************************************************
;(findsymbol symbol symboltable)

(define (ram-read address ram)
  (cond
    [(findsymbol address ram) (findsymbol address ram)]
    [else '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)]))

(define (ram-write address contents ram)
  (ram-writeh address contents ram 0))
  
(define (ram-writeh address contents ram switch)
  (cond
    [(and (empty? ram) (= switch 1))  '()]
    [(and (empty? ram) (= switch 0)) (list (entry address contents))]
    [(equal? (entry-key (first ram)) address) (cons (entry address contents) (finish address contents (rest ram) 1))]
    [else (cons (entry (entry-key (first ram)) (entry-value (first ram))) (ram-writeh address contents (rest ram) 0))]
  ))

(define (finish address contents ram switch)
  (cond
    [(empty? ram)  '()]
    [else (cons (entry (entry-key (first ram)) (entry-value (first ram))) (finish address contents (rest ram) 0))]))
    

;> (ram-write 5 '(1 1 0 0  0 0 1 1  1 1 0 0  0 0 1 1) ram2)
;(list
; (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
; (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
; (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
; (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
; (entry 5 '(1 1 0 0 0 0 1 1 1 1 0 0 0 0 1 1)))

(define (cleanram ram)
  (cond
    [(empty? ram) '()]
    [(equal? (findsymbol (entry-key (first ram)) ram) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) (cleanram (rest ram))]
    [else (cons (entry (entry-key (first ram)) (entry-value (first ram))) (cleanram (rest ram)))]))

(define (equal-rams? ram1 ram2)
  (equal-ramsh? (cleanram ram1) (cleanram ram2) (length (cleanram ram1)) (length (cleanram ram2))))

(define (equal-ramsh? ram1 ram2 n1 n2)
  (cond
    [(not (equal? n1 n2)) #f]
    [(empty? ram1) #t]
    [(equal? (findsymbol (entry-key (first ram1)) ram1) (findsymbol (entry-key (first ram1)) ram2)) (equal-ramsh? (rest ram1) ram2 1 1)]
    [else #f]
  ))

;************************************************************
; For the TC-201 Central Processing Unit (CPU), 
; the contents of the registers are represented by a struct with 4 fields 
; giving the values of the CPU registers:

; the accumulator (acc)
; the program counter (pc)
; the run flag (rf)
; the arithmetic error bit (aeb)

(struct cpu (acc pc rf aeb) #:transparent)

; Each field contains a list of bits of the correct length
; giving the value of the corresponding register; 16 bits for
; the acc, 12 bits for the pc, 1 bit each for the rf and the aeb.
; The constructor is cpu, the type predicate is cpu?, and
; the selectors are cpu-acc, cpu-pc, cpu-rf, cpu-aeb.

; Examples

; The accumulator has value 15, the program counter has value 7,
; the run flag is 1 and the arithmetic error bit is 0.
(define cpu1 
  (cpu
   '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
   '(0 0 0 0 0 0 0 0 0 1 1 1)
   '(1)
   '(0)))

; The accumulator has value -3, the program counter has value 7,
; the run flag is 1 and the arithmetic error bit is 1.
(define cpu2 
  (cpu
   '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1)
   '(0 0 0 0 0 0 0 0 0 1 1 1)
   '(1)
   '(1)))

;************************************************************
; A configuration of the TC-201 is a struct with two fields:
; (1) the contents of the CPU registers, in the above format, and
; (2) the contents of the RAM, in the format of problem 4.

(struct conf (cpu ram) #:transparent)

; Note that the constructor is conf, the type-predicate
; is conf?, and the selectors are conf-cpu, conf-ram.

;************************************************************
; ** problem 5 ** (10 points)
; Write three procedures

; (equal-configs? config1 config2)
; takes two configurations config1 and config2, and returns
; #t if they represent the same contents of the RAM and the CPU registers,
; and returns #f otherwise.

; (addr->pc addr config)
; takes a configuration and a memory address addr (a number
; in the range 0 to 4095 inclusive), and returns a new configuration
; in which the program counter is set to the given address.
; No other registers are changed.

; (add-to-pc n config)
; takes a nonnegative integer n and a TC-201 configuration config
; and returns the TC-201 configuration that is obtained by adding n 
; to the value of pc.  Note that the sum should be taken modulo 4096.  
; (Racket has a modulo procedure.)

; Example configurations

(define config1
  (conf cpu1 ram1))

(define config2
  (conf cpu1 ram2))

(define config3
  (conf cpu2 ram2))

; Examples of procedures

;> (equal-configs? config1 config2)
;#t
;> (equal-configs? config2 config3)
;#f
;> (addr->pc 5 config1)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) '(0 0 0 0 0 0 0 0 0 1 0 1) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))))
;> (addr->pc 1 config3)
;(conf
; (cpu '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1) '(0 0 0 0 0 0 0 0 0 0 0 1) '(1) '(1))
; (list
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
 ; (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))))
;> (add-to-pc 1 config2)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) '(0 0 0 0 0 0 0 0 1 0 0 0) '(1) '(0))
; (list
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))))
;> (add-to-pc 4093 config1)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) '(0 0 0 0 0 0 0 0 0 1 0 0) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))))

;************************************************************

(define (equal-configs? config1 config2)
  (and (equal? (cpu-acc (conf-cpu config1)) (cpu-acc (conf-cpu config2)))
     (equal? (cpu-pc (conf-cpu config1)) (cpu-pc (conf-cpu config2)))
     (equal? (cpu-rf (conf-cpu config1)) (cpu-rf (conf-cpu config2)))
     (equal? (cpu-aeb (conf-cpu config1)) (cpu-aeb (conf-cpu config2)))
     (equal-rams? (conf-ram config1) (conf-ram config2))))
;make sure cpu and ram are equal
;use (equal-ram? to check if two rams are equal)

(define (addr->pc addr config)
  (conf (cpu (cpu-acc (conf-cpu config)) (n->tc addr 12)  (cpu-rf (conf-cpu config)) (cpu-aeb (conf-cpu config))) (conf-ram config))
  )

(define (add-to-pc n config)
   (conf (cpu (cpu-acc (conf-cpu config))
              (n->tc (modulo (+ (tc->n (cpu-pc (conf-cpu config))) n) 4096) 12)
              (cpu-rf (conf-cpu config))
              (cpu-aeb (conf-cpu config)))
         (conf-ram config)))

;************************************************************
; ** problem 6 ** (10 points)
; Write two procedures

; (acc->mem addr config)
; takes a memory address and a configuration, and
; returns the configuration in which the contents of the accumulator
; are copied to the addressed memory register.
; No other registers change value.

; (mem->acc addr config)
; that takes a memory address and a configuration, and
; returns the configuration in which the contents of the addressed
; memory register are copied to the accumulator.
; No other registers change value.

; Examples
;> (acc->mem 3 config1)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) '(0 0 0 0 0 0 0 0 0 1 1 1) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))))
;> (acc->mem 13 config3)
;(conf
; (cpu '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1) '(0 0 0 0 0 0 0 0 0 1 1 1) '(1) '(1))
; (list
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
;  (entry 13 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1))))
;> (mem->acc 4 config1)
;(conf
; (cpu '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1) '(0 0 0 0 0 0 0 0 0 1 1 1) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))))
;> (mem->acc 12 config3)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 1 1 1) '(1) '(1))
; (list
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))))
;************************************************************

(define (acc->mem addr config)
  (conf (cpu (cpu-acc (conf-cpu config))
             (cpu-pc (conf-cpu config))
             (cpu-rf (conf-cpu config))
             (cpu-aeb (conf-cpu config)))
        (ram-write addr (cpu-acc (conf-cpu config)) (conf-ram config)))
  )



(define (mem->acc addr config)
   (conf (cpu (ram-read addr (conf-ram config)) 
             (cpu-pc (conf-cpu config))
             (cpu-rf (conf-cpu config))
             (cpu-aeb (conf-cpu config)))
         (conf-ram config)))

;    (conf (cpu (cpu-acc (conf-cpu config))
;             (cpu-pc (conf-cpu config))
;             (cpu-rf (conf-cpu config))
;             (cpu-aeb (conf-cpu config)))
;         (conf-ram config)))

;************************************************************
; ** problem 7 ** (10 points)
; Write two procedures

; (sum tc1 tc2)
; takes two lists of bits, tc1 and tc2, of the same length, k,
; representing two numbers in k-bit two's complement,
; and returns two values, in a list.
; If the sum of the two numbers can be correctly represented in k-bit
; two's complement, then the first value in the list is #t and the second
; value is the k-bit two's complement representation of the sum.
; If the sum of the two numbers cannot be correctly represented in k-bit
; two's complement, then the first value in the list is #f and the
; second value is a list of k zeroes.

; (diff tc1 tc2)
; is analogous to (sum tc1 tc2), except that, instead of the
; sum of the numbers represented by tc1 and tc2, the value
; computed is their difference, that is, the number represented
; by tc1 minus the number represented by tc2.
; The format of the result is the same: a list with #t and the
; k-bit two's complement representation of the difference, or
; #f and a list of k zeroes.

; For both procedures, you may assume that tc1 and tc2 are 
; lists of bits of equal length, and that the length is at least 2.

; Examples
; 3 + 2 = 5, correctly representable in two's complement with 4 bits
;>  (sum '(0 0 1 1) '(0 0 1 0))
;'(#t (0 1 0 1))

; -3 + 2 = -1, correctly representable in two's complement with 4 bits
;> (sum '(1 1 0 1) '(0 0 1 0))
;'(#t (1 1 1 1))

; -5 + 5 = 0, correctly representable in two's complement with 16 bits
;> (sum '(1 1 1 1  1 1 1 1  1 1 1 1  1 0 1 1) '(0 0 0 0  0 0 0 0  0 0 0 0  0 1 0 1))
;'(#t (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

; 4 + 4 = 8, which is not correctly representable in two's complement with 4 bits
;> (sum '(0 1 0 0) '(0 1 0 0))
;'(#f (0 0 0 0))

; 3 - 2 = 1, correctly representable in two's complement with 4 bits
;>  (diff '(0 0 1 1) '(0 0 1 0))
;'(#t (0 0 0 1))

; 3 - 5 = -2, correctly representable in two's complement with 4 bits
;>  (diff '(0 0 1 1) '(0 1 0 1))
;'(#t (1 1 1 0))

; 3 - (-6) = 9, which is not correctly representable in two's complement with 4 bits
;> (diff '(0 0 1 1) '(1 0 1 0))
;'(#f (0 0 0 0))
;************************************************************

(define (listofzeroes tc1)
  (cond
    [(empty? tc1) '()]
    [else (cons 0 (listofzeroes (rest tc1)))]))

(define (sum tc1 tc2)
  (cond
    [(equal? (n->tc (+ (tc->n tc1) (tc->n tc2)) (length tc1)) 'error) (cons #f (list (listofzeroes tc1)))]
    [else (cons #t (list (n->tc (+ (tc->n tc1) (tc->n tc2)) (length tc1))))]
  ))

(define (diff tc1 tc2)
  (cond
    [(equal? (n->tc (- (tc->n tc1) (tc->n tc2)) (length tc1)) 'error) (cons #f (list (listofzeroes tc1)))]
    [else (cons #t (list (n->tc (- (tc->n tc1) (tc->n tc2)) (length tc1))))]
  ))

;************************************************************
; ** problem 8 ** (10 points)
; Write two procedures

; (do-input config)
; (do-output config)

; Each takes a TC-201 configuration and performs the appropriate action 
; (reading a number from the user or writing a number out to the user)
; AND ALSO **RETURNS** THE RESULTING TC-201 CONFIGURATION.
;
; For input, the new configuration has the value read in the 
; accumulator, and all other registers unchanged.
; To read in a value, you may use the following let construct:

; (let ((value (begin (display "input = ") (read)))) ...)

; If the number provided by the user is not representable
; in two's complement with 16 bits, the returned value should
; be the symbol 'error instead of a new configuration.

; For output, the new configuration is returned UNCHANGED. 
; If the integer value from the accumulator is in 
; value-from-accumulator, then the output to the user can be 
; produced by:

; (display "output = ")
; (display value-from-accumulator)
; (newline)

; Examples

; The lines input = .. and output = .. show the interaction between 
; TC-201 and user.  The TC-201 configuration shows the value
; returned by the procedure.  This assumes init-config is working.

;> (do-input (init-config '()))
;input = 14
;(conf (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0)) '())
;> (do-output (do-input (init-config '())))
;input = -34
;output = -34
;(conf (cpu '(1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 0) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0)) '())

;************************************************************
 
(define (do-input config)
 (let ((value (begin (display "input = ") (read))))
   (cond
     [(equal? (n->tc value 16) 'error) 'error]
     [else
   (conf (cpu (n->tc value 16)
             (cpu-pc (conf-cpu config))
             (cpu-rf (conf-cpu config))
             (cpu-aeb (conf-cpu config)))
        (conf-ram config))])))

(define (do-output config)
 (display "output = ")
 (display (tc->n (cpu-acc (conf-cpu config))))
  (newline)
  config
  )

;************************************************************
; ** problem 9 ** (10 points)
; Write one procedure

; (next-config config)

; that takes a TC-201 configuration and returns the next TC-201 configuration,
; after one iteration of the fetch/execute cycle.

; If the run flag (rf) is 0, then the configuration config is returned unchanged,
; because the machine is halted.

; The instructions that should be implemented are:

; halt, load, store, add, sub, input, output, jump
; skipzero, skippos, skiperr, loadi, storei.

; These are opcodes 0000 through 1100, respectively.
; You should intepret an undefined opcode  (1101 through 1111) 
; as a halt instruction.

; For a halt instruction, in the returned configuration 
; the run flag is 0 and all other registers are unchanged.

; Otherwise, the program counter (pc) contains a memory address, and the TC-201 
; instruction at that location is fetched and executed, and the resulting 
; configuration is returned.  Note that all instructions result in a configuration
; being returned, INCLUDING input and output.
;************************************************************
(define (getopcode list n)
  (cond
    [(empty? list) '()]
    [(= n 0) '()]
    [else (cons (first list) (getopcode (rest list) (- n 1)))]))

(define (grh list)
  (getrest list 4))

(define (getrest list n)
  (cond
    [(empty? list) '()]
    [(> n 0) (getrest (rest list) (- n 1))]
    [else (cons (first list) (getrest (rest list) -1))]))


(define config6
 (conf (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0))
 (list
  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
  (entry 1 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (entry 3 '(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
  (entry 5 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
  (entry 6 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
  (entry 7 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
  (entry 8 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
  (entry 9 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
  )))

;(ram-read address ram)
;(ram-read (tc->n (cpu-pc (conf-cpu config))) (conf-ram config))

(define (next-config config)
  (let ((opcode (getopcode (ram-read (tc->n (cpu-pc (conf-cpu config))) (conf-ram config)) 4))
        (restlist (grh (ram-read (tc->n (cpu-pc (conf-cpu config))) (conf-ram config)))))
  (cond
    [(equal? (first (cpu-rf (conf-cpu config))) 0) config]
    [(equal? opcode '(0 0 0 0)) (conf (cpu (cpu-acc (conf-cpu config))
                                           (cpu-pc (conf-cpu config))
                                           '(0)
                                      (cpu-aeb (conf-cpu config)))
                                (conf-ram config))] ;set run flag to 0,
    [(equal? opcode '(0 0 0 1)) (add-to-pc 1 (mem->acc (tc->n restlist) config))]
    [(equal? opcode '(0 0 1 0)) (add-to-pc 1 (acc->mem (tc->n restlist) config))]
    [(equal? opcode '(0 0 1 1)) (cond
                                  [(first (sum (ram-read (tc->n restlist) (conf-ram config)) (cpu-acc (conf-cpu config))))
                                   (add-to-pc 1 (conf (cpu (first (rest (sum (ram-read (tc->n restlist) (conf-ram config)) (cpu-acc (conf-cpu config))))) 
                                              (cpu-pc (conf-cpu config))
                                              (cpu-rf (conf-cpu config))
                                              (cpu-aeb (conf-cpu config)))
                                         (conf-ram config)))]
                                  [else (add-to-pc 1 (conf (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                    (cpu-pc (conf-cpu config))
                                                    (cpu-rf (conf-cpu config))
                                                    '(1))
                                         (conf-ram config)))])] ;set aeb to 1
    [(equal? opcode '(0 1 0 0)) (cond
                                  [(first (diff (cpu-acc (conf-cpu config)) (ram-read (tc->n restlist) (conf-ram config))))
                                   (add-to-pc 1 (conf (cpu (first (rest (diff (cpu-acc (conf-cpu config)) (ram-read (tc->n restlist) (conf-ram config))))) 
                                              (cpu-pc (conf-cpu config))
                                              (cpu-rf (conf-cpu config))
                                              (cpu-aeb (conf-cpu config)))
                                         (conf-ram config)))]
                                  [else (add-to-pc 1 (conf (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                    (cpu-pc (conf-cpu config))
                                                    (cpu-rf (conf-cpu config))
                                                    '(1))
                                         (conf-ram config)))])]
    [(equal? opcode '(0 1 0 1)) (add-to-pc 1 (do-input config))]
    [(equal? opcode '(0 1 1 0)) (add-to-pc 1 (do-output config))] ;****
    [(equal? opcode '(0 1 1 1)) (addr->pc (tc->n restlist) config)]
    [(equal? opcode '(1 0 0 0)) (if (equal? (cpu-acc (conf-cpu config)) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) (add-to-pc 2 config) (add-to-pc 1 config))] 
    [(equal? opcode '(1 0 0 1)) (if (> (tc->n (cpu-acc (conf-cpu config))) 0) (add-to-pc 2 config) (add-to-pc 1 config))]
    [(equal? opcode '(1 0 1 0)) (if
                                 (equal? (first (cpu-aeb (conf-cpu config))) 1)
                                 (add-to-pc 2 (conf (cpu (cpu-acc (conf-cpu config))
                                                    (cpu-pc (conf-cpu config))
                                                    (cpu-rf (conf-cpu config))
                                                    '(0))
                                               (conf-ram config)))
                                 (add-to-pc 1 config))]
    [(equal? opcode '(1 0 1 1)) (add-to-pc 1 (mem->acc (tc->n (grh (ram-read (tc->n restlist) (conf-ram config)))) config))]
    [(equal? opcode '(1 1 0 0)) (add-to-pc 1 (acc->mem (tc->n (grh (ram-read (tc->n restlist) (conf-ram config)))) config))]
    [else (conf (cpu (cpu-acc (conf-cpu config))
                                           (cpu-pc (conf-cpu config))
                                           '(0)
                                      (cpu-aeb (conf-cpu config)))
                                (conf-ram config))]
  )))


;************************************************************
; ** problem 10 ** (10 points)
; Write three procedures

; (init-config lst)
; takes a list lst 16 bit patterns, and returns a TC-201 configuration 
; in which those patterns are loaded into RAM starting with address 0, 
; and the CPU registers are initialized so that the accumulator has
; 16 zeroes, the program counter has 12 zeroes, the run flag has 
; value 1, and the arithmetic error bit has value 0.

; (simulate steps config)
; takes a number of steps and a configuration config of the TC-201
; and simulates (using next-config) the machine until the machine
; halts (that is, the run flag is 0) or the given number of steps
; have been executed, whichever occurs first.  The list
; of successive configurations reached, starting from config, is returned.

; (run steps prog)
; takes a number of steps and a symbolic assembly-language program
; prog, and assembles the program (using assemble), and loads it
; into memory (using init-config) and runs it until either it
; halts or has run for the given number of steps (using simulate).

; Examples


(define patterns
  '((0 0 0 1  0 0 0 0  0 0 0 0  0 1 0 0)
    (0 0 1 1  0 0 0 0  0 0 0 0  0 1 0 1)
    (0 0 1 0  0 0 0 0  0 0 0 0  0 1 1 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  1 0 1 0)
    (1 1 1 1  1 1 1 1  1 1 1 1  1 1 0 1)
    (0 0 0 0  0 0 0 0  0 0 0 0  1 0 0 0)))

;> (init-config patterns)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;  (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;  (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;  (entry 5 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1))
;  (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0))))

;> (simulate 5 (init-config patterns))
;(list
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0))))
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) '(0 0 0 0 0 0 0 0 0 0 0 1) '(1) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0))))
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1) '(0 0 0 0 0 0 0 0 0 0 1 0) '(1) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0))))
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1) '(0 0 0 0 0 0 0 0 0 0 1 1) '(1) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1))))
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1) '(0 0 0 0 0 0 0 0 0 0 1 1) '(0) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1)))))

;> (define configs (run 200 prog-sum))
;input = 3
;input = -14
;input = 55
;input = 0
;output = 44
;************************************************************

(define (init-config lst)
  (conf (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0))
         (ramcreate lst 0)))

(define (ramcreate lst n)
  (cond
    [(empty? lst) '()]
    [else (cons (entry n (first lst)) (ramcreate (rest lst) (+ n 1)))]))

(define (simulate steps config)
  (append (list config) (simulateh steps config)))

(define (simulateh steps config)
  (let ((nextconfig (next-config config)))
  (cond
    [(= steps 0) '()]
    [(equal? (first (cpu-rf (conf-cpu config))) 0) '()]
    [else (cons nextconfig (simulateh (- steps 1) nextconfig))]))) 

(define (run steps prog)
  (simulate steps (init-config (assemble prog))))

;check to make sure when it fails on first conf that it only displays one configuration

;************************************************************
; ** problem 11 ** (10 points)
; Write two programs for the TC-201, in the
; format required by assemble.

; prog-sort-two
; reads in two numbers from the user and
; prints them out again, with the smaller of
; the two printed first, and halts.

; prog-reverse
; reads in a zero-terminated sequence of numbers from
; the user, and then prints the numbers out in the reverse
; order from which they were input (not including the final 0),
; and halts.

; Note that you can do this problem even if your simulator
; is not yet working.  Your programs will be tested with
; the reference simulator.

; Examples (showing the user interaction, not the configurations returned.)

;> (define configs (run 200 prog-sort-two))
;input = 13
;input = 6
;output = 6
;output = 13

;> (define configs (run 200 prog-sort-two))
;input = -11
;input = -1
;output = -11
;output= -1

;> (define configs (run 200 prog-sort-two))
;input = 32767
;input = -32768
;output = -32768
;output = 32767

;> (define configs (run 200 prog-reverse))
;input = 7
;input = 2
;input = 15
;input = -88
;input = 0
;output = -88
;output = 15
;output = 2
;output = 7
;> 
;************************************************************

(define prog-sort-two
  '((f1: input)
    (store f1)
    (f2: input)
    (store f2)
    (sub f1)
    (skippos)
    (jump positive)
    (jump negative)
    (positive: load f2)
    (output)
    (load f1)
    (output)
    (halt)
    (negative: load f1)
    (output)
    (load f2)
    (output)
    (halt)
     ))

(define prog-reverse
  '(
    (begin: input)
    (storei storage) ;puts input in desired storage spot (entry 450 first)  
    (skipzero)
    (jump startnext)
    (jump test)
    
    (startnext: load storage)
    (add value1)
    (store storage)
    (load count)
    (add value1)
    (store count)
    (jump begin)
    ;increases value of storage by 1 and first puts input in the spot 450
    ;acc is currently 451

    (test: load count)
    (skipzero)
    (jump end)
    (halt)
    
    (end: load count)
    (sub value1)
    (store count)
    (add storagepristine)
    (store storage)
    (loadi storage)
    (output)
    (load count)
    (skipzero)
    (jump end)
    (halt)

    (storage: data 450)
    (storagepristine: data 450)
    (value1: data 1)
    (count: data 0) 
    ))


(define multiply
  '((while: input)
    (store count)
    (store number)
    (load c0)
    (store sum)
    (load count)
    (skipzero)
    (jump do)
    (halt)
    (do: load count)
    (skipzero)
    (jump finishadd)
    (load sum)
    (output)
    (jump while)
    (finishadd: load number)
    (add sum)
    (store sum)
    (load count)
    (sub c1)
    (store count)
    (jump do)
    (count: data 0)
    (number: data 0)
    (sum: data 0)
    (c1: data 1)
    (c0: data 0)))

(define exponent
  '(
    (while: load c0)
    (store sum)
    (store count)
    (store number)
    (input)
    (store count)
    (store number)
    (skipzero)
    (jump do)
    (load c0)
    (output)
    (halt)
    (do: load count)
    (skipzero)
    (jump finishadd)
    (load sum)
    (output)
    (jump while)
    (finishadd: load number)
    (add sum)
    (store sum)
    (load count)
    (sub c1)
    (store count)
    (jump do)
    (sum: data 0)
    (count: data 0)
    (number: data 0)
    (c1: data 1)
    (c0: data 0)))
         
;



  ;test all cases in number 9 and check 3 part 1

;********************** end of hw6.scm **********************