#lang racket

(provide 
 random-list time-many-calls
 tr-length ntr-length
 insert merge
 divide-list isort msort
 visort
 count-compares)

; Please do not change lines above this one.

;************************************************************
; CS 201a HW #7  DUE  Wednesday, April 26, 11:59 pm 
; via the submit system on the Zoo.  Without a "Temporary Incomplete"
; from you residential college dean, no homework can be accepted
; after the last day of Reading Period, that is, after May 4.
;************************************************************
; Name: Arun Soni
; Email address: arun.soni@yale.edu
;************************************************************

; Computer science topics: running times of programs, insertion sort,
; merge sort.  Racket topics: vectors.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;************************************************************

; Timing procedures.
; The Racket procedure (current-inexact-milliseconds)
; returns a real (in the sense of Racket) number representing
; the current time in milliseconds since midnight UTC, January 1, 1970.

;************************************************************
; ** problem 1 ** (15 points)

; Write two procedures

; (random-list n)
; (time-many-calls reps proc args)

; (random-list n) takes a non-negative number n
; and returns a list of length n of randomly chosen 
; real numbers between 0 and 1.

; Note that (random) returns a random number between 0 and 1.

; (time-many-calls reps proc args) takes 
; a nonnegative integer, reps,
; a procedure, proc,
; a list of arguments for the procedure, args,
; and returns the amount of time in SECONDS elapsed
; in calling proc on args a number of times equal to reps. 

; Recall that we can apply a proc to a list of argument args
; with (apply proc args).
; Note that a millisecond is 1/1000 of a second.

; Example of random-list
;> (random-list 10)
;'(0.8339052213011976
;  0.28715053124523504
;  0.3824988826084341
;  0.6922657643424531
;  0.4919157755371373
;  0.6921387871179888
;  0.24176785286695546
;  0.015492810221040746
;  0.22382546601716827
;  0.7667863239747369)

; The following examples of time-many-calls were run on a Zoo
; workstation, calling the built-in compare procedure (<=)
; a certain number of times on the arguments 13 and 14.
; The times for 10^4, 10^5, 10^6 and 10^7 repetitions
; were about 0.0003 seconds, 0.0057 seconds, 0.0609 seconds, and
; 0.5600 seconds, respectively.

; Except in the first case, multiplying the number of repetitions
; by 10 approximately multiplies the measured time by 10,
; so in the range of repetitions between 10^5 and 10^7,
; constant time (time Theta(1)) seems like a reasonable model
; of the time for the operation (<= 13 14).

;> (time-many-calls 1000 <= '(13 14))
;4.1015625e-05
;> (time-many-calls 10000 <= '(13 14))
;0.000259033203125
;> (time-many-calls 1e5 <= '(13 14))
;0.00566796875
;> (time-many-calls 1e6 <= '(13 14))
;0.06086083984375
;> (time-many-calls 1e7 <= '(13 14))
;0.55958984375

; This second set of timings illustrates random variation in
; these measurements on the same Zoo node.  Timings on your
; laptop may be noticeably different.

;> (time-many-calls 1000 <= '(13 14))
;5.419921875e-05
;> (time-many-calls 10000 <= '(13 14))
;0.0002880859375
;> (time-many-calls 1e5 <= '(13 14))
;0.005796875
;> (time-many-calls 1e6 <= '(13 14))
;0.055732177734375
;> (time-many-calls 1e7 <= '(13 14))
;0.5537509765625

; The following examples show timings (on a Zoo node)
; for creating lists of 100,000 or 200,000 or 300,000
; random numbers.
; Less than a tenth of a second suffices in the last case.

;> (time-many-calls 1 random-list '(100000))
;0.014049072265625
;> (time-many-calls 1 random-list '(200000))
;0.02800390625
;> (time-many-calls 1 random-list '(300000))
;0.06631005859375

;******************************(******************************

(define (random-list n)
  (cond
    [(= n 0) '()]
    [else (cons (random) (random-list (- n 1)))]))

(define (timetest proc lst)
  (let ((x (current-inexact-milliseconds))
         (y (apply proc lst))
         (z (current-inexact-milliseconds)))
   (- z x)))
 
(define (addup reps proc lst)
  (cond
    [(= reps 0) 0]
    [else (+ (timetest proc lst) (addup (- reps 1) proc lst))])) 

(define (time-many-calls reps proc args)
  (/ (addup reps proc args) 1000)
  )


;************************************************************
; ** problem 2 ** (15 points)
; For this problem, you will use your procedures
; random-list and time-many-calls
; to time the built-in Racket procedures:

; length, vector-ref, list-ref

; reporting the requested measurements, and answering the related questions.

; *Comment out* your responses with semicolons.
; Also, please *comment out* with semicolons the creation of
; long vectors and lists, or your procedures may fail the timeouts
; and lose credit!

; For length, create lists of lengths k*100,000 for k = 1,2,3,4 
; and report measurements of 1000 repetitions of calling length
; on these lists.

; QUESTION: what can you say about how the time for (length lst)
; grows as a function of the length of lst?

; For vector-ref and list-ref, create vectors and lists of
; lengths 100,000 and 200,000 elements, and for each,
; measure the time for 1000 repetitions of accessing the elements
; at the beginning, 25%, 50% and 75% of the length, and at the end.

; QUESTION: what can you say about how the times for 
; (vector-ref v index) and (list-ref lst index) 
; depend on the length of the structure and the index accessed?
;************************************************************

;Average time is listed below for each test: 
;> (time-many-calls 1000 length (list (random-list 100000)))
;0.17122998046875
;> (time-many-calls 1000 length (list (random-list 200000)))
;0.34755517578125
;> (time-many-calls 1000 length (list (random-list 300000)))
;0.525404541015625
;> (time-many-calls 1000 length (list (random-list 400000)))
;0.693934814453125


; QUESTION 1: what can you say about how the time for (length lst) grows as a function of the length of lst?
;The time (length lst) grows linearly as a function of the length of the lst.
;A list that is twice as long takes twice as long to evaluate (three times as long takes three times as long to evaluate)

;Question 2:
;The time to access elements in (vector-ref v index) with a list that is twice as long takes the same time (accessing time does not depend on length of list).
;In addition, vector-ref takes a similar time regardless of where the element is in the list (1/4 in, 1/2 in, 3/4 in, or the last element). It is relatively constant at 6.2744140625e-05 seconds. 
;However, list-ref takes twice as long to get 1/2 into the list as 1/4 into the list. This indicates a linear relationship between distance into list and time. 
;list-ref also takes twice as long to get to the end (or 1/4 in, 1/2 in, or 3/4 in) when the list is twice as long. This indicates a linear relationship between time and length of list.

;Average time is listed below for each test: 
;> (time-many-calls 1000 vector-ref (list (list->vector (random-list 100000)) 25000))
;6.201171875e-05
;> (time-many-calls 1000 vector-ref (list (list->vector (random-list 100000)) 50000))
;6.4697265625e-05
;> (time-many-calls 1000 vector-ref (list (list->vector (random-list 100000)) 75000))
;6.494140625e-05
;> (time-many-calls 1000 vector-ref (list (list->vector (random-list 100000)) 99999))
;6.2744140625e-05

;> (time-many-calls 1000 vector-ref (list (list->vector (random-list 200000)) 25000))
;6.201171875e-05
;> (time-many-calls 1000 vector-ref (list (list->vector (random-list 200000)) 50000))
;6.9580078125e-05
;> (time-many-calls 1000 vector-ref (list (list->vector (random-list 200000)) 75000))
;6.1767578125e-05
;> (time-many-calls 1000 vector-ref (list (list->vector (random-list 200000)) 99999))
;6.1279296875e-05

;> (time-many-calls 1000 list-ref (list (random-list 100000) 25000))
;0.089964111328125
;> (time-many-calls 1000 list-ref (list (random-list 100000) 50000))
;0.174316650390625
;> (time-many-calls 1000 list-ref (list (random-list 100000) 75000))
;0.26798974609375
;> (time-many-calls 1000 list-ref (list (random-list 100000) 99999))
;0.357215087890625

;> (time-many-calls 1000 list-ref (list (random-list 200000) 50000))
;0.1701005859375
;> (time-many-calls 1000 list-ref (list (random-list 200000) 100000))
;0.3394931640625
;> (time-many-calls 1000 list-ref (list (random-list 200000) 150000))
;0.50782421875
;> (time-many-calls 1000 list-ref (list (random-list 200000) 199999))
;0.680797607421875

;************************************************************
; ** problem 3 ** (10 points)
; Write two procedures

; (tr-length lst)
; (ntr-length lst)

; that each take a list lst and return its length.
; tr-length should be implemented using tail recursion
; ntr-length should be implemented recursively but without
;            using tail recursion.

; QUESTION: Compare the time taken by the built-in length procedure
; with the time taken by your tr-length and ntr-length procedures.
; Comment out with semicolons your data and conclusions.
;************************************************************
  (define (ntr-length lst)
      (if (null? lst)
          0
          (+ 1 (ntr-length (rest lst)))))

 (define (our-length-aux lst len)
      (if (null? lst)
          len
          (our-length-aux (rest lst) (+ 1 len))))

(define (tr-length lst)
      (our-length-aux lst 0))

;Average of five trials of a random list of length 10,000
;> (time-many-calls 1e3  ntr-length (list (random-list 10000)))
;0.21436767578125
;> (time-many-calls 1e3  tr-length (list (random-list 10000)))
;0.140193115234375
;> (time-many-calls 1e3  length (list (random-list 10000)))
;0.0286220703125

;The racket function length takes the least time (performs the best).
;It performs it's operation in nearly 1/4 the time of the tail recursive function. 
;Tr-length is second best as it takes less time than ntr. This is because tr is a tail-recursive procedure. 

;************************************************************

; Now we turn to sorting a list of elements with respect 
; to a given comparison operator.

;************************************************************
; ** problem 4 ** (10 points)
; Write two procedures

; (insert compare? item lst)
; (merge compare? lst1 lst2)

; For these procedures, compare? is a comparison procedure
; for a total ordering of the values item and the elements of lst,
; or the elements of lst1 and lst2.  For example, for lists of
; numbers, compare? might be <= or >=.  The procedure compare?
; takes two arguments and returns #t or #f.

; (insert compare? item lst)
; inserts an item into a list lst of items
; which is sorted with respect to the compare?
; predicate, so that the resulting list of
; items is also sorted with respect to the
; compare? predicate.

; (merge compare? lst1 lst2)
; takes two lists of elements lst1 and lst2, each of which is sorted with
; respect to the compare? predicate, and produces as its result a list
; of all the items in lst1 and lst2 (preserving duplicates) that is
; sorted with respect to compare?

; Examples
;> (insert <= 3 '(1 2 4 5))
;'(1 2 3 4 5)
;> (insert string>=? "hello" (list "the" "best" "arrangment"))
;'("the" "hello" "best" "arrangment")
;> (merge >= '(10 7 4 2 1) '(22 9 5))
;'(22 10 9 7 5 4 2 1)
;> (merge string<=? (list "a" "novel" "thought") (list "more" "predictive"))
;'("a" "more" "novel" "predictive" "thought")
;************************************************************
;insert where failed
(define (insert compare? item lst)
  (cond
    [(empty? lst) (list item)]
    [(compare? item (first lst)) (cons item lst)]
    [else (cons
           (first lst)
           (insert compare? item (rest lst)))]))

(define (merge compare? lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [(compare? (first lst2) (first lst1)) (cons (first lst2) (merge compare? lst1 (rest lst2)))]
    [else (cons (first lst1) (merge compare? (rest lst1) lst2))]))

;(define (merge compare? lst1 lst2)
;  (cond
;    [(empty? lst1) lst2]
;    [else (merge compare? (rest lst1) (insert compare? (first lst1) lst2))]))
  
;************************************************************
; ** problem 5 ** (15 points)
; Write three procedures

; (divide-list lst)
; (isort compare? lst)
; (msort compare? lst)

; (divide-list lst) returns a list of two lists,
; the first of which consists of the first half of the elements of lst and
; the second of which consists of the second half of the elements of lst.
; If lst has an odd number of elements, then the first of the returned
; lists should have one more element than the second.
; This procedure should run in time that is (in principle) proportional
; to the length of lst. (Please recall what you learned about
; the running time of length in previous problems.)

; The procedures isort and msort take a total order comparison predicate 
; compare? and a list lst of items, and returns a list of all 
; the elements in lst (duplicates preserved) arranged so that 
; they are sorted with respect to compare?

; (isort compare? lst) should use (insert compare? item lst) and
; should implement insertion sort.

; (msort compare? lst) should use (divide-list lst) and (merge lst1 lst2) 
; and should implement merge sort.

; Examples
; (divide-list '(a b c d e)) => '((a b c) (d e))
; (divide-list '(12 3 6 4 9 3 2)) => '((12 3 6 4) (9 3 2))
;> (isort string<=? (list "predictive" "novel" "a" "more" "thought"))
;'("a" "more" "novel" "predictive" "thought")
;> (msort string>=? (list "predictive" "novel" "a" "more" "thought"))
;'("thought" "predictive" "novel" "more" "a")
;************************************************************

(define (divide-list lst)
 (divide-listh lst (length lst)))

(define (cutlist lst i j)
  (cond
    [(> i j) '()]
    [else (cons (list-ref lst i) (cutlist lst (+ i 1) j))]))

(define (divide-listh lst n)
  (cond
    [(empty? lst) '()]
    [(odd? n)  (list (cutlist lst 0 (quotient n 2)) (cutlist lst (+ (quotient n 2) 1) (- n 1)))]
    [(even? n) (list (cutlist lst 0 (- (quotient n 2) 1)) (cutlist lst (quotient n 2) (- n 1)))]))

(define (isort compare? lst)
  (cond
    [(empty? lst) '()]
    [else (insert compare? (first lst) (isort compare? (rest lst)))]))

(define (msort compare? lst)
  (cond
    [(<= (length lst) 1) lst]
    [else  (merge compare? (msort compare? (first (divide-list lst)))
                         (msort compare? (second (divide-list lst))))]))

;************************************************************
; ** problem 6 ** (15 points)

; QUESTION: By using sufficiently long lists of numbers (integer or real)
; and the comparison operation <=,
; possibly repeating and averaging measurements, 
; give empirical evidence for the claims that:
; (1) your implementation of insertion sort (isort, above) has best
; case time Theta(n) and worst case time of Theta(n^2).
; (2) your implementation of merge sort (msort, above) has best case and
; worst case times of Theta(n log n).

; QUESTION: Describe the inputs that give best and worst cases for your
; implementations of isort and msort.

; QUESTION: Roughly what is the longest list of random numbers that your isort
; procedure can sort in 10 seconds?  Same question for your msort procedure?

; Because of memory caching and other effects, the timing behaviors will not
; necessarily uniform over the whole range of feasible input lengths.
;************************************************************

(define (adds lst)
  (cond
    [(empty? lst) 0]
    [else (+ (first lst) (adds (rest lst)))]))
;sums all numbers in a list

(define (average lst)
  (/ (adds lst) (length lst)))

(define (wcinsertion num) ;worst case insertion
  (cond
    [(= num 0) '()]
    [else (cons num (wcinsertion (- num 1)))]))

(define (bcinsertion num) (reverse (wcinsertion num))) ;best case insertion

;insertion sort best case times for length 1000, 2000, 3000, 4000
;(time-many-calls 100 (lambda (x) (isort <= x)) (list (bcinsertion 1000)))
;Trials: 0.008111816, 0.007916016, 0.008912109, 0.007924316, 0.007947266, 0.007843994
;Average for length 1000: 0.008109253

;(time-many-calls 100 (lambda (x) (isort <= x)) (list (bcinsertion 2000)))
;Trials: 0.01580029296875, 0.017632568359375, 0.015754638671875, 0.015954833984375, 0.0157412109375, 0.016198974609375
;Average for length 2000: 0.016180419921875

;(time-many-calls 100 (lambda (x) (isort <= x)) (list (bcinsertion 3000)))
;Trials: 0.0240576171875, 0.02338623046875, 0.0238564453125, 0.025281005859375, 0.02310791015625
;Average for length 3000; 0.023937841796875005

;(time-many-calls 100 (lambda (x) (isort <= x)) (list (bcinsertion 4000)))
;Trials: 0.03168505859375, 0.032078125, 0.03178955078125, 0.03298876953125, 0.03139013671875
;Average for length 4000; 0.031986328125000005



;insertion sort worst case times for length 100, 200, 400, 800
;(time-many-calls 100 (lambda (x) (isort <= x)) (list (wcinsertion 100)))
;Trials: 0.035604736328125 0.035026123046875 0.035158935546875 0.036167724609375 0.035229248046875
;Average for length 100; 0.035437353515625

;(time-many-calls 100 (lambda (x) (isort <= x)) (list (wcinsertion 200)))
;Trials: 0.142631103515625 0.143859130859375 0.142242431640625 0.142494384765625 0.14210302734375 0.143779052734375
;Average for length 200; 0.14285152180989583

;(time-many-calls 100 (lambda (x) (isort <= x)) (list (wcinsertion 400)))
;Trials: 0.58025634765625, 0.57835009765625, 0.5799091796875, 0.57712255859375, 0.57781884765625
;Average for length 400; 0.5786914062499999

;(time-many-calls 100 (lambda (x) (isort <= x)) (list (wcinsertion 800)))
;Trials: 2.377280517578125 2.377677490234375 2.422571533203125 2.36783935546875 2.367302978515625 2.370360107421875
;Average for length 800; 2.3805053304036456

;Best case scenario for insertion sort: list is already sorted in order. This makes the insertion sort do the least number of comparisons. 
;Worst case scenario for insertion sort: sorted list is reversed (smallest element at end and largest in beginning with rest in descending order).
;The worst case scenario makes the insertion sort do the maximum number of comparisons. 

;After conducting at least five trials on the best and worst case scenario for insertion sort, we can see as the length of the list doubles,
;the run time increases linearly for the best case but increases quadratically for the worst case. 
;For worst case: a double in length (from 400 to 800 increases run time by 4) showing a n^2 run time.
;For best case: a double in length (from 1000 to 2000 increases run time by 2) showing a n run time. 

(define (bcmsort num) (reverse (wcinsertion num)))

(define (wcmsort num)
  (append (reverse (evenlist num)) (reverse (oddlist num))))
;makes a list of all even numbers then all odd numbers (worst case for msort)

(define (evenlist num)
  (cond
    [(= num 0) '(0)]
    [(even? num) (cons num (evenlist (- num 1)))]
    [else (evenlist (- num 1))]))
;makes a list of all even numbers under num

(define (oddlist num)
  (cond
    [(= num 0) '()]
    [(odd? num) (cons num (oddlist (- num 1)))]
    [else (oddlist (- num 1))]))
;makes a list of all odd numbers under num

;Merge sort best case times for length 1000, 2000, 4000, 8000
;(time-many-calls 100 (lambda (x) (msort <= x)) (list (bcmsort 1000)))
;Trials: 0.117077880859375 0.12011669921875 0.119296875 0.117813720703125 0.117778076171875
;Average for length 100; 0.11841665039062499

;(time-many-calls 100 (lambda (x) (msort <= x)) (list (bcmsort 2000)))
;Trials: 0.259428466796875 0.257703125 0.257495849609375 0.251939208984375 0.25313134765625 0.254031982421875
;Average for length 2000; 0.25562166341145837

;(time-many-calls 100 (lambda (x) (msort <= x)) (list (bcmsort 4000)))
;Trials: 0.55536376953125 0.549987548828125 0.550049072265625 0.547322021484375 0.5518115234375
;Average for length 4000; 0.550906787109375

;(time-many-calls 100 (lambda (x) (msort <= x)) (list (bcmsort 8000)))
;Trials: 1.24049560546875 1.25027099609375 1.200949951171875 1.24161279296875 1.207437255859375 1.2523447265625
;Average for length 8000; 1.2321852213541666



;Merge sort worst case times for length 1000, 2000, 4000, 8000
;(time-many-calls 100 (lambda (x) (msort <= x)) (list (wcmsort 1000)))
;Trials: 0.1220302734375 0.1192490234375 0.119524169921875 0.11882958984375 0.1191318359375 0.119617431640625
;Average for length 100; 0.11973038736979165

;(time-many-calls 100 (lambda (x) (msort <= x)) (list (wcmsort 2000)))
;Trials: 0.261393798828125 0.265164794921875 0.261828125 0.262265380859375 0.261392578125
;Average for length 2000; 0.262408935546875

;(time-many-calls 100 (lambda (x) (msort <= x)) (list (wcmsort 4000)))
;Trials: 0.58390869140625 0.57976318359375 0.585406005859375 0.627047607421875 0.5791142578125
;Average for length 4000; 0.5910479492187501

;(time-many-calls 100 (lambda (x) (msort <= x)) (list (wcmsort 8000)))
;Trials: 1.24265234375 1.286531494140625 1.275927978515625 1.242276611328125 1.287576904296875
;Average for length 8000; 1.26699306640625
 
;Best case for merge sort: list is sorted (either forwards or in reverse gets same best case)
;Worst case example for merge sort; {0,2,4,6,1,3,5,7} which should be sorted as {0,1,2,3,4,5,6,7}.
;left subarray {0, 2, 4, 6} and right subarray {1, 3, 5, 7} will result in the maximum number of comparisons. 

;For both the best case and worst case scenario of merge sort, we can see that taking the ratio of run times
;and the ratio of nlog(n) we get a constant number.  
;For best case: a length of 1000 gets a runtime of 0.118 seconds. a length of 2000 gets a runtime of 0.2556 seconds. The ratio is 2.158.
;(1000)(log(1000)) is 3000 and (2000)(log(2000)) is 6602.06. The ratio is 2.20 which is approximately the same, showing a nlogn time for the best case.
;For the worst case: a length of 1000 gets a runtime of 0.1197 seconds. a length of 2000 gets a runtime of 0.2624 seconds. The ratio is 2.1916.
;The ratio for nlog(n) is 2.20 which is approximately the same, showing a nlog(n) time for the worst case.
;In addition, as evidenced in the trials, each worst case time is slightly longer than the best case time but the ratio of worst case times is still nlog(n).

;(time-many-calls 1 (lambda (x) (isort <= x)) (list (random-list 20000)))
;9.924047119140624

;(time-many-calls 1 (lambda (x) (msort <= x)) (list (random-list 1400000)))
;9.882843994140625

;The longest list isort can sort in under 10 seconds is 20,000 in length.
;While msort can sort a list that is 140,000 in length in under 10 seconds. 

;************************************************************
; ** problem 7 ** (10 points)
; Write one procedure

; (visort vec)

; that takes as input a vector vec of numbers and sorts them
; into increasing order using the algorithm described in
; pseudo-code in the Wikipedia article on insertion sort
; (either 2014 version, given in the lecture notes for 2014,
; or current 2017 version at Wikipedia).  The algorithm
; needs to be slightly corrected, of course.

; Use vector-ref, vector-set! to sort "in place" in vec.

; QUESTION: How does the time for visort compare with the times
; for isort and msort to sort the same inputs (as a vector or list).
;************************************************************
;(vector-set! vec pos v)
;(vector-ref vec pos)
(define (visort vec)
  (forfunc (vector-ref vec 1) 1 1 vec))

(define (forfunc pos x ysub vec)
  (cond
    [(= x (vector-length vec)) vec]
    [else
     (whilefunc (vector-ref vec x) x x vec)]))

(define (whilefunc pos x y vec)
  (cond
    [(not (and
           (> y 0)
           (> (vector-ref vec (- y 1)) pos)))
     (let ((vectornew (vector-set! vec y pos)))
       (forfunc pos (+ x 1) y vec))]
    [else
     (let ((vectornew (vector-set! vec y (vector-ref vec (- y 1)))))
       (whilefunc pos x (- y 1) vec))]))

;saved a random list:
;(define listrandom (random-list 8000))

; (time-many-calls 100 visort (list (list->vector listrandom)))
;0.25066455078125

; (time-many-calls 100 (lambda (x) (isort <= x)) (list listrandom))
;142.80803930664064

; (time-many-calls 100 (lambda (x) (msort <= x)) (list listrandom)))
;1.7484677734375

;On a list of length 8000, 100 times, visort takes 0.25 seconds,
;isort takes 142 seconds, and msort takes  1.75 seconds.
;Visort is significantly faster than msort which is significantly faster than isort on the same randomlist. 


;************************************************************
; ** problem 8 ** (10 points)
; Write one procedure

; (count-compares sort compare? lst)

; that returns the number of calls to the compare? procedure
; when we apply the procedure sort to the arguments compare? and lst.
; Think of sort as a procedure like msort or isort, taking a comparison
; predicate and a list as its arguments, though sort could
; be some other sorting procedure devised for testing purposes.

; The trick here is to take the compare? procedure and "wrap" it
; in another procedure that will count the number of times it
; is called.  Then call sort with the "wrapped" compare? and lst
; as inputs.  Finally, return the final count from the "wrapped"
; compare? as the value from count-compares.

; Please read about the mutator set! to help you keep the count.

; Examples (yours may randomly vary.)
;> (count-compares msort <= (random-list 10))
;23
;> (count-compares msort <= (random-list 10))
;22
;> (count-compares isort <= (random-list 10))
;34
;************************************************************

(define counter 
  (let ((count 0))
    (lambda (cmd)
      (case cmd
        [(zero) (set! count 0)]
        [(increment) (set! count (+ 1 count))]
        [(value) count]))))
;counter from lecture

(define (count-compares sort compare? lst)
  (counter 'zero)
  (sort (lambda (x y)
          (counter 'increment)
          (compare? x y)) lst)
  (counter 'value))

;************************************************************
;********* end of hw7, end of hws! **************************
