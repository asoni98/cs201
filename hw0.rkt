#lang racket

(provide hours
	 sum-of-squares
	 favorite)

; Please do not modify the lines above this one.

; ********************************************************
; CS 201 HW #0  DUE Wednesday 1/25/15, 11:59 pm
; ** electronically using the Zoo submit system **

; ********************************************************
; Name: Arun Soni
; Email address: arun.soni@yale.edu
; ********************************************************

; This file may be loaded into Racket.  
; Lines beginning with semicolons are comments.

; Homework #0 will be worth 20 points -- other
; homeworks will be worth 100 points.
; One purpose of homework #0 is to make sure 
; you can use the submit system on the Zoo.  
; You will receive no credit for this assignment until
; you successfully use the submit system to submit it.

; You will be submitting *two* files for homework #0.
; Please name them:
; hw0.rkt (for the Racket definitions and procedures)
; response.pdf (for the reading response)

; ********************************************************
; ** problem 0 ** (1 easy point) 
; Replace the number 0 in the definition below to
; indicate how many hours you spent doing this assignment.
; Fractions are fine, eg, 3.14159.
; You will receive no credit for this problem if
; you leave the number as 0.

(define hours 3)

; ********************************************************
; ** problem 1 ** (5 points)

; Write a procedure (sum-of-squares x y)
; that takes as arguments integers x and y
; and returns the value of the square of x
; plus the square of y.

; Examples

; (sum-of-squares 3 4) => 25
; (sum-of-squares -2 10) => 104
; (sum-of-squares 0 1) => 1

; ********************************************************

(define (sum-of-squares x y)
  (+ (* x x) (* y y)))

; ********************************************************
; ** problem 2 ** (4 points)

; Write a procedure (favorite)
; that takes no arguments and
; returns a *string* indicating
; something you like to eat.

; Please remember the difference between
; a procedure call and the evaluation
; of a variable!

; Example (yours will likely be different)

; (favorite) => "pizza with artichoke hearts"

; ********************************************************

(define (favorite) "Mexican Pizza")

; ********************************************************
; ** problem 3 ** (10 points)

; For this problem, you are asked to find one
; article (of 2 pages or more) in
; the magazine "Communications of the ACM",
; in one of the four issues:
; October, November, December 2016, January 2017,
; read the article and answer 
; the following three questions:

;   a. What did you know about the topic
;      prior to reading the article?
;   b. What did you learn from reading the
;      article?
;   c. What more would you like to know
;      about the topic?

; Your answer should be AT MOST 2 pages
; saved in pdf format, and submitted as
; the file response.pdf for assignment 0.
; Please include in your file 
; your name and email address
; and the title and author(s) of the article
; you are responding to.

; Your grade for this problem will be 10 if I can
; open, print and read your submitted pdf file.
; It is to help me get acquainted with you and
; your interests -- you won't receive feedback
; on your answers.

; ********************************************************
; ********  end of homework #0
; ********************************************************