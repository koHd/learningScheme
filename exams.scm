#lang racket

; after-filter
; input: predicate p, list xs
; output: list of those elements of xs which immediately follow an element which passes the predicate p

(define (after-filter p xs)
  (cond
    [(empty? xs) null]
    [(p (car xs))
     (cons (car (cdr xs)) (after-filter p (cdr xs)))]
    [else (after-filter p (cdr xs))]))

; deep-fetch
; input: predicate p, s-expresstion sexp
; output: list of all atoms inside the given s-expression which pass the given predicate

(define (deep-fetch p sexp)
  (cond
    [(empty? sexp) null]
    [(number? sexp) cons sexp]
    [(list? (car sexp))
     (append (deep-fetch p (car sexp)) (deep-fetch p (cdr sexp)))]
    [(p (car sexp))
     (cons (car sexp) (deep-fetch p (cdr sexp)))]
    [else (deep-fetch p (cdr sexp))]))

; add-numbers
; input: s-expression sexp
; output: the sum of all the numbers contained therein

; lets do this simple: pull all numbers into list using deep-fetch, then sum that list

(define (sum ls)
  (cond
    [(empty? ls) 0]
    [(number? ls) ls]
    [else
     (+ (car ls) (sum (cdr ls)))]))
  
(define (add-numbers sexp)
  (sum (deep-fetch number? sexp)))
     