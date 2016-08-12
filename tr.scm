#lang racket

(define (heads lsts)
    (cond
        [(empty? lsts) null]
        [(empty? (car lsts)) null]
        [else
            (cons (car (car lsts)) (heads (cdr lsts)))]))

(define (tails lsts)
    (cond
        [(empty? lsts) null]
        [(empty? (car lsts)) null]
        [else
            (cons (cdr (car lsts)) (tails (cdr lsts)))]))

(define (tr lsts)
    (cond
        [(empty? lsts) null]
        [(empty? (car lsts)) null]
        [else
            (cons (heads lsts) (tr (tails lsts)))]))
