(define (member? element set)
    (cond
        [(empty? set) #f]
        [(equal? element (car set)) #t]
        [else (member? element (cdr set))]))

(define (add-element element set)
    (if (member? element set)
        set
        (cons element set)))

(define (make-set lst set)
        (cond
            [(empty? lst) set]
            [else (make-set (cdr lst) (add-element (car lst) set))]))

(define (lst-to-set lst)
    (make-set lst empty))

(define (set-cardinally set)
    (define (count-elements set numElements)
        (cond
            [(empty? set) numElements]
            [else (count-elements (cdr set) (+ numElements 1))]))
    (count-elements (lst-to-set set) 0))

(define (set-union s1 s2)
    (define setA (lst-to-set s1))
    (define setUnion (lst-to-set s2))
    (cond
        [(empty? setA) setUnion]
        [else (set-union (cdr setA) (add-element (car setA) setUnion))]))

(define (set-intersection s1 s2)
    (define setA (lst-to-set s1))
    (define setB (lst-to-set s2))
    (cond
        [(empty? setA) '()]
        [(member? (car setA) setB)
            (cons (car setA) (set-intersection (cdr setA) setB))]
        [else (set-intersection (cdr setA) setB)]))
 
(define (set-difference s1 s2)
    (cond
        [(empty? s1) '()]
        [(not (member? (car s1) s2))
            (cons (car s1) (set-difference (cdr s1) s2))]
        [else (set-difference (cdr s1) s2)]))

(define (set-equal? s1 s2)
    (if (= (set-cardinally (set-intersection s1 s2)) (set-cardinally (lst-to-set s1))) #t #f))
