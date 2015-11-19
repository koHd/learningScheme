(define (member? element set)
    (cond
        [(empty? set) #f]
        [(equal? element (car set)) #t]
        [else (member? element (cdr set))]))

(define (add-element element set)
    (if (member? element set)
        set
        (cons element set)))

(define (make-set list set)
        (cond
            [(empty? list) set]
            [else (make-set (cdr list) (add-element (car list) set))]))

(define (list-to-set list)
    (make-set list empty))

(define (set-cardinality set)
    (define (count-elements set numElements)
        (cond
            [(empty? set) numElements]
            [else (count-elements (cdr set) (+ numElements 1))]))
    (count-elements (list-to-set set) 0))

(define (set-union set1 set2)
    (define setA (list-to-set set1))
    (define setUnion (list-to-set set2))
    (cond
        [(empty? setA) setUnion]
        [else (set-union (cdr setA) (add-element (car setA) setUnion))]))

(define (set-intersection set1 set2)
    (define setA (list-to-set set1))
        (define setB (list-to-set set2))
        (cond
            [(empty? setA) '()]
            [(member? (car setA) setB)
                (cons (car setA) (set-intersection (cdr setA) setB))]
            [else (set-intersection (cdr setA) setB)]))
 
(define (set-difference set1 set2)
    (cond
        [(empty? set1) '()]
        [(not (member? (car set1) set2))
            (cons (car set1) (set-difference (cdr set1) set2))]
        [else (set-difference (cdr set1) set2)]))

(define (set-equal? set1 set2)
    (cond
        [(empty? set1) #t]
        [(not (member? (car set1) set2)) #f]
        [else (set-equal? (cdr set1) set2)])) 
