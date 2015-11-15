(define (member? element set)
    (cond
        [(empty? set) #f]
        [(equal? element (car set)) #t]
        [else (member? element (cdr set))]))

(define (add-element element set)
    (if (member? element set)
        set
        (cons element set)))

(define (set-cardinality set)
    (define (countElements set numElements)
        (cond
            [(empty? set) numElements]
            [(member? (car set) (cdr set))
                (countElements (cdr set) numElements)]
            [else (countElements (cdr set) (+ numElements 1))]))
    (countElements set 0))
