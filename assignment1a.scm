(define (member? element set)
    (cond
        [(empty? set) #f]
        [(equal? element (car set)) #t]
        [else (member? element (cdr set))]))

(define (add-element element set)
    (if (member? element set)
        set
        (cons element set)))

(define (list-to-set list)
    ;; take only unique elements from the list and put them in the set
    (define (make-set list set)
        (cond
            [(empty? list) set]
            [else
                (make-set (cdr list) (add-element (car list) set))]))
    (make-set list empty))

(define (set-cardinality set)
    (length (list-to-set set)))
