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
    (define (count-elements set numElements)
        (cond
            [(empty? set) numElements]
            [else (count-elements (cdr set) (+ numElements 1))]))
    (count-elements (list-to-set set) 0))

(define (set-union set1 set2)
    (cond
        [(empty? set1) set2]
        [else
            (set-union (cdr set1) (add-element (car set1) set2))]))
