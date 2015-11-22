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

(define (set-map-join f s)
    (if (empty? s)
        '()
        (cons (f (car s)) (set-map-join f (cdr s)))))

(define (bound-variables lambdaApplication)
        (cond
            [(empty? lambdaApplication)
                '()]
            [(equal? 'lambda (car lambdaApplication))
                (cons (cadr lambdaApplication) (bound-variables (cdr lambdaApplication)))]
            [else (bound-variables (cdr lambdaApplication))]))

(define (free-variables lambdaApplication)
        (define boundVariables (bound-variables lambdaApplication))
        (cond
            [(empty? lambdaApplication)
                '()]
            [(equal? 'lambda (car lambdaApplication))
                (free-variables (cdr lambdaApplication))]
            [(not (member? (car lambdaApplication) boundVariables))
                (cons (car lambdaApplication) (free-variables (cdr lambdaApplication)))]
            [else '()]))

(define test-it
    (lambda ()
        (define set1 '(a c b))
        (define set2 '(b c a))
        (define set3 '(c d f e))
        (cond
            [(not (= (set-cardinally set1) 3)) "set-cardinally failed test case 1"]
            [(= (set-cardinally set1) 4) "set-cardinally failed test case 2"]
            [(not (set-equal? set1 set2)) "set-equal? failed test case 1"]
            [(set-equal? set1 set3) "set-equal? failed test case 2."]
            [(not (set-equal? (set-union set1 set3) '(a b c d f e))) "set-union failed test case 1."]
            [(set-equal? (set-union set1 set3) '(a b c)) "set-union failed test case 2."]
            [(not (set-equal? (set-intersection set1 set3) '(c))) "set-intersection failed test case 1."]
            [else #t])))
