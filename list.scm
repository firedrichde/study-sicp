(load "map.scm")
(load "super_abstract.scm")
(define nil (list))
(define (list-ref items n)
    (if (= 0 n)
        (car items)
        (list-ref (cdr items) (- n 1))
    )
)

(define (last-pair items)
    (define (last-pair-iter l a)
        (if (null? l)
            a
            (last-pair-iter (cdr l) (car l))))
    (last-pair-iter items (car items))
    )

(define (reverse items)
    (define (iter x y)
        (if (null? x)
            y 
            (iter (cdr x) (cons (car x) y)))
    )
    (iter items (list))
) 

(define (append list-x list-y)
    (if (null? list-x)
        list-y
        (cons (car list-x) (append (cdr list-x) list-y)))
)

(define (same-parity-wrap . w)
    (same-parity w))

(define (same-parity items)
    (define first-item (car items))
    (define compare?
            (lambda (x y) (= (remainder x 2) (remainder y 2)))
    )
    (define (iter x)
            (cond ((null? x) (list))
                ((compare? first-item (car x)) (cons (car x) (iter (cdr x))))
                (else (iter (cdr x)))
            )      
        
    )
    (iter items)
)

; 判断是否为一个简单的表(树我定义为复杂的表)
(define (simple-list? items)
(cond ((null? items) #t)
    ((pair? (car items)) #f)
    (else (simple-list? (cdr items)))
    )
)

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high)))
)


(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y) ) 0 sequence)
)


(define (append-acc seq1 seq2)
    (accumulate cons seq2 seq1)
)

(define sample-items (list 1 2 3 4 5))

(define (test-list-ref n)
    (= (list-ref sample-items n) (+ n 1)))