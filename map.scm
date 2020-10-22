(load "math.scm")

; items: a list
(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items))
        )
    )
)

(define (abs-list items)
    (map abs items))

(define (abs-list-wrap . w)
    (abs-list w)
)

(define (for-each proc)
    (lambda (items)
            (cond ((null? items))
                (else (proc (car items))
                      ((for-each proc) (cdr items))
                ))
    )
)

(define (for-each-wrap proc items)
    ((for-each proc) items)
)