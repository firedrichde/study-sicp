(define (make-rat n d)
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g)))
)

(define (numer x)
    (car x)
)

(define (denom x)
    (cdr x)
)

(define (print-rat x)
    (display (numer x))
    (display "/")
    (display (denom x))
)

(define (gcd a b)
    (if (= 0 b)
        a
        (gcd b (remainder a b)))
    )