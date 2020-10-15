(load "point.scm")

(define (make-segment point-x point-y)
    (cons point-x point-y))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))

(define (print-segment segment)
    (newline)
    (display "(")
    (print-point (start-segment segment))
    (display "-")
    (print-point (end-segment segment))
    (display ")")     
)

(define (midpoint-segment segment)
    (mid-point (start-segment segment) (end-segment segment))
)