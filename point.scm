(define (make-point x y)
    (cons x y))

(define (x-point p)
    (car p))

(define (y-point p)
    (cdr p))

(define (print-point p)
    (display "(")
    (display (x-point p))
    (display ", ")
    (display (y-point p))
    (display ")")
)

(define (average f)
    (lambda (x y) (/ (+ (f x) (f y)) 2)))

(define (mid-point p1 p2)
    (let ((mid-point-x ((average x-point) p1 p2))
          (mid-point-y ((average y-point) p1 p2)))
        (make-point mid-point-x mid-point-y))
    )