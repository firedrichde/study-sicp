(define (accumulate operation initial sequence)
    (if (null? sequence)
        initial
        (operation (car sequence) (accumulate operation initial (cdr sequence))))
)