; helper for echo-lots
(define (echo-lots-help lst n)
  (if (zero? n)
      '()
      (cons lst (echo-lots-help lst (- n 1)))
      )
  )

; helper function for echo-all
 (define (echo-all-help lst rem)
   (if (zero? rem)
       (echo-all (cdr lst))
       (cons (echo-all (car lst)) (echo-all-help lst (- rem 1)))
       )
   )

; #1
(define (echo lst)
  (if (null? lst)
      '()
      (cons (car lst) (cons (car lst) (echo (cdr lst))))
      )
  )

; #2
(define (echo-lots lst n)
  (if (null? lst)
      '()
      (append (echo-lots-help (car lst) n)
              (echo-lots (cdr lst) n))
      )
  )

; #3
(define (echo-all lst)
  (if (pair? lst)
      (cons (echo-all (car lst)) (echo-all-help lst (- 2 1))) lst)
  )

; #4
(define (nth i lst)
  (cond ((= i 0) (car lst))
        (else (nth (- i 1) (cdr lst)))
        )
  )

; #5
(define (filter fn lst)
  (cond ((null? lst) '())
        ((fn (car lst)) (cons (car lst) (filter fn (cdr lst))))
        (else  (filter fn (cdr lst)))
        )
  )

; #6
(define (filter-out-er fn lst)
  (cond ((null? lst) '())
        ((not (fn (car lst))) (cons (car lst) (filter-out-er fn (cdr lst))))
        (else (filter-out-er fn (cdr lst)))
        )
  )

