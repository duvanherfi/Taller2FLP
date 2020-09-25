#lang eopl

;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------

;; zero:
;; Propósito:
(define zero
  (lambda ()
    '()))

;; is-zero?:
;; Propósito:
(define is-zero?
  (lambda (n)
    (null? n)))

;; successor:
;; Propósito:
(define successor
  (lambda (n)
    (if (is-zero? n)
	'(1)
	(let ((t (+ (car n) 1)))
	  (if (= t 16)
	      (cons 0 (successor (cdr n)))
	      (cons t (cdr n))
              )
          )
        )
    )
  )


;; predecessor:
;; Propósito:
(define predecessor
  (lambda (n)
    (cond
     ((is-zero? n) (eopl:error "cero no tiene sucesor"))
     ((>= (car n) 16) (eopl:error "el valor debe ser menor que 16"))
     ((equal? n '(1)) '())
     ((zero? (car n))
      (if (null? (cdr n))
	  (eopl:error "cero no tiene sucessor")
	  (cons (- 16 1) (predecessor (cdr n)))
          )
      )
      (else (cons (- (car n) 1) (cdr n)))
      )
    )
  )

;; suma:
;; Propósito:
(define suma
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma (predecessor x) y)))))

;; resta:
;; Propósito:
(define resta
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta  x (predecessor y))))))

;; multiplicacion:
;; Propósito:
(define multiplicacion
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma (multiplicacion (predecessor x) y) y))
    ))

;; potencia:
;; Propósito:    
(define potencia
  (lambda (x y)
    (if (is-zero? y)
        (successor y)
        (multiplicacion (potencia x (predecessor y)) x))))


;; factorial:
;; Propósito:
(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor n)
        (multiplicacion n (factorial (predecessor n))))))