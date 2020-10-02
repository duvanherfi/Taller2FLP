#lang eopl

;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------

;; zero:
;; Propósito: función que retorna una lista vacía en representación del zero
(define zero
  (lambda ()
    '()))

;prueba
;(zero)
;----------------------------------------------------------------------------
;; is-zero?:
;; Propósito: Función que retorna #t si la lista es zero.
(define is-zero?
  (lambda (n)
    (null? n)))

;prueba
;(is-zero? '())
;(is-zero? '(1))
;----------------------------------------------------------------------------

;; successor:
;; Propósito: función que recibe una lista bignum en base 16 y retorna el sucesor de dicha lista
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
;prueba
;(successor '())
;(successor '(1 1))
;----------------------------------------------------------------------------


;; predecessor:
;; Propósito: función que recibe una lista bignum en base 16 y retorna el predecesor de dicha lista
(define predecessor
  (lambda (n)
    (cond
     ((is-zero? n) (eopl:error "cero no tiene sucesor"))
     ((>= (car n) 16) (eopl:error "el valor debe ser menor que 16"))
     ((equal? n '(1)) '())
     ((zero? (car n))
      (if (null? (cdr n))
	  (eopl:error "cero no tiene sucessor")
	  (cons 15 (predecessor (cdr n)))
          )
      )
      (else (cons (- (car n) 1) (cdr n)))
      )
    )
  )

;prueba
;(predecessor '())
;(predecessor '(1 1))
;----------------------------------------------------------------------------

;; suma:
;; Propósito:
(define suma
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma (predecessor x) y)))))

;prueba
;(suma '(1 1) '(1 1))
;(suma '(0 1) '(1 1))
;----------------------------------------------------------------------------
;; resta:
;; Propósito:
(define resta
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta  x (predecessor y))))))

;prueba
;(resta '(1 1) '(1 2))
;(resta '(0 1) '(1 1))
;----------------------------------------------------------------------------

;; multiplicacion:
;; Propósito:
(define multiplicacion
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma (multiplicacion (predecessor x) y) y))
    ))

;prueba
;(multiplicacion '(1 1) '(1 2))
;(multiplicacion '(0 1) '(1 1))
;----------------------------------------------------------------------------

;; potencia:
;; Propósito:    
(define potencia
  (lambda (x y)
    (if (is-zero? y)
        (successor y)
        (multiplicacion (potencia x (predecessor y)) x))))

;prueba
;(potencia '(1 1) '(2))
;(potencia '(15) '(2)) 
;----------------------------------------------------------------------------


;; factorial:
;; Propósito:
(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor n)
        (multiplicacion n (factorial (predecessor n))))))

;prueba
;(factorial '(5))
;(factorial '(7))