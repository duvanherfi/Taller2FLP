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

;prueba
;(zero)
;---------------------------------------------------------
;; is-zero?:
;; Propósito:
(define is-zero?
  (lambda (n)
    (null? n)))

;prueba
;
;
;----------------------------------------------------------

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
;prueba
;
;
;-------------------------------------------------------------------


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
	  (cons 15 (predecessor (cdr n)))
          )
      )
      (else (cons (- (car n) 1) (cdr n)))
      )
    )
  )

;prueba
;
;
;-------------------------------------------------------------------

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
;-------------------------------------------------------------------
;; resta:
;; Propósito:
(define resta
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta  x (predecessor y))))))

;prueba
;
;
;-------------------------------------------------------------------

;; multiplicacion:
;; Propósito:
(define multiplicacion
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma (multiplicacion (predecessor x) y) y))
    ))

;prueba
;
;
;-------------------------------------------------------------------

;; potencia:
;; Propósito:    
(define potencia
  (lambda (x y)
    (if (is-zero? y)
        (successor y)
        (multiplicacion (potencia x (predecessor y)) x))))

;prueba
;
;
;-------------------------------------------------------------------


;; factorial:
;; Propósito:
(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor n)
        (multiplicacion n (factorial (predecessor n))))))

;prueba
;
;
;-------------------------------------------------------------------