#lang eopl
;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------

;Interfaz: Definición de funciones para sumas anidadas con representación utilizando Data-type

;suma-anidada ::= <valor> <numero>
;             ::=(<suma> suma-anidada suma-anidada)

;; define-datatype:
;; Propósito:
(define-datatype suma-anidada suma-anidada?
  (valor (v number?))
  (suma (izq suma-anidada?) (der suma-anidada?))
  )

;prueba
;
;
;-------------------------------------------------------------------

;; parse-exp:
;; Propósito:
(define parse-exp
  (lambda (L)
    (cond
      [(eqv? (car L) 'valor) (valor (cadr L))]
      [(eqv? (car L) 'suma)
       (suma (parse-exp (cadr L)) (parse-exp (caddr L)))]
      [else #f]
      )
    )
  )

;;prueba
;(parse-exp '(suma (valor 5) (valor 6)))
;-------------------------------------------------------------------

;; unparse-exp:
;; Propósito:
(define unparse-exp
  (lambda (exp)
    (cases suma-anidada exp
      (valor (v) (list 'valor v))
      (suma (izq der)
                  (list 'suma (unparse-exp izq) (unparse-exp der))
                  )
      ))
  )

;prueba
;
;
;------------------------------------------------------------------------
(define sum-anidada
  (lambda (exp)
    (cases suma-anidada exp
      (valor (v)  v)
      (suma (izq der)
                  (+ (sum-anidada  izq) (sum-anidada der))
                  )
      ))
  )