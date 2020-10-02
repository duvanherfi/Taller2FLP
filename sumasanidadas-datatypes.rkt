#lang eopl
;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------
;******************************Gramatica*************************************
;suma-anidada ::= <valor> <numero>
;             ::=(<suma> suma-anidada suma-anidada)

;; define-datatype:
;; Propósito: se define un datatype suma-anidada
(define-datatype suma-anidada suma-anidada?
  (valor (v number?))
  (suma (izq suma-anidada?) (der suma-anidada?))
  )

;-------------------------------------------------------------------

;; parse-exp:
;; Propósito: función que recibe una lista y la convierte a una estructura
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
;(parse-exp '(suma (valor 5) (suma (valor 5) (valor 6))))
;-------------------------------------------------------------------

;; unparse-exp:
;; Propósito: función que recibe una estructura suma-anidada y retorna una lista 
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
;(unparse-exp (parse-exp '(suma (valor 5) (valor 6))))
;(unparse-exp (parse-exp '(suma (valor 5) (suma (valor 5) (valor 6)))))
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

;prueba
;(sum-anidada (parse-exp '(suma (valor 5) (suma (valor 5) (valor 6)))))
;(sum-anidada (parse-exp '(suma (valor 5) (valor 6))))