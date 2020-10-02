#lang eopl
;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------
;******************************Gramatica*************************************

; <Pila> :: 'Pila-vacia
;        :: push valor? <Pila>
;----------------------------------------------------------------------------

;; valor?:
;; Propósito: funcion predicado que retorna #t si v es un número un symbol
(define valor?
  (lambda (v)
    (cond
      [(symbol? v) #t]
      [(number? v) #t]
      [else #f]
      )
    )
  )
;prueba
;(valor? '())
;(valor? 5)
;-------------------------------------------------------------------
;; define-datatype:
;; Propósito: funcion que define un datatype pila
(define-datatype pila pila?
  (empty-stack)
  (push (valor valor?) (pila pila?))
  )
;-------------------------------------------------------------------
;; parse-exp:
;; Propósito: función que recibe una lista y retorna una estructura pila
(define parse-exp
  (lambda (L)
    (cond
      [(eqv? (car L) 'empty-stack) (empty-stack)]
      [(eqv? (car L) 'push) (push (cadr L) (parse-exp (caddr L)))]      
      [else #f]
      )
    )
  )

;;prueba
;(parse-exp '(push 10 (empty-stack)))
;(parse-exp '(empty-stack))
;-------------------------------------------------------------------
;; unparse-exp:
;; Propósito: función que recibe una estructura pila y retorna una lista 
(define unparse-exp
  (lambda (exp)
    (cases pila exp
      (empty-stack () (list 'empty-stack))
      (push (v p) (list 'push v (unparse-exp p)))
      (else #f)
      )
    )
  )

;prueba
;(unparse-exp (parse-exp '(push 10 (empty-stack))))
;(unparse-exp (parse-exp '(empty-stack)))
;------------------------------------------------------------------------
;; pop-datatype:
;; Propósito: función que elimina el ultimo elemento ingresado en la pila
(define pop-datatype
  (lambda (exp)
    (cases pila exp
      (empty-stack () (eopl:error "No se puede realizar pop en una pila vacía"))      
      (push (v p) p)      
      (else #f)
      )
    )
  )

;prueba
;(pop-datatype (parse-exp '(push 5(push 10 (empty-stack)))))
;(pop-datatype (parse-exp '(push 10 (empty-stack))))
;------------------------------------------------------------------------
;; pop-datatype:
;; Propósito: función que retorna el ultimo elemento que se ingresó en la pila
(define top-datatype
  (lambda (exp)
    (cases pila exp
      (empty-stack () (eopl:error "No se puede realizar pop en una pila vacía"))      
      (push (v p) v)     
      (else #f)
      )
    )
  )

;prueba
;(top-datatype (parse-exp '(push 5(push 10 (empty-stack)))))
;(top-datatype (parse-exp '(push 10 (empty-stack))))
;------------------------------------------------------------------------
;; push-datatype:
;; Propósito:
(define push-datatype
  (lambda (valor stack)
    (push valor stack)
    )
  )

;prueba
;(push-datatype 5 (empty-stack))
;(push-datatype 4 (push 5 (empty-stack)))
;------------------------------------------------------------------------

;; empty-stack?:
;; Propósito:
(define empty-stack?
  (lambda (exp)
    (cases pila exp
      (empty-stack () #t)
      (else #f)
      )
    )
  )
;prueba
;(empty-stack? (empty-stack))
;(empty-stack? (parse-exp '(push x (empty-stack))))
;-------------------------------------------------------------------
