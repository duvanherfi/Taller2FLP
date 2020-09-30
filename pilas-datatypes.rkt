#lang eopl
;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------
; <Pila> :: 'Pila-vacia
;        :: push valor? <Pila>

; empty-stack, push, pop, top, empty-stack?

;; valor?:
;; Propósito:
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
;
;
;-------------------------------------------------------------------
;; define-datatype:
;; Propósito:
(define-datatype pila pila?
  (empty-stack)
  (push (valor valor?) (pila pila?))
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
      [(eqv? (car L) 'empty-stack) (empty-stack)]
      [(eqv? (car L) 'push) (push (cadr L) (parse-exp (caddr L)))]      
      [else #f]
      )
    )
  )

;;prueba
;
;
;-------------------------------------------------------------------
;; unparse-exp:
;; Propósito:
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
;
;
;------------------------------------------------------------------------
;; pop-datatype:
;; Propósito:
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
;
;
;------------------------------------------------------------------------
;; pop-datatype:
;; Propósito:
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
;

;------------------------------------------------------------------------
;; push-datatype:
;; Propósito:
(define push-datatype
  (lambda (valor stack)
    (push valor stack)
    )
  )

;prueba
;
;
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
;
(empty-stack? (parse-exp '(push x (empty-stack))))
;-------------------------------------------------------------------
