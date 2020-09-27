#lang eopl
;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------
; <Pila> :: 'Pila-vacia
;        :: push sheme-value <Pila>
;        :: pop <Pila>
;        :: top <Pila>

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

;; define-datatype:
;; Propósito:
(define-datatype pila pila?
  (empty-stack)
  (push (valor valor?) (pila pila?))
  (pop (pila pila?))
  (top (pila pila?))
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
      [(eqv? (car L) 'pop) (pop (parse-exp (cadr L)))]
      [(eqv? (car L) 'top) (top (parse-exp (cadr L)))]
      [else #f]
      )
    )
  )

;;prueba
;
;-------------------------------------------------------------------
;; unparse-exp:
;; Propósito:
(define unparse-exp
  (lambda (exp)
    (cases pila exp
      (empty-stack () (list 'empty-stack))
      (push (v p) (list 'push v (unparse-exp p)))
      (pop (p) (list 'pop (unparse-exp p)))
      (top (p) (list 'top (unparse-exp p)))
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
      (empty-stack () empty-stack)
      (top (p) #f)
      (push (v p) (push-datatype p))
      (pop (p) (pop-datatype p))      
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
      (empty-stack () empty-stack)
      (top (p) (top-datatype p))
      (push (v p) v)
      (pop (p) (pop-datatype p))      
      (else #f)
      )
    )
  )

;prueba
;
;
;------------------------------------------------------------------------
;; push-datatype:
;; Propósito:
(define push-datatype
  (lambda (exp)
    (cases pila exp
      (empty-stack () empty-stack)
      (top (p) (top-datatype p))
      (push (v p) (push v p))
      (pop (p) (pop-datatype p))      
      (else #f)
      )
    )
  )

;prueba
;
;
;------------------------------------------------------------------------