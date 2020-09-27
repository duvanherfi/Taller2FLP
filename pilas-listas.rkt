#lang eopl

;;;;;;;;;;;; GRAMÁTICA ;;;;;;;;;;;;;;;;;;;;;

; <Pila> :: ('Pila-vacia)
;        :: ('Pila-valor scheme-value <Pila>)

; empty-stack, push, pop, top, empty-stack?

(define empty-stack
  (lambda ()
    (list 'empty-stack)))

(define empty-stack?
  (lambda (Pila)
    (and (eq? (car Pila) 'empty-stack) (null? (cdr Pila)))))

(define push
  (lambda (v Pila)
    (list 'stack v Pila)))

(define pop
  (lambda (Pila)
    (if (empty-stack? Pila)
        (eopl:error "No de puede sacar elementos de una pila vacía")
        (if (eqv? (car Pila) 'stack)
            (caddr Pila)
            (eopl:error "Pila mal estructurada")))))

(define top
  (lambda (Pila)
    (if (empty-stack? Pila)
        (eopl:error "La pila está vacía")
        (if (eqv? (car Pila) 'stack)
            (cadr Pila)
            (eopl:error "Pila mal estructurada")))))

(define p
  (push 5 (push 6 (push 10 (empty-stack)))))

;PILAS LISTAS

;(pop p) ;---> (stack 6 (stack 10 (empty-stack)))
;(top p) ;---> 5