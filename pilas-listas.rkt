#lang eopl

;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------

;;;;;;;;;;;; GRAMÁTICA ;;;;;;;;;;;;;;;;;;;;;

; <árbol-binario> := ('vacio)
;                 := ('nodo <numero> <árbol-binario> <árbol-binario>)

;----------------------------------------------------------------------------

;; define empty-stack
;; Propósito: Función que crea un stack vacío

(define empty-stack
  (lambda ()
    (list 'empty-stack)))

;prueba
;(empty-stack) ; retorna (empty-stack)

;-------------------------------------------------------------------
;; define empty-stack?
;; Propósito: Función que verifica si un stack está vacío

(define empty-stack?
  (lambda (Pila)
    (and (eq? (car Pila) 'empty-stack) (null? (cdr Pila)))))

;prueba
;(vacio) ; retorna (vacio)

;-------------------------------------------------------------------
;; define push
;; Propósito: Función que agrega un valor a la pila

(define push
  (lambda (v Pila)
    (list 'stack v Pila)))

;prueba

;(push 8 p) ; retorna (stack 8 (stack 5 (stack 6 (stack 10 (empty-stack)))))
;(push 10 (push 3 (empty-stack))) ; retorna (stack 10 (stack 3 (empty-stack)))

;-------------------------------------------------------------------
;; define pop
;; Propósito: Función que quita un valor a la pila

(define pop
  (lambda (Pila)
    (if (empty-stack? Pila)
        (eopl:error "No se puede sacar elementos de una pila vacía")
        (if (eqv? (car Pila) 'stack)
            (caddr Pila)
            (eopl:error "Pila mal estructurada")))))

;prueba
;(pop p) ; retorna (stack 6 (stack 10 (empty-stack)))
;(pop (pop p)) ; retorna (stack 10 (empty-stack))
;(pop (empty-stack)) retorna "No se puede sacar elementos de una pila vacía"

;-------------------------------------------------------------------
;; define nodo
;; Propósito: Función que muestra el último valor agregado a la pila

(define top
  (lambda (Pila)
    (if (empty-stack? Pila)
        (eopl:error "La pila está vacía")
        (if (eqv? (car Pila) 'stack)
            (cadr Pila)
            (eopl:error "Pila mal estructurada")))))

;prueba

;(top p) ; retorna 5
;(top (empty-stack)) ; retorna "La pila está vacía"

;-------------------------------------------------------------------

(define p
  (push 5 (push 6 (push 10 (empty-stack)))))
