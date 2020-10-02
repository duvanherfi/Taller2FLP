#lang eopl

;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------

;;;;;;;;;;;; GRAMÁTICA ;;;;;;;;;;;;;;;;;;;;;

; <Pila> :: ('Pila-vacia)
;        :: ('Pila-valor scheme-value <Pila>)

;----------------------------------------------------------------------------

;; define empty-stack
;; Propósito: Función que crea un stack vacío y retorna un procedimiento

(define empty-stack-proc
  (lambda()
    (lambda (proc)
      (cond
       ((eqv? proc 'top-proc)
        (eopl:error "El stack está vacío"))
       ((eqv? proc 'pop-proc)
        (eopl:error "No se puede hacer pop en una lista vacía"))
       (else
        (eopl:error "Procedimiento desconocido"))))))

;prueba
;(empty-stack) ; retorna un procedimiento

;-------------------------------------------------------------------
;; define push-proc
;; Propósito: Función que recibe un valor y una pila y agrega el valor a a pila. retorna un procedimiento

(define push-proc
  (lambda (var saved-stack)
    (lambda (proc)
      (cond
       ((eqv? proc 'top-proc) var)
       ((eqv? proc 'pop-proc) saved-stack)
       (else
	(eopl:error "error de procedimiento"))))))

;prueba
;(push-proc 6 p) ; retorna un procedimiento
;(push-proc 10 (empty-stack-proc)) ; retorna un procedimiento

;-------------------------------------------------------------------
;; define pop-proc
;; Propósito: Función que recibe una pila, quita el último elemento agregado y retorna un procedimiento

(define pop-proc
  (lambda (stack)
    (stack 'pop-proc)))

;prueba
;(pop-proc p) ; retorna un procedimiento
;(pop-proc (push-proc 6 (push-proc 10 (empty-stack-proc)))) ; retorna un procedimiento

;-------------------------------------------------------------------
;; define top-proc
;; Propósito: Función que recibe una pila y muestra el valor del último elemento agregado a la pila

(define top-proc
  (lambda (stack)
    (stack 'top-proc)))

;prueba
;(top-proc p) ; retorna 5
;(top-proc (push-proc 6 (push-proc 10 (empty-stack-proc)))) ; retorna 6

;-------------------------------------------------------------------

(define p (push-proc 5 (push-proc 6 (push-proc 10 (empty-stack-proc)))))
