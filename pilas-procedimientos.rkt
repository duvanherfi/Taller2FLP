#lang eopl

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

(define push-proc
  (lambda (var saved-stack)
    (lambda (proc)
      (cond
       ((eqv? proc 'top-proc) var)
       ((eqv? proc 'pop-proc) saved-stack)
       (else
	(eopl:error "error de procedimiento"))))))

(define pop-proc
  (lambda (stack)
    (stack 'pop-proc)))

(define top-proc
  (lambda (stack)
    (stack 'top-proc)))

(define p (push-proc 5 (push-proc 6 (push-proc 10 (empty-stack-proc)))))

;(top-proc (pop-proc (push-proc 10 (empty-stack-proc)))) ;-----> "El stack está vacío"
;(top-proc (push-proc 10 (empty-stack-proc))) ;----> 10
