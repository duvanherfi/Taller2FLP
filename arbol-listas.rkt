#lang eopl

; <árbol-binario> := ('vacio)
;                 := ('nodo <numero> <árbol-binario> <árbol-binario>)


;;;; CONSTRUCTORES

(define vacio
  (lambda ()
    (list 'vacio)))

(define nodo
  (lambda (num h1 h2)
    (list 'nodo num h1 h2)))

;;;; OBSERVADORES

; Extractores

(define extractor-nodo
  (lambda (arbol)
    (cadr arbol)))

(define extractor-hijoizq
  (lambda (arbol)
    (caddr arbol)))

(define extractor-hijoder
  (lambda (arbol)
    (cadddr arbol)))

; Predicados

(define arbol-vacio?
        (lambda (arbol)
          (equal? (car arbol) 'vacio)))

(define arbol-hoja?
        (lambda (arbol)
          (and
           (number? (cadr arbol))
           (equal? (caaddr arbol) 'vacio)
           (equal? (caar (cdddr arbol)) 'vacio))))

(define arbol-nodo?
        (lambda (arbol)
          (and
           (equal? (car arbol) 'nodo)
           (or
            (equal? (caaddr arbol) 'nodo)
            (equal? (caar (cdddr arbol)) 'nodo)))))

(define validador-orden-bst
  (lambda (arbol)
    (cond
      [(arbol-vacio? arbol) (eopl:error "Error: esta ingresando un árbol vacío")]
      [(arbol-hoja? arbol) #t]
      [(and
        (if(arbol-vacio? (extractor-hijoizq arbol)) #t (> (cadr arbol) (cadr (extractor-hijoizq arbol))))
        (if(arbol-vacio? (extractor-hijoder arbol)) #t (< (cadr arbol) (cadr (extractor-hijoder arbol)))))
       (and
        (if(arbol-vacio? (extractor-hijoizq arbol)) #t (validador-orden-bst (extractor-hijoizq arbol)))
        (if(arbol-vacio? (extractor-hijoder arbol)) #t (validador-orden-bst (extractor-hijoder arbol))))]
      [else #f])))
#|
(define insertar-elemento-abb
  (lambda (arbol num)
    (cond
      [(arbol-vacio? arbol) (nodo num (vacio) (vacio))]
      [(arbol-hoja? arbol) 
       (if(< num (cadr arbol))
          (nodo (cadr arbol) (insertar-elemento-abb (caddr arbol) num) (vacio))
          (nodo (cadr arbol) (vacio) (insertar-elemento-abb (caddr arbol) num)))]
      [else (if
        (arbol-vacio? (extractor-hijoizq arbol))
        (if
         (< num (cadr arbol))
         (nodo (cadr arbol) (insertar-elemento-abb (caddr arbol) num) (cadddr arbol))
         (#f))
        (if
         (< num (cadr arbol))
         (cons (car arbol) (cons (cadr arbol) (cons (insertar-elemento-abb (caddr arbol) num) (cdddr arbol))))
         (cons (car arbol) (cons (cadr arbol) (cons (caddr arbol) (insertar-elemento-abb (cadddr arbol) num))))
         ))])))
|#
(define insertar-elemento-abb
  (lambda (arbol num)
    (cond
      [(arbol-vacio? arbol) (nodo num (vacio) (vacio))]
      [(arbol-hoja? arbol) 
       (if(< num (cadr arbol))
          (nodo (cadr arbol) (insertar-elemento-abb (caddr arbol) num) (vacio))
          (nodo (cadr arbol) (vacio) (insertar-elemento-abb (caddr arbol) num)))]



      
      [(> (value tree) n) (merge (insert (left tree) n) (value tree)(right tree))] ; internal - go left
      [(< (value tree) n) (merge (left tree) (value tree) (insert (right tree) n))] ; internal - go right
      ;[(eq? (value tree) n) tree] ; internal - n already in tree
    )
  )

(define ab '(1 2 3 4))

(define Arbol_Ejemplo1 (nodo 5 (nodo 1 (vacio) (vacio)) (nodo 7 (vacio) (vacio)))) 
(define Arbol_Ejemplo2 (nodo 9 (nodo 2 (vacio) (vacio)) (vacio)))
(define Arbol_Ejemplo3 (nodo 5 (vacio) (vacio))) 
(define Arbol_Ejemplo4 (nodo 8 (nodo 3 (nodo 1 (vacio) (vacio)) (nodo 6 (nodo 4 (vacio) (vacio)) (nodo 7 (vacio) (vacio)))) (nodo 10 (vacio) (nodo 14 (nodo 13 (vacio) (vacio)) (vacio))))) 
(define Arbol_Ejemplo6 (vacio))
(define Arbol_Ejemplomalo (nodo 7 (nodo 5 (vacio) (vacio)) (nodo 1 (vacio) (vacio))))


;Extractores del valor del nodo, del hijo izquierdo y del hijo derecho(1 punto c/u - Total 3)
(extractor-hijoizq Arbol_Ejemplo1) ; retorna (1 () ())
(extractor-hijoder Arbol_Ejemplo4) ; retorna (10 () (14 (13 () ()) ()))
(extractor-nodo Arbol_Ejemplo4) ; retorna 8

;Predicados: ́arbol-vacio?, ́arbol-hoja?, ́arbol-nodo? (1 punto c/u - Total 3)
(arbol-hoja? Arbol_Ejemplo1) ; retorna #f
(arbol-nodo? Arbol_Ejemplo1) ; retorna #t
(arbol-vacio? Arbol_Ejemplo1) ; retorna #f

; Validar orden e insertar elemento (2 puntos c/u - Total 10)
(validador-orden-bst Arbol_Ejemplo1) ; retorna #t
;(validador-orden-bst Arbol_Ejemplo6) ; retorna Error: esta ingresando un arbol vacio
(validador-orden-bst Arbol_Ejemplomalo) ; retorna #f

;(insertar-elemento-abb Arbol_Ejemplo4 10) ; (8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ())))
;(insertar-elemento-abb Arbol_Ejemplo4 2) ; (8 (3 (1 () (2 () ())) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ())))
