#lang eopl

;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------

;;;;;;;;;;;; GRAMÁTICA ;;;;;;;;;;;;;;;;;;;;;

; <árbol-binario> := ('vacio)
;                 := ('nodo <numero> <árbol-binario> <árbol-binario>)

;----------------------------------------------------------------------------

;; define vacio:
;; Propósito: Función que crea un árbol vacío

(define vacio
  (lambda ()
    (list 'vacio)))

;prueba
;(vacio) ; retorna (vacio)

;-------------------------------------------------------------------
;; define nodo
;; Propósito: Función que crea un nodo

(define nodo
  (lambda (num h1 h2)
    (list 'nodo num h1 h2)))

;prueba
;(nodo 4 (vacio) (vacio)) ; retorna (nodo 4 (vacio) (vacio))
;(nodo 10 (vacio) (vacio)) ; retorna (nodo 10 (vacio) (vacio))

;-------------------------------------------------------------------
;; define extractor-nodo
;; Propósito: Función que extrae el valor del nodo

(define extractor-nodo
  (lambda (arbol)
    (if (arbol-vacio? arbol)
        (eopl:error "El nodo está vacío")
        (cadr arbol))))

;pruebas:

;(extractor-nodo Arbol_Ejemplo4) ; retorna 8
;(extractor-nodo Arbol_Ejemplo6) ; retorna "El nodo está vacío"
;
;-------------------------------------------------------------------
;; define extractor-hijoizq
;; Propósito: Función que extrae el hijo izquierdo del árbol. Retorna "No tiene hijos" si se le ingresa una hoja (vacio) 

(define extractor-hijoizq
  (lambda (arbol)
    (if (arbol-vacio? arbol)
        (eopl:error "No tiene hijos")
        (caddr arbol))))

;pruebas:

;(extractor-hijoizq Arbol_Ejemplo1) ; retorna (nodo 1 (vacio) (vacio))
;(extractor-hijoizq Arbol_Ejemplo6) ; retorna "No tiene hijos"
;
;-------------------------------------------------------------------
;; define extractor-hijoder
;; Propósito: Función que extrae el hijo derecho del árbol. Retorna "No tiene hijos" si se le ingresa una hoja (vacio)

(define extractor-hijoder
  (lambda (arbol)
    (if (arbol-vacio? arbol)
        (eopl:error "No tiene hijos")
        (cadddr arbol))))

;pruebas:

;(extractor-hijoder Arbol_Ejemplo4) ; retorna (nodo 10 (vacio) (nodo 14 (nodo 13 (vacio) (vacio)) (vacio)))
;(extractor-hijoizq Arbol_Ejemplo6) ; retorna "No tiene hijos"

;-------------------------------------------------------------------
;; define arbol-vacio?
;; Propósito:  Función que verifica si un árbol está vacío

; Predicados

(define arbol-vacio?
        (lambda (arbol)
          (equal? (car arbol) 'vacio)))

;pruebas:

;(arbol-vacio? Arbol_Ejemplo1) ; retorna #f
;(arbol-vacio? Arbol_Ejemplo6) ; retorna #t

;-------------------------------------------------------------------
;; define arbol-hoja?
;; Propósito: Función que verifica si un árbol es una hoja

(define arbol-hoja?
        (lambda (arbol)
          (and
           (number? (cadr arbol))
           (equal? (caaddr arbol) 'vacio)
           (equal? (caar (cdddr arbol)) 'vacio))))

;prueba

;(arbol-hoja? Arbol_Ejemplo1) ; retorna #f
;(arbol-hoja? Arbol_Ejemplo3) ; retorna #t

;-------------------------------------------------------------------
;; define arbol-nodo?
;; Propósito: Función que verifica si un árbol es un nodo

(define arbol-nodo?
        (lambda (arbol)
          (and
           (equal? (car arbol) 'nodo)
           (or
            (equal? (caaddr arbol) 'nodo)
            (equal? (caar (cdddr arbol)) 'nodo)))))

;prueba

;(arbol-nodo? Arbol_Ejemplo1) ; retorna #t
;(arbol-nodo? Arbol_Ejemplo3) ; retorna #f
;
;-------------------------------------------------------------------
;; define validador-orden-bst
;; Propósito: Función que valida si un árbol se encuentra ordenado

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

;prueba

;(validador-orden-bst Arbol_Ejemplo1) ; retorna #t
;(validador-orden-bst Arbol_Ejemplo6) ; retorna Error: esta ingresando un arbol vacio
;(validador-orden-bst Arbol_Ejemplomalo) ; retorna #f

;-------------------------------------------------------------------
;; define insertar-elemento-abb
;; Propósito: Función que inserta un elemento en un árbol binario de búsqueda

(define insertar-elemento-abb
  (lambda (arbol num)
    (cond
      [(arbol-vacio? arbol) (nodo num (vacio) (vacio))]
      [(arbol-hoja? arbol)
       (if (= num (cadr arbol))
        arbol
        (if(< num (cadr arbol))
           (nodo (cadr arbol) (insertar-elemento-abb (caddr arbol) num) (vacio))
           (nodo (cadr arbol) (vacio) (insertar-elemento-abb (caddr arbol) num))))]
      [else
       (if (= num (cadr arbol))
           arbol
           (if
             (< num (cadr arbol))
             (cons (car arbol) (cons (cadr arbol) (cons (insertar-elemento-abb (caddr arbol) num) (cdddr arbol))))
             (cons (car arbol) (cons (cadr arbol) (cons (caddr arbol) (insertar-elemento-abb (cadddr arbol) num))))))])))

;prueba

;(insertar-elemento-abb Arbol_Ejemplo4 10) ; (nodo 8 (nodo 3 (nodo 1 (vacio) (vacio)) (nodo 6 (nodo 4 (vacio) (vacio)) (nodo 7 (vacio) (vacio)))) (nodo 10 (vacio) (nodo 14 (nodo 13 (vacio) (vacio)) (vacio))))
;(insertar-elemento-abb Arbol_Ejemplo4 2) ; (nodo 8 (nodo 3 (nodo 1 (vacio) (nodo 2 (vacio) (vacio))) (nodo 6 (nodo 4 (vacio) (vacio)) (nodo 7 (vacio) (vacio)))) (nodo 10 (vacio) (nodo 14 (nodo 13 (vacio) (vacio)) (vacio))))
;(insertar-elemento-abb Arbol_Ejemplo3 5) ; (nodo 5 (vacio) (vacio))) 
;(insertar-elemento-abb Arbol_Ejemplo6 5) ; (nodo 5 (vacio) (vacio)))

;-------------------------------------------------------------------

;;;;;;;;;;;; DEFINICIÓN DE ÁRBOLES ;;;;;;;;;;;;;;;;;;;;;

(define Arbol_Ejemplo1 (nodo 5 (nodo 1 (vacio) (vacio)) (nodo 7 (vacio) (vacio)))) 
(define Arbol_Ejemplo2 (nodo 9 (nodo 2 (vacio) (vacio)) (vacio)))
(define Arbol_Ejemplo3 (nodo 5 (vacio) (vacio))) 
(define Arbol_Ejemplo4 (nodo 8 (nodo 3 (nodo 1 (vacio) (vacio)) (nodo 6 (nodo 4 (vacio) (vacio)) (nodo 7 (vacio) (vacio)))) (nodo 10 (vacio) (nodo 14 (nodo 13 (vacio) (vacio)) (vacio))))) 
(define Arbol_Ejemplo6 (vacio))
(define Arbol_Ejemplomalo (nodo 7 (nodo 5 (vacio) (vacio)) (nodo 1 (vacio) (vacio))))
