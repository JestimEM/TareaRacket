#lang racket
(provide (all-defined-out))

;; ==========================================
;; 1. CREACIÓN DE LA MATRIZ
;; ==========================================

;; Funcion: crear-fila
;; Genera una lista recursiva de ceros.
;; Entradas: m (número de columnas que tendrá la fila)
;; Salidas: Una lista con 'm' elementos, todos en 0.
(define (crear-fila m)
  (cond ((zero? m) '())
        (else (cons 0 (crear-fila (- m 1))))))

;; Funcion: crear-matriz
;; Construye el tablero inicial formando una lista de listas llenas de ceros.
;; Entradas: n (número de filas), m (número de columnas)
;; Salidas: Una matriz de n*m llena de ceros.
(define (crear-matriz n m)
  (cond ((zero? n) '())
        (else (cons (crear-fila m) (crear-matriz (- n 1) m)))))

;; ==========================================
;; 2. BÚSQUEDA Y CONDICIONES DE VICTORIA/DERROTA
;; ==========================================

;; Funcion: buscar-en-fila
;; Recorre una fila para encontrar si un valor 'x' existe dentro de ella.
;; Entradas: x (valor a buscar), fila (la lista donde se va a buscar)
;; Salidas: #t si encuentra 'x', #f en caso contrario.
(define (buscar-en-fila x fila)
  (cond ((null? fila) #f)
        ((equal? x (car fila)) #t)
        (else (buscar-en-fila x (cdr fila)))))

;; Funcion: buscar-x
;; Recorre todas las filas de la matriz buscando un valor 'x'.
;; Entradas: x (valor a buscar), matriz (la lista de listas completa)
;; Salidas: #t si el valor 'x' se encuentra en alguna parte de la matriz, #f en caso contrario.
(define (buscar-x x matriz)
  (cond ((null? matriz) #f)
        ((buscar-en-fila x (car matriz)) #t)
        (else (buscar-x x (cdr matriz)))))

;; Funcion: adyacentes-en-fila?
;; Verifica si existen dos casillas consecutivas con el mismo valor dentro de una fila.
;; Entradas: fila (lista de valores a evaluar)
;; Salidas: #t si hay valores iguales adyacentes, #f en caso contrario.
(define (adyacentes-en-fila? fila)
  (cond ((null? fila) #f)
        ((null? (cdr fila)) #f)
        ((equal? (car fila) (car (cdr fila))) #t)
        (else (adyacentes-en-fila? (cdr fila)))))

;; Funcion: adyacentes-en-matriz?
;; Verifica fila por fila si en la matriz existen valores adyacentes iguales.
;; Entradas: matriz (lista de listas a evaluar)
;; Salidas: #t si hay alguna fila con adyacentes iguales, #f en caso contrario.
(define (adyacentes-en-matriz? matriz)
  (cond ((null? matriz) #f)
        ((adyacentes-en-fila? (car matriz)) #t)
        (else (adyacentes-en-matriz? (cdr matriz)))))

;; Funcion: sin-movimientos?
;; Evalúa las condiciones de derrota (sin ceros y sin adyacentes que puedan sumarse ni vertical ni horizontalmente).
;; Entradas: matriz (tablero actual del juego)
;; Salidas: #t si el jugador ya no puede hacer ningún movimiento, #f si aún puede jugar.
(define (sin-movimientos? matriz)
  (cond ((buscar-x 0 matriz) #f)
        ((adyacentes-en-matriz? matriz) #f)
        ((adyacentes-en-matriz? (transpuesta matriz)) #f)
        (else #t)))

;; Funcion: ganaste?
;; Revisa si el tablero ya contiene el número objetivo (2048).
;; Entradas: matriz (tablero actual del juego)
;; Salidas: #t si existe la casilla 2048, #f si aún no se ha logrado.
(define (ganaste? matriz)
  (buscar-x 2048 matriz))

;; Funcion: extraer-elemento
;; Navega a través de una lista para retornar el elemento que se encuentra en un índice específico.
;; Entradas: index (posición entera a buscar), lista (lista a recorrer)
;; Salidas: El elemento en la posición 'index', o lista vacía si el índice no existe.
(define (extraer-elemento index lista)
  (cond ((null? lista) '())
        ((zero? index) (car lista))
        (else (extraer-elemento (- index 1) (cdr lista)))))

;; Funcion: extraer-mat
;; Retorna el valor contenido en una coordenada específica (fila, columna) de la matriz.
;; Entradas: fila (índice Y), col (índice X), matriz (el tablero)
;; Salidas: El número ubicado en esa coordenada.
(define (extraer-mat fila col matriz)
  (extraer-elemento col (extraer-elemento fila matriz)))

;; ==========================================
;; 3. INSERCIÓN DE FICHAS 
;; ==========================================

;; Funcion: aplastar-matriz
;; Toma la matriz de dos dimensiones y la convierte en una sola lista continua.
;; Entradas: mat (matriz original)
;; Salidas: Una lista simple de 1 dimensión con todos los elementos de la matriz.
(define (aplastar-matriz mat)
  (cond ((null? mat) '())
        (else (append (car mat) (aplastar-matriz (cdr mat))))))

;; Funcion: contar-ceros
;; Cuenta cuántas casillas vacías (valor 0) hay en una lista.
;; Entradas: lista (lista plana de números)
;; Salidas: Un número entero que representa la cantidad de ceros.
(define (contar-ceros lista)
  (cond ((null? lista) 0)
        ((zero? (car lista)) (+ 1 (contar-ceros (cdr lista))))
        (else (contar-ceros (cdr lista)))))

;; Funcion: generar-numero
;; Devuelve un número base para insertar, calculando 10% de probabilidad para un 4 y 90% para un 2.
;; Entradas: Ninguna.
;; Salidas: El número 2 o el número 4.
(define (generar-numero)
  (cond ((zero? (random 10)) 4)
        (else 2)))

;; Funcion: reemplazar-n-cero
;; Busca el n-ésimo cero dentro de la lista plana y lo reemplaza por una ficha nueva.
;; Entradas: lista (lista plana), n (índice del cero que se decidió reemplazar)
;; Salidas: La lista plana modificada con el nuevo número insertado.
(define (reemplazar-n-cero lista n)
  (cond ((null? lista) '())
        ((zero? (car lista))
         (cond ((zero? n) (cons (generar-numero) (cdr lista)))
               (else (cons 0 (reemplazar-n-cero (cdr lista) (- n 1))))))
        (else (cons (car lista) (reemplazar-n-cero (cdr lista) n)))))

;; Funcion: tomar-m
;; Extrae la cantidad 'm' de números indicados desde el inicio de una lista plana.
;; Entradas: lista (lista plana de números), m (cantidad de elementos a tomar)
;; Salidas: Una lista con los 'm' primeros elementos.
(define (tomar-m lista m)
  (cond ((zero? m) '())
        ((null? lista) '())
        (else (cons (car lista) (tomar-m (cdr lista) (- m 1))))))

;; Funcion: quitar-m
;; Borra la cantidad 'm' de números indicados desde el inicio de la lista y devuelve lo sobrante.
;; Entradas: lista (lista plana de números), m (cantidad de elementos a quitar)
;; Salidas: La lista plana sin los primeros 'm' elementos.
(define (quitar-m lista m)
  (cond ((zero? m) lista)
        ((null? lista) '())
        (else (quitar-m (cdr lista) (- m 1)))))

;; Funcion: reconstruir-matriz
;; Toma una lista plana y la corta en trozos de tamaño 'm' para volver a armar las filas y columnas.
;; Entradas: lista (lista plana con el nuevo número ya insertado), m (tamaño de la fila)
;; Salidas: La matriz (lista de listas) completamente reconstruida.
(define (reconstruir-matriz lista m)
  (cond ((null? lista) '())
        (else (cons (tomar-m lista m) (reconstruir-matriz (quitar-m lista m) m)))))

;; Funcion: insertar-una-segura
;; Identifica la cantidad de espacios vacíos reales y coloca 1 ficha de forma segura si hay campo disponible.
;; Entradas: matriz (tablero antes de insertar), m (número de columnas)
;; Salidas: La matriz con una ficha nueva agregada, o la matriz intacta si ya estaba llena.
(define (insertar-una-segura matriz m)
  (cond ((zero? (contar-ceros (aplastar-matriz matriz))) matriz)
        (else (reconstruir-matriz 
               (reemplazar-n-cero (aplastar-matriz matriz) (random (contar-ceros (aplastar-matriz matriz))))
               m))))

;; Funcion: insertar-dos-casillas
;; Invoca la inserción segura dos veces consecutivas para el inicio de un juego nuevo.
;; Entradas: matriz (matriz vacía), m (número de columnas)
;; Salidas: El tablero inicial con 2 fichas al azar.
(define (insertar-dos-casillas matriz m)
  (insertar-una-segura (insertar-una-segura matriz m) m))

;; ==========================================
;; 4. COLISIONES, SUMAS Y MANIPULACIÓN
;; ==========================================

;; Funcion: quitar-ceros
;; Remueve los ceros de una fila para agrupar las casillas con número (simulando un deslizamiento).
;; Entradas: fila (lista con los números y ceros mezclados)
;; Salidas: La fila empujada con solo los números existentes.
(define (quitar-ceros fila)
  (cond ((null? fila) '())
        ((zero? (car fila)) (quitar-ceros (cdr fila)))
        (else (cons (car fila) (quitar-ceros (cdr fila))))))

;; Funcion: sumar-fila
;; Recorre una fila (previamente agrupada) para sumar las fichas adyacentes si tienen el mismo valor.
;; Entradas: fila (lista de números deslizados sin ceros de por medio)
;; Salidas: La fila habiendo combinado/sumado los números iguales y colisionados.
(define (sumar-fila fila)
  (cond ((null? fila) '())
        ((null? (cdr fila)) fila)
        ((equal? (car fila) (car (cdr fila)))
         (cons (+ (car fila) (car (cdr fila))) (sumar-fila (cdr (cdr fila)))))
        (else (cons (car fila) (sumar-fila (cdr fila))))))

;; Funcion: rellenar-ceros
;; Restaura el tamaño de la fila agregando la cantidad de ceros necesaria a la derecha.
;; Entradas: fila (lista sumada que probablemente es más corta ahora), m (tamaño meta de la fila)
;; Salidas: La fila restaurada a su tamaño original 'm'.
(define (rellenar-ceros fila m)
  (cond ((zero? m) '())
        ((null? fila) (cons 0 (rellenar-ceros '() (- m 1))))
        (else (cons (car fila) (rellenar-ceros (cdr fila) (- m 1))))))

;; Funcion: mover-fila-izq
;; Realiza el proceso completo de movimiento horizontal para 1 sola fila: empuja, suma y rellena vacíos.
;; Entradas: fila (fila de la matriz), m (tamaño de la fila)
;; Salidas: La fila completamente calculada tras un movimiento a la izquierda.
(define (mover-fila-izq fila m)
  (rellenar-ceros (sumar-fila (quitar-ceros fila)) m))

;; Funcion: sacar-1f
;; Extrae el primer elemento de todas las filas de la matriz para ayudar a crear una transpuesta.
;; Entradas: mat (matriz actual)
;; Salidas: Una lista conteniendo la primera columna de la matriz.
(define (sacar-1f mat)
  (cond ((null? mat) '())
        (else (cons (car (car mat)) (sacar-1f (cdr mat))))))

;; Funcion: borrar-1f
;; Elimina la primera columna de una matriz y devuelve el remanente.
;; Entradas: mat (matriz actual)
;; Salidas: La matriz sin los primeros elementos de cada fila.
(define (borrar-1f mat)
  (cond ((null? mat) '())
        (else (cons (cdr (car mat)) (borrar-1f (cdr mat))))))

;; Funcion: transpuesta
;; Intercambia geométricamente las filas por columnas utilizando llamadas recursivas.
;; Entradas: mat (matriz actual)
;; Salidas: La matriz girada o transpuesta.
(define (transpuesta mat)
  (cond ((null? (car mat)) '())
        (else (cons (sacar-1f mat) (transpuesta (borrar-1f mat))))))

;; Funcion: reversa-lista
;; Invierte por completo el orden de los elementos que se encuentren dentro de una lista.
;; Entradas: lista (una fila de la matriz)
;; Salidas: La lista invertida.
(define (reversa-lista lista)
  (cond ((null? lista) '())
        (else (append (reversa-lista (cdr lista)) (cons (car lista) '())))))

;; Funcion: reversa-matriz
;; Invierte individualmente cada fila dentro de la matriz, preparando el tablero para movimientos a la derecha.
;; Entradas: matriz (tablero actual)
;; Salidas: La matriz con sus filas invertidas en modo espejo.
(define (reversa-matriz matriz)
  (cond ((null? matriz) '())
        (else (cons (reversa-lista (car matriz)) (reversa-matriz (cdr matriz))))))

;; ==========================================
;; 5. MOVIMIENTOS BASE
;; ==========================================

;; Funcion: mover-izq-base
;; Mueve estrictamente a la izquierda matemáticamente todas las filas de la matriz.
;; Entradas: matriz (tablero actual), m (número de columnas)
;; Salidas: La matriz tras el movimiento (no incluye insertar nuevas piezas).
(define (mover-izq-base matriz m)
  (cond ((null? matriz) '())
        (else (cons (mover-fila-izq (car matriz) m) (mover-izq-base (cdr matriz) m)))))

;; Funcion: mover-der-base
;; Ejecuta el movimiento a la derecha aplicando el truco matemático de invertir, mover izquierda, invertir.
;; Entradas: matriz (tablero actual), m (número de columnas)
;; Salidas: La matriz resultante tras moverse a la derecha.
(define (mover-der-base matriz m)
  (reversa-matriz (mover-izq-base (reversa-matriz matriz) m)))

;; Funcion: mover-arriba-base
;; Ejecuta el movimiento arriba aplicando transpuesta, movimiento izquierdo, y devolviendo la transpuesta.
;; Entradas: matriz (tablero actual), m (número de columnas)
;; Salidas: La matriz movida hacia arriba de manera estricta.
(define (mover-arriba-base matriz m)
  (transpuesta (mover-izq-base (transpuesta matriz) m)))

;; Funcion: mover-abajo-base
;; Ejecuta el movimiento hacia abajo aplicando transpuesta, movimiento derecho, y devolviendo la transpuesta.
;; Entradas: matriz (tablero actual), m (número de columnas)
;; Salidas: La matriz calculada para el desplazamiento a la zona inferior.
(define (mover-abajo-base matriz m)
  (transpuesta (mover-der-base (transpuesta matriz) m)))

;; ==========================================
;; 6. MOVIMIENTOS FINALES
;; ==========================================

;; Funcion: mover-izq
;; Engloba el movimiento final del turno hacia la izquierda: mueve matemáticamente y, de haber un cambio, inserta ficha.
;; Entradas: matriz (estado actual), m (columnas)
;; Salidas: El estado del tablero para el siguiente turno.
(define (mover-izq matriz m)
  (cond ((equal? matriz (mover-izq-base matriz m)) matriz)
        (else (insertar-una-segura (mover-izq-base matriz m) m))))

;; Funcion: mover-der
;; Engloba el movimiento final del turno hacia la derecha: mueve matemáticamente y, de haber un cambio, inserta ficha.
;; Entradas: matriz (estado actual), m (columnas)
;; Salidas: El estado del tablero para el siguiente turno.
(define (mover-der matriz m)
  (cond ((equal? matriz (mover-der-base matriz m)) matriz)
        (else (insertar-una-segura (mover-der-base matriz m) m))))

;; Funcion: mover-arriba
;; Engloba el movimiento final del turno hacia arriba: mueve matemáticamente y, de haber un cambio, inserta ficha.
;; Entradas: matriz (estado actual), m (columnas)
;; Salidas: El estado del tablero para el siguiente turno.
(define (mover-arriba matriz m)
  (cond ((equal? matriz (mover-arriba-base matriz m)) matriz)
        (else (insertar-una-segura (mover-arriba-base matriz m) m))))

;; Funcion: mover-abajo
;; Engloba el movimiento final del turno hacia abajo: mueve matemáticamente y, de haber un cambio, inserta ficha.
;; Entradas: matriz (estado actual), m (columnas)
;; Salidas: El estado del tablero para el siguiente turno.
(define (mover-abajo matriz m)
  (cond ((equal? matriz (mover-abajo-base matriz m)) matriz)
        (else (insertar-una-segura (mover-abajo-base matriz m) m))))