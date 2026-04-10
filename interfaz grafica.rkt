#lang racket
(require racket/gui/base)
(require "logica.rkt")

;; ==========================================
;; VARIABLES DE ESTADO GLOBALES Y ANIMACIÓN
;; ==========================================
;; Estas variables mantienen el estado de la interfaz sin reiniciar el programa.
(define filas 4)
(define columnas 4)
(define tablero '())
(define estado-juego 'jugando) ;; Posibles estados: 'jugando, 'ganaste, 'perdiste, 'continuando

;; Variables exclusivas para controlar el motor de renderizado y animaciones
(define animando? #f)          ;; Bandera para saber si hay una animación en curso (bloquea teclado)
(define tablero-viejo '())     ;; Guarda la "foto" del tablero antes de moverse
(define tablero-futuro '())    ;; Guarda el resultado matemático final
(define direccion-anim 'left)  ;; Dirección hacia la que patinan las fichas
(define matriz-destinos #())   ;; Matriz auxiliar con las coordenadas exactas de parada
(define anim-frames 0)         ;; Contador del fotograma actual
(define MAX-FRAMES 7)          ;; Cantidad total de fotogramas por movimiento (define la velocidad)

;; ==========================================
;; FUNCIÓN DE VALIDACIÓN DE ENTRADAS
;; ==========================================

;; Funcion: validar-entrada
;; Descripción: Actúa como un filtro de seguridad (sanitización). Recibe el texto escrito por 
;; el usuario en las cajas de configuración y verifica que sea un número entero válido 
;; dentro del rango permitido para que el juego no colapse ni se congele.
;; Entradas: texto (String ingresado por el usuario)
;; Salidas: El número convertido a entero si es válido. Retorna #f (falso) si tiene letras, 
;; decimales o está fuera del rango (menor a 4 o mayor a 10).
(define (validar-entrada texto)
  (let ((num (string->number texto)))
    (if (and num (integer? num) (>= num 4) (<= num 10))
        num
        #f)))

;; ==========================================
;; LÓGICA DE CÁLCULO DE DESTINOS INDIVIDUALES
;; ==========================================

;; Funcion: obtener-fila
;; Descripción: Extrae una fila específica de la matriz utilizando coordenadas exactas.
;; Entradas: mat (matriz actual), f (índice de la fila), cols (cantidad de columnas)
;; Salidas: Una lista plana con los elementos de esa fila.
(define (obtener-fila mat f cols)
  (for/list ([c (in-range cols)]) (extraer-mat f c mat)))

;; Funcion: obtener-col
;; Descripción: Extrae una columna específica simulando una transposición local.
;; Entradas: mat (matriz actual), c (índice de la columna), fils (cantidad de filas)
;; Salidas: Una lista plana con los elementos de esa columna.
(define (obtener-col mat c fils)
  (for/list ([f (in-range fils)]) (extraer-mat f c mat)))

;; Funcion: calcular-destinos-linea
;; Descripción: Simula matemáticamente las colisiones de una sola línea (fila o columna).
;; Calcula exactamente en qué índice va a terminar cada casilla para que la animación
;; gráfica sepa a qué distancia en píxeles debe deslizar cada cuadro.
;; Entradas: linea (lista de números), reversa? (booleano para invertir el cálculo si va a la derecha/abajo)
;; Salidas: Un vector con los índices de destino correspondientes a cada elemento original.
(define (calcular-destinos-linea linea reversa?)
  (let* ((len (length linea))
         (linea-trabajo (if reversa? (reverse linea) linea))
         (destinos (make-vector len 0))
         (target 0)
         (ultimo-val -1)
         (ultimo-target -1))
    
    (for ([i (in-range len)])
      (let ((val (list-ref linea-trabajo i)))
        (unless (zero? val)
          (if (= val ultimo-val)
              (begin
                (vector-set! destinos i ultimo-target)
                (set! ultimo-val -1)) 
              (begin
                (vector-set! destinos i target)
                (set! ultimo-val val)
                (set! ultimo-target target)
                (set! target (+ target 1)))))))
    
    (if reversa?
        (let ((destinos-reales (make-vector len 0)))
          (for ([i (in-range len)])
            (let ((val (list-ref linea-trabajo i)))
              (unless (zero? val)
                (vector-set! destinos-reales (- len 1 i) (- len 1 (vector-ref destinos i))))))
          destinos-reales)
        destinos)))

;; Funcion: generar-matriz-destinos
;; Descripción: Orquesta el cálculo de destinos para todo el tablero. Evalúa la dirección
;; presionada por el usuario y genera una "matriz espejo" donde cada celda contiene 
;; la coordenada final hacia donde debe patinar la ficha en la pantalla.
;; Entradas: matriz (tablero actual), dir (símbolo de dirección: 'up, 'down, 'left, 'right), fils, cols.
;; Salidas: Un vector de vectores (matriz optimizada) con los índices finales.
(define (generar-matriz-destinos matriz dir fils cols)
  (let ((m-dest (make-vector fils)))
    (for ([f (in-range fils)]) (vector-set! m-dest f (make-vector cols 0)))
    (cond
      ((eq? dir 'left)
       (for ([f (in-range fils)])
         (let ((dest (calcular-destinos-linea (obtener-fila matriz f cols) #f)))
           (for ([c (in-range cols)]) (vector-set! (vector-ref m-dest f) c (vector-ref dest c))))))
      ((eq? dir 'right)
       (for ([f (in-range fils)])
         (let ((dest (calcular-destinos-linea (obtener-fila matriz f cols) #t)))
           (for ([c (in-range cols)]) (vector-set! (vector-ref m-dest f) c (vector-ref dest c))))))
      ((eq? dir 'up)
       (for ([c (in-range cols)])
         (let ((dest (calcular-destinos-linea (obtener-col matriz c fils) #f)))
           (for ([f (in-range fils)]) (vector-set! (vector-ref m-dest f) c (vector-ref dest f))))))
      ((eq? dir 'down)
       (for ([c (in-range cols)])
         (let ((dest (calcular-destinos-linea (obtener-col matriz c fils) #t)))
           (for ([f (in-range fils)]) (vector-set! (vector-ref m-dest f) c (vector-ref dest f)))))))
    m-dest))

;; ==========================================
;; COLORES ESTILO 2048
;; ==========================================

;; Funcion: obtener-color-fondo
;; Descripción: Mapea un valor numérico a un objeto de color (RGB) para rellenar el fondo de la casilla.
;; Entradas: num (valor entero de la ficha)
;; Salidas: Un objeto `color%` de la librería racket/gui.
(define (obtener-color-fondo num)
  (cond ((= num 0) (make-object color% 205 193 180))
        ((= num 2) (make-object color% 238 228 218))
        ((= num 4) (make-object color% 237 224 200))
        ((= num 8) (make-object color% 242 177 121))
        ((= num 16) (make-object color% 245 149 99))
        ((= num 32) (make-object color% 246 124 95))
        ((= num 64) (make-object color% 246 94 59))
        ((= num 128) (make-object color% 237 207 114))
        ((= num 256) (make-object color% 237 204 97))
        ((= num 512) (make-object color% 237 200 80))
        ((= num 1024) (make-object color% 237 197 63))
        ((= num 2048) (make-object color% 237 194 46))
        (else (make-object color% 60 58 50))))

;; Funcion: obtener-color-texto
;; Descripción: Decide el color de la fuente para asegurar contraste. Letra oscura para
;; números bajos y letra blanca para números altos.
;; Entradas: num (valor entero de la ficha)
;; Salidas: Un objeto `color%` oscuro o claro.
(define (obtener-color-texto num)
  (if (<= num 4)
      (make-object color% 119 110 101)
      (make-object color% 249 246 242)))

;; ==========================================
;; VENTANA PRINCIPAL DEL JUEGO Y COMPONENTES
;; ==========================================

;; Componente: ventana-juego
;; Marco (frame) principal de la aplicación donde ocurre la jugabilidad.
(define ventana-juego (new frame% [label "2048 - Jugando"] [width 500] [height 600]))

;; Componente: panel-superior y btn-opciones
;; Contenedor horizontal que aloja el botón para acceder al menú de configuración en medio del juego.
(define panel-superior (new horizontal-panel% 
                            [parent ventana-juego]
                            [stretchable-height #f]
                            [alignment '(center center)]
                            [spacing 10]))

(define btn-opciones 
  (new button% [parent panel-superior] [label "⚙️ Opciones / Reiniciar Juego"]
       [callback (lambda (boton evento) (mostrar-dialogo-opciones))]))

;; Componente: timer-animacion
;; Descripción: Es el motor que da vida al juego. Se ejecuta cada 15ms cuando se detecta
;; un movimiento. Aumenta los frames, obliga al lienzo a redibujarse creando el efecto visual
;; de deslizamiento, y al terminar, inserta la nueva ficha y evalúa si se ganó o se perdió.
(define timer-animacion
  (new timer% [notify-callback
               (lambda ()
                 (set! anim-frames (+ anim-frames 1))
                 (if (>= anim-frames MAX-FRAMES)
                     (begin
                       (send timer-animacion stop)
                       (set! tablero (insertar-una-segura tablero-futuro columnas))
                       (set! animando? #f)
                       (send lienzo refresh)
                       
                       (cond ((and (ganaste? tablero) (eq? estado-juego 'jugando))
                              (set! estado-juego 'ganaste)
                              (mostrar-victoria))
                             ((sin-movimientos? tablero)
                              (set! estado-juego 'perdiste)
                              (mostrar-derrota))))
                     (send lienzo refresh)))]))

;; Componente: mi-canvas%
;; Descripción: Es una clase hija de canvas% que sobrescribe el evento del teclado (`on-char`).
;; Aquí se capturan las flechas direccionales, se calcula matemáticamente el tablero futuro,
;; y si el movimiento es válido, se activa el motor de animación temporal.
(define mi-canvas%
  (class canvas%
    (super-new)
    (define/override (on-char evento)
      (when (and (or (eq? estado-juego 'jugando) (eq? estado-juego 'continuando)) (not animando?))
        (let* ((tecla (send evento get-key-code))
               (tb-base (cond
                          ((eq? tecla 'up)    (mover-arriba-base tablero columnas))
                          ((eq? tecla 'down)  (mover-abajo-base tablero columnas))
                          ((eq? tecla 'left)  (mover-izq-base tablero columnas))
                          ((eq? tecla 'right) (mover-der-base tablero columnas))
                          (else tablero))))
          
          (unless (equal? tablero tb-base)
            (set! tablero-viejo tablero)
            (set! tablero-futuro tb-base)
            (set! animando? #t)
            (set! anim-frames 0)
            (set! direccion-anim tecla)
            (set! matriz-destinos (generar-matriz-destinos tablero-viejo tecla filas columnas))
            (send timer-animacion start 15)))))))

;; Componente: lienzo
;; Descripción: La superficie gráfica donde se dibuja el fondo y se pintan los rectángulos 
;; (fichas). Utiliza interpolación matemática con la variable "progreso" para mover 
;; los píxeles gradualmente simulando un deslizamiento suave en la pantalla.
(define lienzo 
  (new mi-canvas% [parent ventana-juego]
       [paint-callback
        (lambda (canvas dc)
          (let* ((ancho-canvas (send canvas get-width))
                 (alto-canvas (send canvas get-height))
                 (tam-casilla (min (/ ancho-canvas columnas) (/ alto-canvas filas)))
                 (margen-x (/ (- ancho-canvas (* tam-casilla columnas)) 2))
                 (margen-y (/ (- alto-canvas (* tam-casilla filas)) 2)))
            
            (send dc set-brush (make-object color% 187 173 160) 'solid)
            (send dc set-pen "transparent" 1 'transparent)
            (send dc draw-rectangle margen-x margen-y (* tam-casilla columnas) (* tam-casilla filas))
            
            (for* ([f (in-range filas)] [c (in-range columnas)])
              (let ((pos-x (+ margen-x (* c tam-casilla)))
                    (pos-y (+ margen-y (* f tam-casilla))))
                (send dc set-brush (obtener-color-fondo 0) 'solid)
                (send dc set-pen (make-object color% 187 173 160) 5 'solid)
                (send dc draw-rounded-rectangle pos-x pos-y tam-casilla tam-casilla 5)))
            
            (let ((matriz-dibujar (if animando? tablero-viejo tablero))
                  (progreso (if animando? (/ anim-frames (exact->inexact MAX-FRAMES)) 0)))
              
              (for* ([f (in-range filas)] [c (in-range columnas)])
                (let ((valor (extraer-mat f c matriz-dibujar)))
                  (unless (zero? valor)
                    (let* ((target-f (if (and animando? (or (eq? direccion-anim 'up) (eq? direccion-anim 'down)))
                                         (vector-ref (vector-ref matriz-destinos f) c) f))
                           (target-c (if (and animando? (or (eq? direccion-anim 'left) (eq? direccion-anim 'right)))
                                         (vector-ref (vector-ref matriz-destinos f) c) c))
                           
                           (dist-x (- target-c c))
                           (dist-y (- target-f f))
                           
                           (offset-x (* dist-x tam-casilla progreso))
                           (offset-y (* dist-y tam-casilla progreso))
                           
                           (pos-x (+ margen-x (* c tam-casilla) offset-x))
                           (pos-y (+ margen-y (* f tam-casilla) offset-y)))
                      
                      (send dc set-brush (obtener-color-fondo valor) 'solid)
                      (send dc set-pen (make-object color% 187 173 160) 5 'solid)
                      (send dc draw-rounded-rectangle pos-x pos-y tam-casilla tam-casilla 5)
                      
                      (let ((str-val (number->string valor)))
                        (send dc set-font (make-font #:size (if (> valor 512) 16 24) #:weight 'bold))
                        (send dc set-text-foreground (obtener-color-texto valor))
                        (let-values (((tw th td ta) (send dc get-text-extent str-val)))
                          (send dc draw-text str-val 
                                (+ pos-x (/ (- tam-casilla tw) 2)) 
                                (+ pos-y (/ (- tam-casilla th) 2))))))))))))]))

;; ==========================================
;; CUADROS DE DIÁLOGO Y OPCIONES
;; ==========================================

;; Funcion: mostrar-victoria
;; Detiene el flujo e invoca una alerta del sistema para felicitar al jugador al alcanzar 2048.
(define (mostrar-victoria)
  (let ((respuesta (message-box/custom "¡Ganaste!" 
                                       "¡Felicidades, has llegado a la casilla 2048!\n¿Qué deseas hacer ahora?"
                                       "Continuar Jugando" "Reiniciar Juego" #f ventana-juego)))
    (if (eq? respuesta 1)
        (begin (set! estado-juego 'continuando) (send lienzo focus))
        (mostrar-dialogo-opciones))))

;; Funcion: mostrar-derrota
;; Detiene el flujo informando al jugador que ya no quedan combinaciones matemáticas posibles.
(define (mostrar-derrota)
  (let ((respuesta (message-box/custom "Derrota" "¡Te has quedado sin movimientos posibles!"
                                       "Reiniciar Juego" #f #f ventana-juego)))
    (mostrar-dialogo-opciones)))

;; Componente: dialogo-opciones
;; Ventana emergente (modal) que permite al usuario modificar el tamaño de las filas y columnas a mitad de partida.
(define dialogo-opciones (new dialog% [label "Opciones de Juego"] [parent ventana-juego] [width 250]))
(define panel-opc (new horizontal-panel% [parent dialogo-opciones] [alignment '(center center)]))

(define txt-filas-opc (new text-field% [label "Filas:"] [parent panel-opc]))
(define txt-cols-opc (new text-field% [label "Cols:"] [parent panel-opc]))

(define btn-aplicar-opc 
  (new button% [parent dialogo-opciones] [label "Aplicar y Reiniciar"]
       [callback (lambda (boton evento)
                   ;; Validación al aplicar opciones
                   (let ((f-val (validar-entrada (send txt-filas-opc get-value)))
                         (c-val (validar-entrada (send txt-cols-opc get-value))))
                     (if (and f-val c-val)
                         (begin
                           (send timer-animacion stop)
                           (set! animando? #f)
                           (set! filas f-val)
                           (set! columnas c-val)
                           (set! tablero (insertar-dos-casillas (crear-matriz filas columnas) columnas))
                           (set! estado-juego 'jugando)
                           (send dialogo-opciones show #f)
                           (send lienzo focus)
                           (send lienzo refresh))
                         ;; Mensaje de error actualizado si la validación falla
                         (message-box "Error de Entrada" "Por favor ingrese números enteros entre 4 y 10." #f '(stop ok)))))]))

;; Funcion: mostrar-dialogo-opciones
;; Pre-llena las cajas de texto con los valores actuales antes de mostrar la ventana modal.
(define (mostrar-dialogo-opciones)
  (send txt-filas-opc set-value (number->string filas))
  (send txt-cols-opc set-value (number->string columnas))
  (send dialogo-opciones show #t))

;; ==========================================
;; VENTANA DE BIENVENIDA 
;; ==========================================

;; Componente: ventana-inicio
;; El primer marco visible al arrancar el programa. Recibe los parámetros iniciales del juego.
(define ventana-inicio (new frame% [label "2048"] [width 400] [height 220]))

(define panel-bienvenida (new vertical-panel% [parent ventana-inicio] [alignment '(center center)] [spacing 15]))
(define msg-titulo (new message% [parent panel-bienvenida] [label "¡BIENVENIDO A 2048!"] [font (make-font #:size 16 #:weight 'bold)]))

(define msg-instrucciones (new message% 
                               [parent panel-bienvenida] 
                               [label "Para iniciar la partida digite las filas y columnas (mín. 4, máx. 10):"]))

(define panel-inputs-inicio (new horizontal-panel% [parent panel-bienvenida] [alignment '(center center)]))
(define txt-filas-inicio (new text-field% [label "Filas:"] [parent panel-inputs-inicio] [init-value "4"]))
(define txt-cols-inicio (new text-field% [label "Columnas:"] [parent panel-inputs-inicio] [init-value "4"]))

;; Componente: btn-iniciar
;; Valida la entrada inicial, genera la matriz 2D llamando al motor lógico, oculta 
;; la bienvenida y muestra el lienzo interactivo definitivo.
(define btn-iniciar 
  (new button% [parent panel-bienvenida] [label "▶ Iniciar Juego"]
       [callback (lambda (boton evento)
                   ;; Validación estricta al iniciar partida
                   (let ((f-val (validar-entrada (send txt-filas-inicio get-value)))
                         (c-val (validar-entrada (send txt-cols-inicio get-value))))
                     (if (and f-val c-val)
                         (begin
                           (set! filas f-val)
                           (set! columnas c-val)
                           (set! tablero (insertar-dos-casillas (crear-matriz filas columnas) columnas))
                           (set! estado-juego 'jugando)
                           
                           (send ventana-inicio show #f)
                           (send ventana-juego show #t)
                           (send lienzo focus))
                         ;; Mensaje de error actualizado
                         (message-box "Error de Entrada" "Por favor ingrese números enteros entre 4 y 10." #f '(stop ok)))))]))

;; Muestra únicamente el menú de bienvenida para arrancar el flujo de la aplicación.
(send ventana-inicio show #t)