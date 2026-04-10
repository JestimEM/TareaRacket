#lang racket
(require racket/gui/base)
(require "logica.rkt")

;; ==========================================
;; VARIABLES DE ESTADO GLOBALES Y ANIMACIÓN
;; ==========================================
(define filas 4)
(define columnas 4)
(define tablero '())
(define estado-juego 'jugando)

(define animando? #f)
(define tablero-viejo '())
(define tablero-futuro '())
(define direccion-anim 'left)
(define matriz-destinos #()) 
(define anim-frames 0)
(define MAX-FRAMES 7) 

;; ==========================================
;; FUNCIÓN DE VALIDACIÓN DE ENTRADAS
;; ==========================================
;; Verifica que el texto sea un número entero, >= 4 y <= 10
(define (validar-entrada texto)
  (let ((num (string->number texto)))
    (if (and num (integer? num) (>= num 4) (<= num 10))
        num
        #f)))

;; ==========================================
;; LÓGICA DE CÁLCULO DE DESTINOS INDIVIDUALES
;; ==========================================
(define (obtener-fila mat f cols)
  (for/list ([c (in-range cols)]) (extraer-mat f c mat)))

(define (obtener-col mat c fils)
  (for/list ([f (in-range fils)]) (extraer-mat f c mat)))

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

(define (obtener-color-texto num)
  (if (<= num 4)
      (make-object color% 119 110 101)
      (make-object color% 249 246 242)))

;; ==========================================
;; VENTANA PRINCIPAL DEL JUEGO
;; ==========================================
(define ventana-juego (new frame% [label "2048 - Jugando"] [width 500] [height 600]))

(define panel-superior (new horizontal-panel% 
                            [parent ventana-juego]
                            [stretchable-height #f]
                            [alignment '(center center)]
                            [spacing 10]))

(define btn-opciones 
  (new button% [parent panel-superior] [label "⚙️ Opciones / Reiniciar Juego"]
       [callback (lambda (boton evento) (mostrar-dialogo-opciones))]))

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
(define (mostrar-victoria)
  (let ((respuesta (message-box/custom "¡Ganaste!" 
                                       "¡Felicidades, has llegado a la casilla 2048!\n¿Qué deseas hacer ahora?"
                                       "Continuar Jugando" "Reiniciar Juego" #f ventana-juego)))
    (if (eq? respuesta 1)
        (begin (set! estado-juego 'continuando) (send lienzo focus))
        (mostrar-dialogo-opciones))))

(define (mostrar-derrota)
  (let ((respuesta (message-box/custom "Derrota" "¡Te has quedado sin movimientos posibles!"
                                       "Reiniciar Juego" #f #f ventana-juego)))
    (mostrar-dialogo-opciones)))

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
                         ;; Mensaje de error actualizado
                         (message-box "Error de Entrada" "Por favor ingrese números enteros entre 4 y 10." #f '(stop ok)))))]))

(define (mostrar-dialogo-opciones)
  (send txt-filas-opc set-value (number->string filas))
  (send txt-cols-opc set-value (number->string columnas))
  (send dialogo-opciones show #t))

;; ==========================================
;; VENTANA DE BIENVENIDA 
;; ==========================================
(define ventana-inicio (new frame% [label "2048"] [width 400] [height 220]))

(define panel-bienvenida (new vertical-panel% [parent ventana-inicio] [alignment '(center center)] [spacing 15]))
(define msg-titulo (new message% [parent panel-bienvenida] [label "¡BIENVENIDO A 2048!"] [font (make-font #:size 16 #:weight 'bold)]))

(define msg-instrucciones (new message% 
                               [parent panel-bienvenida] 
                               [label "Para iniciar la partida digite las filas y columnas (mín. 4, máx. 10):"]))

(define panel-inputs-inicio (new horizontal-panel% [parent panel-bienvenida] [alignment '(center center)]))
(define txt-filas-inicio (new text-field% [label "Filas:"] [parent panel-inputs-inicio] [init-value "4"]))
(define txt-cols-inicio (new text-field% [label "Columnas:"] [parent panel-inputs-inicio] [init-value "4"]))

(define btn-iniciar 
  (new button% [parent panel-bienvenida] [label "▶ Iniciar Juego"]
       [callback (lambda (boton evento)
                   ;; Validación al iniciar partida
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

(send ventana-inicio show #t)