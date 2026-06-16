;; ============================================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura (Devuelve la lista con la transicion de colores realiza) 
;; ESTRATEGIA: Seleccion condicional mediante cond
;; IMPACTO: No destructiva
;; ----------------------------------------------------------------------------
;; Funcion: Recibe el color actual y el color destino, luego devuelve una lista 
;; con el estado y la acción correspondiente en cada caso. Si la transición no 
;; es válida devuelve accion-por-defecto.
;; ============================================================================

(defn transicion [color-actual cambiar-a] 
    (cond
        (and (= color-actual 'en-rojo) (= cambiar-a 'verde-intermitente)) 
             (list color-actual 'Cambiar-a-verde-intermitente)      
        (and (= color-actual 'en-verde-intermitente) (= cambiar-a 'verde))
            (list color-actual 'Cambiar-a-verde)
        (and (= color-actual 'en-verde) (= cambiar-a 'amarillo-intermitente))
            (list color-actual 'Cambiar-a-amarillo-intermitente)
        (and (= color-actual 'en-amarillo-intermitente) (= cambiar-a 'amarillo)) 
             (list color-actual 'Cambiar-a-amarillo)    
        (and (= color-actual 'en-amarillo) (= cambiar-a 'rojo-intermitente)) 
             (list color-actual 'Cambiar-a-rojo-intermitente)      
        (and (= color-actual 'en-rojo-intermitente) (= cambiar-a 'rojo)) 
             (list color-actual 'Cambiar-a-rojo)      
        :else      
            (list color-actual 'Accion-por-defecto)
        )
    )

;; Casos Prubea                                             Salida
;; (transicion 'en-rojo 'verde-intermitente)                  (en-rojo "cambiar-a-verde-intermitente")
;; (transicion 'en-verde 'amarillo-intermitente)              (en-verde "cambiar-a-amarillo-intermitente")
;; (transicion 'en-amarillo 'rojo-intermitente)               (en-amarillo "cambiar-a-rojo-intermitente")

;; Camino Alternativo - transicion invalida      Salida
;; (transicion 'en-rojo 'amarillo)               (en-rojo accion-por-defecto)
 
;; Caso Error - Variable inexistente             Salida
;; (transicion 'en-azul 'verde)                 (en-azul accion-por-defecto)


===================================================================================================================

;; ====================================================================== 
;; FUNCIÓN: Timer
;; NATURALEZA: Pura (no devuelve nada)
;; ESTRATEGIA: Seleccion condicional
;; IMPACTO: No destructiva
;; ----------------------------------------------------------------------
;; Funcion: Recibe tiempo Unix y devuelve el color activo en ese momento.
;; Las duraciones se pasan como argumentos porque el enunciado
;; prohíbe variables globales mutables.
;;
;; Comentario sobe el uso del mod:
;;   El mod existe igual en ambos lenguajes, Devolviendo el resto de la 
;;   división, esto nos dice en qué segundo del ciclo actual estamos, 
;;   ignorando todos los ciclos anteriores ya completados.
;;
;; Comentario sobre el uso del let:
;;   Usamos let porque necesitamos duracion-ciclo y posicion
;;   en más de un lugar del cond. Sin let, tendríamos que
;;   recalcular mod tres veces, lo cual es innecesario.
;; ======================================================================
(defn timer [unix-time]
  (let [duracion-ciclo 225
        posicion       (mod unix-time duracion-ciclo)]
    (cond
      (< posicion 90)
      'rojo

      (< posicion 93)
      'verde-intermitente

      (< posicion 213)
      'verde

      (< posicion 216)
      'amarillo-intermitente

      (< posicion 222)
      'amarillo

      :else
      'rojo-intermitente)))
    
;; Casos Prueba                      Salida
;; (timer 0 )              rojo
;; (timer 90)              verde-intermitente
;; (timer 96)              verde
;; (timer 221)             amarillo   

;; Caso Error (tiempo invalido)      Salida
;; (timer -10)             verde   (mod negativo, resultado invalido)
