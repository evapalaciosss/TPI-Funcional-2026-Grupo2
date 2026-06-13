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
        ((and (= color-actual 'en-rojo) (= cambiar-a 'verde)) 
             (list color-actual 'Cambiar-a-verde)      
        )
        ((and (= color-actual 'en-verde) (= cambiar-a 'amarillo))
            (list color-actual 'Cambiar-a-amarilo)
        )
        ((and (= color-actual 'en-amarillo) (= cambiar-a 'rojo))
            (list color-actual 'Cambiar-a-rojo)
        )
        :else      
            (list color-actual 'Accion-por-defecto)
        )
    )

;; Casos Prubea                                  Salida
;; (transicion 'en-rojo 'verde)                  (en-rojo "cambiar-a-verde")
;; (transicion 'en-verde 'amarillo)              (en-verde "cambiar-a-amarillo")
;; (transicion 'en-amarillo 'rojo)               (en-amarillo "cambiar-a-rojo")

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

(defn timer [unix-time duracion-rojo duracion-amarillo duracion-verde]
  (let [duracion-ciclo (+ duracion-rojo duracion-amarillo duracion-verde)
        posicion       (mod unix-time duracion-ciclo)]
    (cond
      (< posicion duracion-rojo)
      'rojo

      (< posicion (+ duracion-rojo duracion-amarillo))
      'amarillo

      :else
      'verde)))
    
;; Casos Prueba                      Salida
;; (timer 0   90 6 120)              rojo
;; (timer 90  90 6 120)              amarillo
;; (timer 96  90 6 120)              verde
;; (timer 216 90 6 120)              rojo    (nuevo ciclo)

;; Camino Alternativo                Salida
;; (timer 40  30 5 60)               amarillo

;; Caso Error (tiempo invalido)      Salida
;; (timer -10  90 6 120)             verde   (mod negativo, resultado invalido)