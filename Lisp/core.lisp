;;Requerimiento 1

;; ======================================================== 
;; FUNCION: transicion
;; NATURALEZA: Pura (Devuelve la lista con la transicion de colores realiza) 
;; ESTRATEGIA: Seleccion condicional mediante cond
;; IMPACTO: No destructiva
;; ======================================================== 

(defun transicion (color-actual cambiar-a)
    (cond
        ((and (eq color-actual 'en-rojo) (eq cambiar-a 'verde)) ;;cambie varias veces el orden del que hacia el cambio de color
            (list color-actual 'Cambiar-a-verde)
        )
        ((and (eq color-actual 'en-verde) (eq cambiar-a 'amarillo))
            (list color-actual 'Cambiar-a-amarillo)
        )
        ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'rojo))
            (list color-actual 'Cambiar-a-rojo)
        )
        (t
            (list color-actual 'Accion-por-defecto) ;;Es en caso de que no se cumpla la validación de "cambiar-a"
        )                                           ;;Ya que se da por hecho que el "color-actual ingresa sin inconvenientes"
    )                                               ;;Mantiene robuzto al codigo
)

;;Requerimiento 2

;; ======================================================== 
;; FUNCION: timer
;; NATURALEZA:  Pura (no devuelve nada en pantalla)
;; ESTRATEGIA: Seleccion condicional
;; IMPACTO: No destructiva
;; ======================================================== 
(defun timer (unix-time)    ;;unix-time es el tiempo epoch
    (let ((resto (mod unix-time 216)))      ;;coloque un let para evitar la repeticion
        (cond
            ((< resto 90) 'rojo)
            ((< resto 210) 'verde)  
            (t                                          
                'amarillo
            )
        )
    )
)
;;del 0 al 89 segundos va a ser rojo, de 90 al 209 va a ser verde y del 210 al 215 va a ser amarillo
;;del 0 al 89 segundos va a ser rojo, de 90 al 209 va a ser verde y del 210 al 215 va a ser amarillo
;;la intencion es dejar en caso de que no sea ni menor o igual a 89, ni mayor o igual a 210
;;asi simplemente queda ese intervalo de segundos en medio para el verde, quedando en el ultimo cond

;;Requerimiento 3

;; ======================================================== 
;; FUNCION: auditoria
;; NATURALEZA: Impura (escribe en pantalla el cambio de estado) 
;; ESTRATEGIA: Seleccion condicional mediante cond
;; IMPACTO: No destructiva
;; ======================================================== 
(defun auditoria (unix-time)        ;unix-time es el tiempo epoch
    (let ((resto (mod unix-time 216)))
        (cond
            ((= resto 0)                  ;;si coindice con el tiempo pasado significa que hubo un cambio de una transicion 
                (format t "Tiempo ~A: La luz a cambiado de amarillo a rojo" unix-time)   
            )
            ((= resto 90)
                (format t "Tiempo ~A: La luz a cambiado de rojo a verde" unix-time)
            )
            ((= resto 210) 
                (format t "Tiempo ~A: La luz a cambiado de verde a amarillo" unix-time)
            )
        )
    )
)
;;corregi haciendo que muestre en que momento hubo una transicion

;;Requerimiento 4 a)
;; ------------------------------------------------------------
;; Función: duracion-ciclo
;; Naturaleza: Pura (sin efectos secundarios)
;; Estrategia de Control: Aritmética simple (suma directa de fases)
;; Impacto en Memoria: No Destructiva
;; Clasificación: Función de cálculo de duración de ciclo semafórico
;; ------------------------------------------------------------

;;la logica aqui consta en ingresar los segundos de cada color del semaforo rojo,amarillo y verde, en base a esos datos sumar el ciclo del mismo para saber la duracion del mismo
(defun duracion-ciclo (Seg-rojo Seg-amarillo Seg-verde)
   (+ Seg-rojo Seg-amarillo Seg-verde)
)

;;Requerimiento 4 b)
;; ------------------------------------------------------------
;; Función: recomendacion-ciclo
;; Naturaleza: Pura (sin efectos secundarios)
;; Estrategia de Control: Condicional Simple (clasificación de valores)
;; Impacto en Memoria: No Destructiva (devuelve nuevo valor)
;; Clasificación: Función auxiliar de recomendación sobre duración de ciclo semafórico
;; ------------------------------------------------------------

;;total es la suma de segundos del ciclo de semaforo en rojo,amarillo,verde,rojo definida en la funcion duracion-ciclo

(defun recomendacion-ciclo (total) 
  (cond
    ((< total 35) (list total "demasiado corto"))
    ((> total 150) (list total "demasiado largo"))
    (t (list total "optimo"))
	)
)


;;Requerimiento N°5

;; =================================================================================================================
;; FUNCIÓN: crear-ciclos
;; NATURALEZA: Pura (Retorna la cantidad de ciclos completos)
;; ESTRATEGIA: Transformacion aritmetica (Convierte minutos a segundos y calcula ciclos completos mediante floor)
;; IMPACTO: No destructiva (No modifica variables ni estructuras)
;; PROPOSITO: Determinar cuantos ciclos completos de 216 segundos pueden realizarse en un intervalo de tiempo dado.
;; =================================================================================================================

(defun ciclos-por-tiempo (minutos)
  (floor (* minutos 60) 216))

;;Requerimiento N°6

;; ===================================================================================================================================
;; FUNCIÓN: informe-distribucion
;; NATURALEZA: Impura (Además de calcular porcentajes, muestra resultados por pantalla mediante format)
;; ESTRATEGIA: Calculo directo (Utiliza funciones y operaciones aritmeticas para distribuir el tiempo entre los estados del semaforo)
;; IMPACTO: No destructiva (No modifica variables externas ni estructuras de datos)
;; PROPOSITO: Calcular y mostrar el porcentaje de tiempo que el semaforo permanece en rojo, verde y amarillo
;; ===================================================================================================================================

(defun informe-distribucion (minutos)
  ;; hacemos una validacion para evitar errores de calculo
  (if (or (not (numberp minutos)) (<= minutos 0))
      (format t "~%La cantidad de minutos debe ser mayor a 0.~%")
      ;; usamos let* para que se evalue linea por linea y no todo el bloque al mismo tiempo
      (let* (
             ;; convertimos los minutos a segundos
             (total-segundos (* minutos 60.0))

             ;; calculamos la cantidad de ciclos completos usando la funcion del Requerimiento 5
             (ciclos (ciclos-por-tiempo minutos))

             ;; guarda los segundos que sobran despues de los ciclos completos con mod
             (segundos-sobra (mod (* minutos 60) 216))

             ;; el rojo usa 90 segundos del sobrante
             ;; min agarra el valor mas chico, para qu si sobra menos de 90 lo agarre todo, y si sobra mas, que solo agarre 90
             (sobra-rojo (min segundos-sobra 90))

             ;; restamos el tiempo usado por el rojo para darle sobrante al verde
             ;; max agarra el valor mas grande para asegurarnos de que no agarre un numero en negativo en caso de que ya no sobre nada
             (sobra-verde (min (max 0 (- segundos-sobra 90)) 120))

             ;; al sobrante le restamos el tiempo que usaron el rojo y el verde
             (sobra-amarillo (max 0 (- segundos-sobra 210)))

             ;; calculamos los segundos totales de cada color
             (segundos-rojo (+ (* ciclos 90) sobra-rojo))
             (segundos-verde (+ (* ciclos 120) sobra-verde))
             (segundos-amarillo (+ (* ciclos 6) sobra-amarillo))

             ;; calculamos los porcentajes
             (porcentaje-rojo (* (/ segundos-rojo total-segundos) 100))
             (porcentaje-verde (* (/ segundos-verde total-segundos) 100))
             (porcentaje-amarillo (* (/ segundos-amarillo total-segundos) 100)))

       ;; mostramos
       (format t "~%Informe de Distribución Temporal (~A minutos):~%~%" minutos)
       (format t "Porcentaje Rojo: ~,2F%~%" porcentaje-rojo)
       (format t "Porcentaje Verde: ~,2F%~%" porcentaje-verde)
       (format t "Porcentaje Amarillo: ~,2F%~%" porcentaje-amarillo))))

;; ========================================================
;; ITERACION 1 - INTERMITENCIA DE SEGURIDAD
;; ========================================================

;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Seleccion condicional mediante cond
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion (color-actual cambiar-a)

    (cond

        ((and (eq color-actual 'en-rojo)
              (eq cambiar-a 'verde))

            (list color-actual
                  'amarillo-intermitente
                  'cambiar-a-verde)
        )

        ((and (eq color-actual 'en-verde)
              (eq cambiar-a 'amarillo))

            (list color-actual
                  'amarillo-intermitente
                  'cambiar-a-amarillo)
        )

        ((and (eq color-actual 'en-amarillo)
              (eq cambiar-a 'rojo))

            (list color-actual
                  'amarillo-intermitente
                  'cambiar-a-rojo)
        )

        (t
            (list color-actual
                  'accion-por-defecto)
        )
    )
)



;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Calculo directo
;; IMPACTO: No destructiva
;; ========================================================

(defun duracion-ciclo ()

    (let (

        (rojo 90)
        (verde 120)
        (amarillo 6)

        ;; 3 cambios × 3 segundos
        (intermitencia 9)

    )

        (+ rojo verde amarillo intermitencia)
    )
)



;; ========================================================
;; FUNCIÓN: informe-distribucion
;; NATURALEZA: Pura
;; ESTRATEGIA: Calculo porcentual
;; IMPACTO: No destructiva
;; ========================================================

(defun informe-distribucion ()

    (let (

        (total 225)

        (rojo (/ (* 90 100.0) total))

        (verde (/ (* 120 100.0) total))

        (amarillo (/ (* 6 100.0) total))

        (intermitente (/ (* 9 100.0) total))
    )

        (list

            (list 'rojo rojo)

            (list 'verde verde)

            (list 'amarillo amarillo)

            (list 'amarillo-intermitente intermitente)
        )
    )
)





;; ========================================================
;; ITERACION 2 - PERSISTENCIA DE DATOS
;; ========================================================

;; ========================================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura (escribe archivo)
;; ESTRATEGIA: Escritura mediante with-open-file
;; IMPACTO: No destructiva
;; ========================================================

(defun informe (datos)

    (with-open-file
        (stream
        "informe-ejecucion-semaforo.txt"
        :direction :output)

        (format stream
                "Informe de Ejecucion del Sistema Semaforico~%")

        (format stream
                "=======================================~%")

        (format stream "~A~%" datos)

        (format stream
                "~%--- Fin del Informe ---")
    )
)





