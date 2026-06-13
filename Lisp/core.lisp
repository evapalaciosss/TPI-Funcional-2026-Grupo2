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
;; Estrategia de Control: Recursiva Simple (suma directa de fases)
;; Impacto en Memoria: No Destructiva
;; Clasificación: Función de cálculo de duración de ciclo semafórico
;; ------------------------------------------------------------

;;la logica aqui consta en ingresar los segundos de cada color del semaforo rojo,amarillo y verde, en base a esos datos sumar el ciclo del mismo para saber la duracion del mismo
(defun duracion-ciclo (Seg-rojo Seg-amarillo Seg-verde)
  (duracion-ciclo-aux (+ Seg-rojo Seg-amarillo Seg-verde Seg-rojo))
)

;;Requerimiento 4 b)
;; ------------------------------------------------------------
;; Función: duracion-ciclo-aux
;; Naturaleza: Pura (sin efectos secundarios)
;; Estrategia de Control: Condicional Simple (clasificación de valores)
;; Impacto en Memoria: No Destructiva (devuelve nuevo valor)
;; Clasificación: Función auxiliar de recomendación sobre duración de ciclo semafórico
;; ------------------------------------------------------------

;;total es la suma de segundos del ciclo de semaforo en rojo,amarillo,verde,rojo definida en la funcion duracion-ciclo

(defun duracion-ciclo-aux (total) 
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

(defun crear-ciclos (minutos)
  (floor (/ (* minutos 60) 216))
)

;;Requerimiento 6

;; ===================================================================================================================================
;; FUNCIÓN: informe-distribucion
;; NATURALEZA: Impura (Además de calcular porcentajes, muestra resultados por pantalla mediante format)
;; ESTRATEGIA: Calculo directo (Utiliza funciones y operaciones aritmeticas para distribuir el tiempo entre los estados del semaforo)
;; IMPACTO: No destructiva (No modifica variables externas ni estructuras de datos)
;; PROPOSITO: Calcular y mostrar el porcentaje de tiempo que el semaforo permanece en rojo, verde y amarillo
;; ===================================================================================================================================

(defun informe-distribucion (minutos)
  (let* (
         ;; convertimos los minutos a segundos
         (total-segundos (* minutos 60.0))

         ;; cantidad de ciclos completos
         (ciclos (crear-ciclos minutos))

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
    (format t "Porcentaje Amarillo: ~,2F%~%" porcentaje-amarillo)))

;; Casos de prueba:                    Resultado esperado:
;; (informe-distribucion 1)            Rojo=100.00%, Verde=0.00%, Amarillo=0.00%
;; (informe-distribucion 2)            Rojo=75.00%, Verde=25.00%, Amarillo=0.00%
;; (informe-distribucion 15)           Rojo=44.00%, Verde=53.33%, Amarillo=2.67%
;; (informe-distribucion 30)           Rojo=44.00%, Verde=53.33%, Amarillo=2.67%
;; (informe-distribucion 60)           Rojo=42.50%, Verde=54.83%, Amarillo=2.67%
