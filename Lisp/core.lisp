;;Requerimiento 1 con Iteracion 2 Extencion 1

;; ======================================================== 
;; FUNCION: transicion
;; NATURALEZA: Pura (Devuelve la lista con la transicion de colores realiza) 
;; ESTRATEGIA: Seleccion condicional mediante cond
;; IMPACTO: No destructiva
;; ======================================================== 

(defun transicion (color-actual cambiar-a)
    (cond
        ((and (eq color-actual 'en-rojo) (eq cambiar-a 'verde-intermitente))
            (list color-actual 'Cambiar-a-verde-intermitente)
        )
        ((and (eq color-actual 'en-verde-intermitente) (eq cambiar-a 'verde))
            (list color-actual 'Cambiar-a-verde)
        )
        ((and (eq color-actual 'en-verde) (eq cambiar-a 'amarillo-intermitente))
            (list color-actual 'Cambiar-a-amarillo-intermitente)
        )
        ((and (eq color-actual 'en-amarillo-intermitente) (eq cambiar-a 'amarillo))
            (list color-actual 'Cambiar-a-amarillo)
        )
        ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'rojo-intermitente))
            (list color-actual 'Cambiar-a-rojo-intermitente)
        )
        ((and (eq color-actual 'en-rojo-intermitente) (eq cambiar-a 'rojo))
            (list color-actual 'Cambiar-a-rojo)
        )
        (t
            (list color-actual 'Accion-por-defecto)
        )                                               
                                                        
    )
)



;;Requerimiento 2 con Iteracion 2 Extencion 1

;; ======================================================== 
;; FUNCION: timer
;; NATURALEZA:  Pura (no devuelve nada en pantalla)
;; ESTRATEGIA: Seleccion condicional
;; IMPACTO: No destructiva
;; ======================================================== 
(defun timer (unix-time)   
    (let ((resto (mod unix-time 225)))      ;;coloque un let para evitar la repeticion
        (cond
            ((< resto 90) 'rojo)                         
            ((< resto 93) 'verde-intermitente)         
            ((< resto 213) 'verde)                        
            ((< resto 216) 'amarillo-intermitente)        
            ((< resto 222) 'amarillo)                     
            (t 'rojo-intermitente)                        
        )
    )
)
;;del 0 al 89 segundos va a ser rojo, de 90 al 209 va a ser verde y del 210 al 215 va a ser amarillo
;;la intencion es dejar en caso de que no sea ni menor o igual a 89, ni mayor o igual a 210
;;asi simplemente queda ese intervalo de segundos en medio para el verde, quedando en el ultimo cond

;;Requerimiento 3 con iteracion 2

;; ======================================================== 
;; FUNCION: auditoria
;; NATURALEZA: Impura (escribe en pantalla el cambio de estado) 
;; ESTRATEGIA: Delega a funcion "auditoria-aux"
;; IMPACTO: No destructiva
;; ======================================================== 
(defun auditoria (unix-time)        ;unix-time es el tiempo epoch
    (auditoria-aux unix-time (+ unix-time 600)) ;;son 10 min en segundos
)
;;corregi haciendo que muestre en que momento hubo una transicion

;; ======================================================== 
;; FUNCION: auditoria-aux
;; NATURALEZA: Impura (escribe en pantalla el cambio de estado) 
;; ESTRATEGIA: Recursividad de cola
;; IMPACTO: No destructiva
;; ======================================================== 
(defun auditoria-aux (tiempo limite)    ;;hace la recursividad hasta el limite de 10 min
    (cond 
        ((> tiempo limite) NIL)
        (t
           (let ((resto (mod tiempo 225)))
                (cond
                    ((= resto 0)                  ;;si coindice con el tiempo pasado significa que hubo un cambio de una transicion 
                       (format t "Tiempo ~A: La luz a cambiado de amarillo-intermitente a rojo~%" tiempo)   
                    )
                    ((= resto 90)
                       (format t "Tiempo ~A: La luz a cambiado de rojo a verde-intermitente~%" tiempo)
                    )
                    ((= resto 93)
                       (format t "Tiempo ~A: La luz a cambiado de verde-intermitente a verde~%" tiempo)
                   )
                    ((= resto 213) 
                        (format t "Tiempo ~A: La luz a cambiado de verde a amarillo-intermitente~%" tiempo)
                   )
                    ((= resto 216) 
                        (format t "Tiempo ~A: La luz a cambiado de amarillo-intermitente a amarillo~%" tiempo)
                   )
                    ((= resto 222) 
                        (format t "Tiempo ~A: La luz a cambiado de amarillo a rojo-intermitente ~%" tiempo)
                   )
                )
            )
            (auditoria-aux (+ tiempo 1) limite)     ;;va sumando de a 1 segundo
       )
    )
)

;;Requerimiento 4 a) con iteracion 2
;; ------------------------------------------------------------
;; Función: duracion-ciclo
;; Naturaleza: Pura (sin efectos secundarios)
;; Estrategia de Control: Aritmética simple (suma directa de fases)
;; Impacto en Memoria: No Destructiva
;; Clasificación: Función de cálculo de duración de ciclo semafórico
;; ------------------------------------------------------------

;;la logica aqui consta en ingresar los segundos de cada color del semaforo rojo,amarillo y verde, en base a esos datos sumar el ciclo del mismo para saber la duracion del mismo
(defun duracion-ciclo (Seg-rojo Seg-amarillo Seg-verde Seg-intermitencia)
   (+ Seg-rojo Seg-amarillo Seg-verde (* 3 Seg-intermitencia))
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


;;Requerimiento N°5 con Iteracion 2 Extencion 1

;; =================================================================================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura (Retorna la cantidad de ciclos completos)
;; ESTRATEGIA: Transformacion aritmetica (Convierte minutos a segundos y calcula ciclos completos mediante floor)
;; IMPACTO: No destructiva (No modifica variables ni estructuras)
;; PROPOSITO: Determinar cuantos ciclos completos de 225 segundos pueden realizarse en un intervalo de tiempo dado.
;; =================================================================================================================

(defun ciclos-por-tiempo (minutos)
  (floor (* minutos 60) 225))

;;Requerimiento N°6 con Iteracion 2 Extencion 1

;; ===================================================================================================================================
;; FUNCIÓN: informe-distribucion
;; NATURALEZA: Impura (Ademas de calcular porcentajes, muestra resultados por pantalla mediante format)
;; ESTRATEGIA: Resta acumulativa (Resta los segundos ya asignados al total sobrante para al siguiente estado)
;; IMPACTO: No destructiva (No modifica variables externas ni estructuras de datos)
;; PROPOSITO: Calcular y mostrar los porcentajes, usando el orden: Rojo, Verde-Int, Verde, Amarillo-Int, Amarillo, Rojo-Int
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
             (segundos-sobra (mod (* minutos 60) 225))
             
             ;; el rojo usa 90 segundos del sobrante
             ;; min agarra el valor mas chico, para qu si sobra menos de 90 lo agarre todo, y si sobra mas, que solo agarre 90
             (sobra-rojo (min segundos-sobra 90))
             
             ;; restamos el tiempo usado por el anterior estado para darle sobrante al verde intermitente
             (sobra-verde-inter (min (- segundos-sobra sobra-rojo) 3))
             
             ;; restamos el tiempo usado por los anteriores estados para darle sobrante al verde
             (sobra-verde (min (- segundos-sobra sobra-rojo sobra-verde-inter) 120))
             
             ;; restamos el tiempo usado por los anteriores estados para darle sobrante al amarillo intermitente
             (sobra-amarillo-inter (min (- segundos-sobra sobra-rojo sobra-verde-inter sobra-verde) 3))
             
             ;; restamos el tiempo usado por los anteriores estados para darle sobrante al amarillo
             (sobra-amarillo (min (- segundos-sobra sobra-rojo sobra-verde-inter sobra-verde sobra-amarillo-inter) 6))
             
             ;; restamos el tiempo usado por los anteriores estados para darle sobrante al rojo intermitente
             (sobra-rojo-inter (min (- segundos-sobra sobra-rojo sobra-verde-inter sobra-verde sobra-amarillo-inter sobra-amarillo) 3))

             ;; calculamos los segundos totales de cada estado
             (segundos-rojo (+ (* ciclos 90) sobra-rojo))
             (segundos-verde (+ (* ciclos 120) sobra-verde))
             (segundos-amarillo (+ (* ciclos 6) sobra-amarillo))
             (segundos-verde-inter (+ (* ciclos 3) sobra-verde-inter))
             (segundos-amarillo-inter (+ (* ciclos 3) sobra-amarillo-inter))
             (segundos-rojo-inter (+ (* ciclos 3) sobra-rojo-inter))

             ;; calculamos los porcentajes
             (porcentaje-rojo (* (/ segundos-rojo total-segundos) 100))
             (porcentaje-verde (* (/ segundos-verde total-segundos) 100))
             (porcentaje-amarillo (* (/ segundos-amarillo total-segundos) 100))
             (porcentaje-verde-inter (* (/ segundos-verde-inter total-segundos) 100))
             (porcentaje-amarillo-inter (* (/ segundos-amarillo-inter total-segundos) 100))
             (porcentaje-rojo-inter (* (/ segundos-rojo-inter total-segundos) 100))
            )

        ;; mostramos los porcentajes
        (format t "~%Informe de Distribución Temporal (~A minutos):~%~%" minutos)
        (format t "Porcentaje Rojo: ~,2F%~%" porcentaje-rojo)
        (format t "Porcentaje Verde Intermitente: ~,2F%~%" porcentaje-verde-inter)
        (format t "Porcentaje Verde: ~,2F%~%" porcentaje-verde)
        (format t "Porcentaje Amarillo Intermitente: ~,2F%~%" porcentaje-amarillo-inter)
        (format t "Porcentaje Amarillo: ~,2F%~%" porcentaje-amarillo)
        (format t "Porcentaje Rojo Intermitente: ~,2F%~%" porcentaje-rojo-inter))))

;; ========================================================
;; ITERACION 1 - INTERMITENCIA DE SEGURIDAD
;; ========================================================

;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Seleccion condicional mediante cond
;; IMPACTO: No destructiva
;; ========================================================


;; ======================================================== 
;; FUNCION: timer
;; NATURALEZA:  Pura (no devuelve nada en pantalla)
;; ESTRATEGIA: Seleccion condicional
;; IMPACTO: No destructiva
;; ======================================================== 
(defun timer (unix-time)    ;;unix-time es el tiempo epoch
    (let ((resto (mod unix-time 225)))      ;;coloque un let para evitar la repeticion
       (cond
           ((< resto 90) 'rojo)
           ((< resto 93) 'amarillo-intermitente)
           ((< resto 213) 'verde)
		   ((< resto 216) 'amarillo-intermitente)
           ((< resto 222) 'amarillo)
		   ((< resto 225) 'amarillo-intermitente)
            (t                                          
                'amarillo-intermitente
           )
       )
   )
)

;; ======================================================== 
;; FUNCION: auditoria
;; NATURALEZA: Impura (escribe en pantalla el cambio de estado) 
;; ESTRATEGIA: Seleccion condicional mediante cond
;; IMPACTO: No destructiva
;; ======================================================== 

(defun auditoria (unix-time)        ;unix-time es el tiempo epoch
    (auditoria-aux unix-time (+ unix-time 600)) ;;son 10 min en segundos
)

;;corregi haciendo que muestre en que momento hubo una transicion

;; ======================================================== 
;; FUNCION: auditoria-aux
;; NATURALEZA: Impura (escribe en pantalla el cambio de estado) 
;; ESTRATEGIA: Recursividad simple
;; IMPACTO: No destructiva
;; ======================================================== 

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

        ;; 2 cambios × 3 segundos
        (intermitencia 6)

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

(defun informe (datos)        ;;los datos son distitas salida de la funcion auditoria

    (with-open-file
        (stream
        "informe-ejecucion-semaforo.txt"
        :direction :output)       ;;Se creara en la carpeta desde donde esta ejecutandose el lisp

        (format stream
                "Informe de Ejecucion del Sistema Semaforico~%")

        (format stream
                "=======================================~%")

        (format stream "~A~%" datos)

        (format stream
                "~%--- Fin del Informe ---")
    )
)

;; ========================================================
;;                  Fase 2:
;;            QUICKLISP + LOCAL-TIME
;; ========================================================
;;Antes del inicio se debera poner:

;;(ql:quickload "local-time")

;;para cargar la libreria local-time


;;Requerimiento 3
;; ======================================================== 
;; FUNCION: unix-a-normal
;; NATURALEZA: Pura 
;; ESTRATEGIA: 
;; IMPACTO: No destructiva
;; ======================================================== 
(defun unix-a-normal (unix-time)
  (local-time:format-timestring               ;;convierte en texto legible
   nil
   (local-time:universal-to-timestamp (+ unix-time 2208988800))   ;;Transforma en formato interno de lisp, y lo hace timestamp de local-time
   :format '(:day "/" :month "/" :year " " :hour ":" :min ":" :sec))
)



;; ======================================================== 
;; FUNCION: auditoria
;; NATURALEZA: Impura (escribe en pantalla el cambio de estado) 
;; ESTRATEGIA: Seleccion condicional mediante cond
;; IMPACTO: No destructiva
;; ======================================================== 
(defun auditoria (unix-time)        ;unix-time es el tiempo epoch
    (auditoria-aux unix-time (+ unix-time 600)) ;;son 10 min en segundos
)
;;corregi haciendo que muestre en que momento hubo una transicion


;; ======================================================== 
;; FUNCION: auditoria-aux
;; NATURALEZA: Impura (escribe en pantalla el cambio de estado) 
;; ESTRATEGIA: Recursividad simple
;; IMPACTO: No destructiva
;; ======================================================== 
(defun auditoria-aux (tiempo limite)    ;;hace la recursividad hasta el limite de 10 min
    (cond 
        ((> tiempo limite) NIL)
        (t
            (let ((resto (mod tiempo 216)))
                (cond
                    ((= resto 0)                  ;;si coindice con el tiempo pasado significa que hubo un cambio de una transicion 
                        (format t "Tiempo ~A: La luz a cambiado de amarillo a rojo~%" (unix-a-normal tiempo))   ;;llamando la funcion la hace en tiempo legible
                    )
                    ((= resto 90)
                        (format t "Tiempo ~A: La luz a cambiado de rojo a verde~%" (unix-a-normal tiempo))
                    )
                    ((= resto 210) 
                        (format t "Tiempo ~A: La luz a cambiado de verde a amarillo~%" (unix-a-normal tiempo))
                    )
                )
            )
            (auditoria-aux (+ tiempo 1) limite)     ;;suma de a un segundo.
        )
    )
)



