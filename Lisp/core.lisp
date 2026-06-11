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
            (list color-actual 'Accion-por-defecto) ;;se entiende la idea que es por defecto de no cumplir ninguna
        )
    )
)

;; ======================================================== 
;; FUNCION: timer
;; NATURALEZA:  Pura (no devuelve nada en pantalla)
;; ESTRATEGIA: Seleccion condicional
;; IMPACTO: No destructiva
;; ======================================================== 
(defun timer (unix-time)    ;;unix-time es el tiempo epoch
    (cond
        ((<= (mod unix-time 216) 89) 'rojo)
        ((>= (mod unix-time 216) 210) 'amarillo)    ;;creo que es la manera maeficiente
        (t                                          
            'verde
        )
    )
)
;;del 0 al 89 segundos va a ser rojo, de 90 al 209 va a ser verde y del 210 al 215 va a ser amarillo
;;la intencion es dejar en caso de que no sea ni menor o igual a 89, ni mayor o igual a 210
;;asi simplemente queda ese intervalo de segundos en medio para el verde, quedando en el ultimo cond