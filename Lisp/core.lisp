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