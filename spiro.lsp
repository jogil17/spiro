;Jogil Moreno Martínez & Pedro Bustamante Disla // 2023

(defun vermell () (color 255 0 0))
(defun negre () (color 0 0 0))
(defun verd () (color 0 255 0))
(defun blau () (color 0 0 255))

;Almacenamiento de información en las propiedades del átomo simbólico "spiro"
(defun guarda-informacio ()
    (putprop 'spiro '((150 105) 
                       (144 96)) 'grans)
    (putprop 'spiro '(  (84 35 56) 
                        (80 33 53) 
                        (75 31 50) 
                        (72 29 48) 
                        (63 25 42)
                        (60 23 40) 
                        (56 21 37) 
                        (52 19 35) 
                        (48 17 32) 
                        (45 16 30) 
                        (42 14 28)
                        (40 13 27)
                        (32 9 21) 
                        (30 8 20) 
                        (24 5 16) ) 'petits)
    (putprop 'spiro 150 'rgran)                                  
    (putprop 'spiro 50 'rpetit)    
    (putprop 'spiro 3 'punt)
    (putprop 'spiro 0 'inici)
    (putprop 'spiro 1.8 'escala)
    (putprop 'spiro T 'interior)    
    (putprop 'spiro 0 'x)
    (putprop 'spiro 0 'y)
    (putprop 'spiro 0.2 'pas)
    (string "Informacio guardada!")
)

;función que dibuja un círculo en las coordenadas x y, de radio z, y n segmentos
(defun cercle (x y radi n)
    (mou (+ x radi) y)
    (cercle2 x y radi (/ 360 n) 0)
)

;obtención de coordenada x pasado un ángulo por parámetro
(defun obtex (angle)
    (* (- (get 'spiro 'rgran) (get 'spiro 'rpetit)) 
    (sin (radians angle)))
)
;obtención de coordenada y pasado un ángulo por parámetro
(defun obtey (angle)
    (* (- (get 'spiro 'rgran) (get 'spiro 'rpetit)) 
    (cos (radians angle)))
)

;función para pintar de x a y, haciendo una circunferencia
(defun cercle2 (x y radi pas angle)
    (cond 
        ((< angle 360)
            (pinta 
                (+ x (* radi (cos (radians (+ angle pas)))))
                (+ y (* radi (sin (radians (+ angle pas)))))
            )
            (cercle2 x y radi pas (+ angle pas))
        )
    (t t)
    )
)

;método para la obtención de las siguientes coordenadas para pintar el círculo
(defun mou (x y)
    (setq xp (rotax x y (get 'spiro 'inici)))
    (setq yp (rotay x y (get 'spiro 'inici)))

    (move 
        (realpart (round (+ 320 (* (get 'spiro 'escala) xp))))
        (realpart (round (+ 187 (* (get 'spiro 'escala) yp))))
    )
)

;método que pinta hasta la coordenada x y, desde la posición inicio del spiro
(defun pinta (x y)
    (setq xp (rotax x y (get 'spiro 'inici)))
    (setq yp (rotay x y (get 'spiro 'inici)))

    (draw 
        (realpart (round (+ 320 (* (get 'spiro 'escala) xp))))
        (realpart (round (+ 187 (* (get 'spiro 'escala) yp))))
    )
)

;conversión de grados a radianes
(defun radians (graus)
    (/(* graus(* 2 pi))360)
)

;asignación de un valor a la propiedad de spiro, en radigran, pasado por parámetro
(defun radigran (r)
    (putprop 'spiro r 'rgran)
    (cercle (get 'spiro 'x) (get 'spiro 'y) 
    (get 'spiro 'rgran) (get 'spiro 'rgran))
)

;asignación de un valor a la propiedad de spiro, en radipetit, pasado por parámetro
(defun radipetit (r)
    (putprop 'spiro r 'rpetit)
    (cercle (obtex(get 'spiro 'inici)) (obtey(get 'spiro 'inici))
     (get 'spiro 'rpetit) (get 'spiro 'rpetit))
)

;asingar punto
(defun punt (p)
    (putprop 'spiro p 'punt)
)

(defun inici (a)
    (putprop 'spiro a 'inici)
    ; CALCULAR LAS CORDENADAS DEL CENTRO DEL NUEVO ANGULO Y UTILIZAR MOU PARA QUE SE DIBUJE AHI
)

;asignar escala
(defun escala (e)
    (putprop 'spiro e 'escala)
)

;asignar nuevas coordenadas
(defun posicio (a b)
    (putprop 'spiro a 'x)
    (putprop 'spiro b 'y)
)

(defun reduir (m n)
    (list (/ m (gcd m n)) (/ n (gcd m n))) 
)

;función que calsula x
(defun calculaX (a t1)
    ; = (R - r) * cos(r*a/R) + t * cos((1- r/R)*a)
    (+ 
        (* 
            (- (get 'spiro 'rgran) (get 'spiro 'rpetit)) 
            (cos (/ (* (get 'spiro 'rpetit) a) (get 'spiro 'rgran)))
        ) 

        (* t1 
            (cos (* a  (- 1 (/ (get 'spiro 'rpetit) (get 'spiro 'rgran)))))
        )
    )
)

;función que calcula y
(defun calculaY (a t1)
    ; = (R - r) * sin(r*a/R) - t * sin((1- r/R)*a)
    (- 
        (* 
            (- (get 'spiro 'rgran) (get 'spiro 'rpetit)) 
            (sin (/ (* (get 'spiro 'rpetit) a) (get 'spiro 'rgran)))
        ) 
        
        
        (* t1 
           (sin (* a  (- 1 (/ (get 'spiro 'rpetit) (get 'spiro 'rgran)))))
        )
    
    )

)

;función que rota las coordenadas para realizar el spiro
(defun rotax (x y inici)
    ; 𝑥' = 𝑥 ∙ cos 𝛼 + 𝑦 ∙ sin 𝛼
    (+ 
        (* x (cos (radians inici))) 
        (* y (sin (radians inici)))
    )
)

;función que rota las coordenadas para realizar el spiro
(defun rotay (x y inici)
    ; 𝑦' = −𝑥(string "HOLA") ∙ sin 𝛼 + 𝑦 ∙ cos 𝛼
    (+ 
        (* (* x -1) (sin (radians inici)))
        (* y (cos (radians inici)))
    )
)

;Simula el comportamiento de un spiro, y llama a una función u otra dependiendo si es interior o exterior
(defun spirograph (p gran petit t1 inc inici)
    (putprop 'spiro gran 'rgran)
    (putprop 'spiro petit 'rpetit)
    (putprop 'spiro inici 'inici)

    (cond ((get 'spiro 'interior) (interior p gran petit t1 inc inici))
          (t (exterior p gran petit t1 inc inici))  
    )
)


;función que utilizamos en spiro para poder simular el comportamiento dentro de la funcion spirograph,
;claculamos las x e y, y las metemos en las propiedades del spiro, para movernos hacia el comienzo del trazado y
; así llamar a interior2 para comenzar a pintar mientra p > 0
(defun interior (p gran petit t1 inc inici)
    (putprop 'spiro (calculaX p t1) 'x)
    (putprop 'spiro (calculaY p t1) 'y)
    (mou (get 'spiro 'x) (get 'spiro 'y))
    (interior2 (- p inc) gran petit t1 inc inici)
)

;Caso cuando spiro es interior
(defun interior2 (p gran petit t1 inc inici)

    (cond 
        ((> p 0) 
            (putprop 'spiro (calculaX p t1) 'x)
            (putprop 'spiro (calculaY p t1) 'y)
            (pinta (get 'spiro 'x) (get 'spiro 'y))
            (interior2 (- p inc) gran petit t1 inc inici))
        (t nil)
    )
)

;caso en que spiro es exterior 
(defun exterior (p gran petit t1 inc inici)
    (putprop 'spiro (calculaXE p t1) 'x)
    (putprop 'spiro (calculaYE p t1) 'y)
    (mou (get 'spiro 'x) (get 'spiro 'y))
    (exterior2 (- p inc) gran petit t1 inc inici)
)

;mismo caso que cuando es interior pero para exterior
(defun exterior2 (p gran petit t1 inc inici)
    (cond 
        ((> p 0) 
            (putprop 'spiro (calculaXE p t1) 'x)
            (putprop 'spiro (calculaYE p t1) 'y)
            (pinta (get 'spiro 'x) (get 'spiro 'y))
            (exterior2 (- p inc) gran petit t1 inc inici))
        (t nil)
    )
)

;función que calcula la coordenada
(defun calculaXE (a t1)
    ; = (R + r) * cos(r*a/R) - t * cos((1+ r/R)*a)
    (- 
        (* 
            (+ (get 'spiro 'rgran) (get 'spiro 'rpetit)) 
            (cos (/ (* (get 'spiro 'rpetit) a) (get 'spiro 'rgran)))
        ) 

        (* t1 
            (cos (* a  (+ 1 (/ (get 'spiro 'rpetit) (get 'spiro 'rgran)))))
        )
    )
)

;función que calcula la coordenada
(defun calculaYE (a t1)
    ; = (R + r) * sin(r*a/R) - t * sin((1+ r/R)*a)
    (- 
        (* 
            (+ (get 'spiro 'rgran) (get 'spiro 'rpetit)) 
            (sin (/ (* (get 'spiro 'rpetit) a) (get 'spiro 'rgran)))
        ) 
        
        
        (* t1 
           (sin (* a  (+ 1 (/ (get 'spiro 'rpetit) (get 'spiro 'rgran)))))
        )
    
    )

)

;función que calcula la distancia entre los puntos equidistantes 
(defun calcularDistanciaPunto (radipetit npuntos)
    (/ radipetit (+ npuntos 1))
)

;función que calcula el valor t, una vez sabido el valor de 'e', 'p' y 'forats'
(defun calcularT (e p forats)
    (* (- forats p) e)
)

;función que comprueba que el radio es válido, y obtiene los agujeros correspondientes
(defun getForats (radi L)
    (cond 
            ((null L) nil)
            ((= radi (car(car L))) (car (cdr (car L)))) 
            (t (getForats radi (cdr L)))  
    ) 
)

;calcular la P del spiro
(defun calcularPspiro (R inc)
    (/ (* R (* 2 pi)) inc)
)

;simulación del spiro con el número de vueltas necesarias para acabar todo el trazado
(defun spiro (gran petit p inc inici)

    ;obtengo forats
    (setq encontrado nil)
    (setq forats (getForats petit (get 'spiro 'petits)))
    (print forats)
    ;la p que nos pasan es el punto por el que desean pintar debemos mirar si es mayor a 0 y menor a forats
    ;si es asi podemos calcular la T y llamar a spirograph
    (cond 
        (
            forats                                                                      ;si forats existe
            (setq e (calculardistanciapunto petit forats))                              ;calculo e la distancia entre punto y punto
            (spirograph (calcularPspiro (rPrima gran petit) inc) gran petit (calcularT e p forats) inc inici)   ;ejecuto espirograph con la nueva t calculada a partir de e y p
        )
        (t nil)
    )
    ;calcularDistanciaPunto --> e
    ; e --> calcularT --> t
    ; t --> (spirograph p gran petit "t" inc inici)
)



