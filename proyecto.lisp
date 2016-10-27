(setq red_prueba '( ((-0.73911077 0.48575234 0.71969724 -0.46698612) (0.50105757 -0.04077381 0.9354189 0.43237752) (0.7361344 -0.25686264 0.9819797 0.580209) (-0.036017597 0.58125323 -0.7166718 -0.36782748) (0.30670696 -0.4492206 -0.3467139 0.91801775)) 
					((-0.45649338 -0.97071046 0.96524775 -0.33714664 0.59356153) (0.928551936 -0.56126482 0.72230923 0.4879372 -0.697185)(-0.9212456 0.7181677 -0.544099 0.6086891 -0.85607886))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Lectura de datos
(defun StringaLista(str)
        (if (not (streamp str))
           (StringaLista (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (StringaLista str))
               nil)))


(defun reading1 ()
  
  (with-open-file (stream "Entrenamiento.txt")
    (loop for line = (read-line stream nil)
          while line
   
          collect (car (StringaLista line)))))

;Todos los ejemplos como string
(setq edatos (reading1 ))

(defun reading2 ()
  
  (with-open-file (stream "Test.txt")
    (loop for line = (read-line stream nil)
          while line
   
          collect (car (StringaLista line)))))

;Todos los ejemplos como string
(setq dprueba (reading2 ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun principal (tipo n era red)
	(cond 	((eq tipo 1)
				(entrenar edatos red n era)
			)
			(t
				(test dprueba red )
			)
	)

)

(defun test (datos red)
	(cond 	((null datos)
				nil	
			)
			(t
				(cons (evaluar red (delete (nth 4 (car datos)) (car datos)) ) (test (cdr datos) red))
			)
	)
)

;Funcion para entrenar una cantidad n de eras
(defun entrenar (ejemplo red n era)
	(cond 	((eq era 0)
				red
			)
			(t
				(entrenar ejemplo (entrenar_era ejemplo red n) n (- era 1))
			)
	)
)
;Funcion para entranar 1 sola era
(defun entrenar_era(ejemplo red n)
	(cond 	((null ejemplo)
				red
			)
			((hayerrores red (car ejemplo))
				(entrenar_era 
					(cdr ejemplo) 
					(ajustar red (car ejemplo) n)
					n
				)
			)
			(t
				(entrenar_era (cdr ejemplo) red n)
			)
	)
)

;Funcion para evaluar si dado un ejemplo de entrenamiento el perceptron funciono bien
(defun hayerrores (red ejemplo)
	(cond 	((eq (nth 4 ejemplo) (evaluar red (delete (nth 4 ejemplo) ejemplo)) )
		    ;(eq (evaluar (delete (nth 4 '(0.2 0.3 0.4 0.5 virgnica)) '(0.2 0.3 0.4 0.5 virginica)) red_prueba) (nth 4 '(0.2 0.3 0.4 0.5 virginic)))
				nil
			)
			(t
				'error
			)
	)
)



;Dadas las 4 entradas da la categoria del lirio, entrenamiento adelante
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluar (red vector)
	(cond 	((eq (encontrar (salidas vector red) (max (nth 0 (salidas vector red)) (nth 1 (salidas vector red)) (nth 2 (salidas vector red))) 0) 0)
				'IRIS-SETOSA
			)
			((eq (encontrar (salidas vector red) (max (nth 0 (salidas vector red)) (nth 1 (salidas vector red)) (nth 2 (salidas vector red))) 0) 1)
				'IRIS-VERSICOLOR
			)
			(t
				'IRIS-VIRGINICA
			)
	)
)

(defun encontrar (lista elemento rst)
  	(cond((null lista) nil)
       	((eq (car lista) elemento) 
       		rst
       	)
       	(t (encontrar  (cdr lista) elemento (+ rst 1)) 
       		) 
  	) 
) 

;Funcion para calcular los valores de activacion de la capa oculta y la capa de salida
(defun salidas (vector red)

	(capa (capa vector (car red) ) (cadr red) )


)

;funcion para calcular los valores de salida de cada capa, donde valor capa en la 
;parte de la red de esa capa y vetor es los valores de entrada para esa capa. 
;Esta funcion sirve para capa oculta y capa de salida.
;La funcion nodo se encarga de calcula el valor acumulado del nodo y se lo paso a 
;la sigmoidal para calular el valor de activacion.
(defun capa (vector valor_capa)
	(cond 	((null valor_capa )
				'()
			)
			(t
				;(capa vector (cdr valor_capa) (append rst (cons (sigmoidal (nodo vector (car valor_capa) 0)) '()) ) )
				(cons (sigmoidal (nodo vector (car valor_capa) 0)) (capa vector (cdr valor_capa))  )
			)
	)
)

;Se calcula el valor acumulado del nodo con sus entradas
(defun nodo ( vector valor_nodo acum)
	(cond 	((null valor_nodo )
				acum
			)
			(t
				(nodo (cdr vector) (cdr valor_nodo) (+ (* (car vector) (car valor_nodo)) acum))
			)
	)
)

;Calculo de la funcion sigmoidal
(defun sigmoidal ( x) 
	(/ 1 (+ 1 (exp (* -1 x)) ))
)

;Ajusta la red, entrenamiento atras
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ajustar (red ejemplo n)
	(delete (nth 4 ejemplo) ejemplo)
	(cons (ajustar_capa (car red) n (delta2 (capa2 ejemplo (car red)) (sumatoria (cadr red) (delta1 (error ejemplo red) (capa2 (capa ejemplo (car red) ) (cadr red) )) ) ) ejemplo) 
		(cons 
		(ajustar_capa (cadr red) n (delta1 (error ejemplo red) (capa2 (capa ejemplo (car red) ) (cadr red) )  ) (capa ejemplo (car red))) '())
	)
)

;Funcion para calcular el error propagado en la capa oculta
 (defun sumatoria (capa delta)
 	(cond 	((equal capa '(nil nil nil) )
 				'()
 			)
 			(t
 				(cons  (suma (mapcar #'car capa) delta) (sumatoria (mapcar #'cdr capa) delta)  )
 			)
 	)
 )

 (defun suma(delta w)
 	(cond 	((null delta )
 				0
 			)
 			(t
 				(+ (* (car delta) (car w)) (suma (cdr delta)(cdr w))  )
 			)
 	)
 )
;Funcion para calcular el delta de la formula para la capa de salida
 (defun delta1 (error capa)
 	(cond 	((null error )
 				'()
 			)
 			(t
 				(cons  (* (car error) (sigmoidal_der (car capa))) (delta1 (cdr error)(cdr capa))  )
 			)
 	)

 )
 ;Funcion para calcular la derivada de la funcion sigmoidal
 (defun sigmoidal_der (x)
  (/ (exp x) (expt (+ 1 (exp x)) 2))

  )
;Funcion para calcular el delta de la formula para la capa oculta
 (defun delta2 (capa sumas)
 	(cond 	((null capa )
 				'()
 			)
 			(t
 				(cons  (* (sigmoidal_der (car capa)) (car sumas)) (delta2 (cdr capa) (cdr sumas) )  )
 			)
 	)

 )


 ;Funcion para calcular los valores acumulados de una capa sin aplicar la funcion de activacion 
 (defun capa2 (vector valor_capa)
 	(cond 	((null valor_capa )
 				'()
 			)
 			(t
 				;(capa vector (cdr valor_capa) (append rst (cons (sigmoidal (nodo vector (car valor_capa) 0)) '()) ) )
 				(cons  (nodo vector (car valor_capa) 0) (capa2 vector (cdr valor_capa))  )
 			)
 	)
 )

 ;Funcion para ajustar una capa, recorre la capa y el valor de lso delta
 (defun ajustar_capa (capa n delta vector)
 	(cond 	((null capa)
 				nil
 			)
 			(t
 				(cons (ajustar_nodo (car capa) (car delta) vector  n) (ajustar_capa (cdr capa) n (cdr delta) vector) )
 			)
 	)
 )

 ;Funcion para ajustar un nodo, se recorre un todas las conexiones a ese nodo y el vector con las salidas sin activar
 (defun ajustar_nodo (nodo di vector n)
 	(cond 	((null nodo)
 				nil
 			)
 			(t
 				(cons (ajustar_conec (car nodo) (car vector) di n) (ajustar_nodo (cdr nodo) di (cdr vector) n) )
 			)
 	)
 )

 ;Funcion para calcular la formula
 (defun ajustar_conec (Wji aj di n)
 	(+ Wji (* aj (* di n)))
 )

;Funcion que calcula el error dado un caso de prueba
 (defun error (ejemplo red)
 	(cond 	((eq (nth 4 ejemplo) 'IRIS-SETOSA)
 				(resta '(1 0 0) (salidas ejemplo red) )
 			)
 			((eq (nth 4 ejemplo) 'IRIS-VERSICOLOR)
 				(resta '(0 1 0) (salidas ejemplo red) )
 			)
 			(t
 				(resta '(0 0 1) (salidas ejemplo red) )
 			)
 	)
 )

 (defun resta (v1 v2)
 	(cond 	((null v1)
 				nil
 			)
 			(t
 				(cons (- (car v1) (car v2)) (resta (cdr v1) (cdr v2) ))
 			)
 	)
 )

