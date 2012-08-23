;;;; Simulador de Poker
;;;; Marco Herrero <hielo.helado@gmail.com>
;;;; GPLv3

;;; Pretende ser un simulador de poker, construido modularmente, de manera que acepte diversas variedades del juego (Hold'em, Chicago, etc) mediante distintos modulos.

;;; Modulos: Core, Apuestas, Hold'em.

;;; CORE:
;;;    Identifica lo basico de un juego de cartas, una carta, una baraja, una mano, jugadores, jugadas, ganador. Se puede "jugar" solo con este modulo, pero es un repartir y ver quien gana.
;;; TODO:
;;;  Sistema de turnos (repartiendo una carta por turno).

(defvar *jugadores* NIL)
(defvar *baraja* NIL)

;; Posiblemente tenga que anadir nombre aqui.
(defun jugador (id mano)
  (list id mano))

;; Valor de 1 a 13 (11, 12 y 13 J, Q y K, respectivamente). 0-3 palos. Si esta mal formada, devuelve NIL.
(defun carta (valor palo)
  (if (or (< valor 1) (> valor 13) (< palo 0) (> palo 3))
      NIL
      (list valor palo)))

;; Mano de 5 cartas, si la mano esta mal formada, devuelve NIL
(defun mano (a b c d e)
  (if (or (equal a nil) (equal b nil) (equal c nil) (equal d nil) (equal e nil))
      NIL
      (list a b c d e)))

;; Solo crea la baraja francesa, 52 cartas, ordenada.
(defun baraja ()
  (setf *baraja* NIL)
  (loop for palo from 0 to 3 do
       (loop for valor from 1 to 13 do
	    (setf *baraja* (cons (carta valor palo) *baraja*))
       )
  )
  *baraja*)

;; Mezcla la baraja.
(defun shuffle ()
  ; Do nothing
)

;; Muestra la baraja en un formato bueno
(defun print-baraja ())

(defun comprueba-jugada (mano)
  )

;  manos = (list mano1 mano2 ...)
(defun comprueba-ganador (manos) 
)

(defun juega (jugadores)
  ; Crea los jugadores y les asigna manos vacias.
  (loop for jugador from 1 to jugadores do
       (
       