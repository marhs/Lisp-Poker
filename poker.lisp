;;;; Simulador de Poker
;;;; Marco Herrero <hielo.helado@gmail.com>
;;;; GPLv3

;;; Pretende ser un simulador de poker, construido modularmente, de manera que acepte diversas variedades del juego (Hold'em, Chicago, etc) mediante distintos modulos.

;;; Modulos: Core, Poker, Apuestas, Hold'em.

;;; CORE:
;;;    Identifica lo basico de un juego de cartas, una carta, una baraja, una mano, jugadores. No se puede jugar solo con este modulo, ya que solo reparte, recoge y baraja.
;;; TODO:
;;;  Sistema de turnos (repartiendo una carta por turno).

(defvar *jugadores* (make-hash-table))
(defvar *baraja* NIL)

;; Datos de un jugador. El id es lo unico obligatorio, el dinero empieza en 0.
; Hand = mano, money = dinero para apostar, active = juega esa ronda
(defun jugador (id &key name hand (money 0) (active T))
  (list id name hand money active))

;; Crea un hashtable con num-jugadores, asignandoles su id como clave y los datos de (jugador) como valor.
(defun crea-jugadores (num) 
  (setf *jugadores* (make-hash-table))
  ;; TODO Delete players.
  ; Crea num-jugadores
  (loop for n from 1 to num do
       (setf (gethash n *jugadores*) (jugador n))
       )
  )

;; Metodos auxiliares para consultar y modificar jugadores.
(defun getAt (atributo jug)
  (nth atributo (gethash jug *jugadores*)))
(defun setAt (atributo jug data)
  (setf (nth atributo (gethash jug *jugadores*)) data))

;; Carta. Valor de 1 a 13 (11, 12 y 13 J, Q y K, respectivamente). 0-3 palos. Si esta mal formada, devuelve NIL.
(defun carta (valor palo)
  (if (or (< valor 1) (> valor 13) (< palo 0) (> palo 3))
      NIL
      (list valor palo)))

;; Crea la baraja francesa, 52 cartas, ordenada y elimina toda carta de la mano de todos los jugadores.
(defun baraja ()
  (setf *baraja* NIL)
  (loop for key being the hash-keys of *jugadores* do
       (setf (nth 2 (gethash key *jugadores*)) NIL))
  (loop for palo from 0 to 3 do
       (loop for valor from 1 to 13 do
	    (setf *baraja* (cons (carta valor palo) *baraja*))
       )
  )
  (shuffle)
  *baraja*)
;; TODO
;; Mezcla la baraja.
(defun shuffle ()
  (loop for i below (length *baraja*) do
	(rotatef
	 (elt *baraja* i)
	 (elt *baraja* (random (length *baraja*)))))
  *baraja*
)
;; TODO
;; Muestra la baraja en un formato bueno
(defun print-baraja ())

;; Saca 1 carta de la baraja
(defun saca-carta ()
  (let ((x (first *baraja*)))
    (setf *baraja* (rest *baraja*))
    x))

;; Reparte 1 carta a cada jugador activo
(defun reparte () 
  (loop for key being the hash-keys of *jugadores* do
       (if (nth 4 (gethash key *jugadores*))
	   (setf (nth 2 (gethash key *jugadores*)) (cons (saca-carta) (nth 2 (gethash key *jugadores*)))))) 
)

;; Formato de muestra. TODO Usar unicode para mostrar los simbolos de los palos.
(defun muestra-jugadores () 
  (loop for key being the hash-keys of *jugadores* do
       (format t "Jugador: ~a" (nth 0 (gethash key *jugadores*)) )
       (format t " Dinero: ~a" (nth 3 (gethash key *jugadores*)) )
       (format t " Mano: ~a~%" (nth 2 (gethash key *jugadores*)) )))


;;;; MODULO POKER

;; Devuelve una lista del tipo (x . mano) donde X es el valor de la mano (0-Nada - 9-Royal Flush), y la mano ordenada por importancia de cartas (p.ej: dobles parejas: pareja-alta, pareja-baja, carta)
(defun comprueba-jugada (mano)
  )

;;  Comprueba que jugador tiene la mano mas alta, y en caso de empate, quien tiene su primera carta mas alta (y despues segunda, etc).
;; Puede generar empate.
(defun comprueba-ganador (manos) 
)

;;; Funcion principal para jugar 1 partida. Para testear.
(defun juega (jugadores)
  ; Vacia jugadores antiguos, crea los jugadores y les asigna manos vacias.
  (crea-jugadores num)
  (setf *jugadores* NIL)
  (loop for jug from 1 to jugadores do
       (setf *jugadores* (cons (jugador jug (mano (list NIL))) *jugadores*)))
  ; Reparte cartas (modulo basico, 5 cartas)
  (reparte 5)
)
       