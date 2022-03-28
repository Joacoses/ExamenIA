(deffacts  robot 
    (robot 0 0 liniaPedido 0 0 0 0 0 0 naranjas 0 manzanas 0 caquis 0 uva 0 )
    (PaletNaranjas 1 0)
    (PaletManzanas 2 0)
    (PaletCaquis 3 0)
    (PaletUva 4 0)
    (limite 5 1)
    (pedido 2 3 0 1)
)

;;movimientos
(defrule moverDerecha
    (declare (salience 50))

    ;;donde esta el robot
    ?f1 <- (robot ?x  ?y liniaPedido $?datos naranjas ?N manzanas ?M caquis ?C uva ?V)
    ?f2 <- (limite ?x2 ?y2)
    (test (< ?x ?x2) )
    =>
    ;;datos actualizados
    (assert (robot (+ ?x 1) ?y liniaPedido $?datos naranjas ?N manzanas ?M caquis ?C uva ?V ))
)

(defrule moverIzquierda
    (declare (salience 50))

    ?f1 <- (robot ?x  ?y liniaPedido $?datos naranjas ?N manzanas ?M caquis ?C uva ?V)
    ?f2 <- (limite ?x2 ?y2)

    (not (obstaculo =(- ?x 1) ?y))
    (test (> ?x 0) )
    =>

    (assert (robot (- ?x 1) ?y liniaPedido $?datos naranjas ?N manzanas ?M caquis ?C uva ?V))
)

;;acciones
(defrule cogerPaquete
    (declare (salience 100))

    ?f1 <- (robot ?x  ?y ?p almacen $?datosAlmacen)
    ?f2 <- (paquete ?x2 ?y2)

    (test (= ?x ?x2) )
    (test (= ?y ?y2) )
    (test (eq ?p no))
    =>

    (assert (robot ?x ?y si almacen $?datosAlmacen))
    (printout t "Cogiendo el paquete" crlf)
)

















(defrule dejarPaquete
    (declare (salience 100))
    ?f1 <- (robot ?x  ?y liniaPedido ?x ?y ?n ?m ?c ?u naranjas ?N manzanas ?M caquis ?C uva ?V)
    (test (= ?x ?x2) )
    (test (= ?y ?y2) )
    (test (eq ?p si))
    =>

    (assert (robot ?x  ?y no almacen ?x ?y si))
    ;;paramos
    (halt)
    (printout t "Dejando el paquete" crlf)
)