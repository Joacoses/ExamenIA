(deffacts  robot 
    (robot 0 0 0 liniaPedido 0 0 0 0 0 0 naranjas 0 manzanas 0 caquis 0 uva 0 )
    (PaletNaranjas 1 0)
    (PaletManzanas 2 0)
    (PaletCaquis 3 0)
    (PaletUva 4 0)
    (limite 5 1)
    (pedido 2 3 0 1)
)
;;---------------------------------------------------------------------------------------------------------------------------
;;movimientos
(defrule moverDerecha
    (declare (salience 50))

    ;;donde esta el robot
    ?f1 <- (robot ?x ?y  ?z liniaPedido $?datos naranjas ?N manzanas ?M caquis ?C uva ?V)
    ?f2 <- (limite ?x2 ?y2)
    (test (< ?x ?x2) )
    =>
    ;;datos actualizados
    (assert (robot (+ ?x 1) ?y ?z liniaPedido $?datos naranjas ?N manzanas ?M caquis ?C uva ?V ))
)

(defrule moverIzquierda
    (declare (salience 50))

    ?f1 <- (robot ?x ?y ?z liniaPedido $?datos naranjas ?N manzanas ?M caquis ?C uva ?V)
    ?f2 <- (limite ?x2 ?y2)

    (not (obstaculo =(- ?x 1) ?y))
    (test (> ?x 0) )
    =>

    (assert (robot (- ?x 1) ?y ?z liniaPedido $?datos naranjas ?N manzanas ?M caquis ?C uva ?V))
)

;;---------------------------------------------------------------------------------------------------------------------------
;;acciones
(defrule cogerNaranjas
    (declare (salience 80))

    ?f1 <- (robot ?x ?y ?z liniaPedido $?datos naranjas ?N manzanas ?M caquis ?C uva ?V)
    ?f2 <- (PaletNaranjas ?x2 ?y2)
    ?f3 <- (pedido ?pedidoNaranjas ?pedidoManzanas ?pedidoCaquis ?pedidoUvas)

    (test (< ?N ?pedidoNaranjas))
    (test (= ?x ?x2) )
    (test (= ?y ?y2) )
    (test (< ?z 4))  
    =>

    (assert (robot ?x ?y (+ ?z 1) liniaPedido $?datos naranjas (+ ?N 1) manzanas ?M caquis ?C uva ?V))
    (printout t "Cogiendo el paquete de naranjas" crlf)
)

(defrule cogerManzanas
    (declare (salience 80))

    ?f1 <- (robot ?x ?y ?z liniaPedido $?datos naranjas ?N manzanas ?M caquis ?C uva ?V)
    ?f2 <- (PaletNaranjas ?x2 ?y2)
    ?f3 <- (pedido ?pedidoNaranjas ?pedidoManzanas ?pedidoCaquis ?pedidoUvas)

    (test (< ?N ?pedidoManzanas))
    (test (= ?x ?x2) )
    (test (= ?y ?y2) )
    (test (< ?z 4))
    =>

    (assert (robot ?x ?y (+ ?z 1) liniaPedido $?datos naranjas ?N manzanas (+ ?M 1) caquis ?C uva ?V))
    (printout t "Cogiendo el paquete de manzanas" crlf)
)


(defrule cogerCaquis
    (declare (salience 80))

    ?f1 <- (robot ?x ?y ?z liniaPedido $?datos naranjas ?N manzanas ?M caquis ?C uva ?V)
    ?f2 <- (PaletNaranjas ?x2 ?y2)
    ?f3 <- (pedido ?pedidoNaranjas ?pedidoManzanas ?pedidoCaquis ?pedidoUvas)

    (test (< ?N ?pedidoCaquis))
    (test (= ?x ?x2) )
    (test (= ?y ?y2) )
    (test (< ?z 4))
    =>

    (assert (robot ?x ?y (+ ?z 1) liniaPedido $?datos naranjas ?N manzanas ?M caquis (+ ?C 1) uva ?V))
    (printout t "Cogiendo el paquete de caquis" crlf)
)

(defrule cogerUvas
    (declare (salience 80))

    ?f1 <- (robot ?x ?y ?z liniaPedido $?datos naranjas ?N manzanas ?M caquis ?C uva ?V)
    ?f2 <- (PaletNaranjas ?x2 ?y2)
    ?f3 <- (pedido ?pedidoNaranjas ?pedidoManzanas ?pedidoCaquis ?pedidoUvas)

    (test (< ?N ?pedidoUvas))
    (test (= ?x ?x2) )
    (test (= ?y ?y2) )
    (test (< ?z 4))
    =>

    (assert (robot ?x ?y (+ ?z 1) liniaPedido $?datos naranjas ?N manzanas ?M caquis ?C uva (+ ?V 1)))
    (printout t "Cogiendo el paquete de uvas" crlf)
)


(defrule dejarPaquete
    (declare (salience 100))
    ?f1 <- (robot ?x ?y ?z liniaPedido ?x2 ?y2 ?vNar ?vMan ?vCaq ?vUva naranjas ?N manzanas ?M caquis ?C uva ?V)
    (test (= ?x ?x2) )
    (test (= ?y ?y2) )
    =>

    (assert (robot ?x  ?y 0 liniaPedido ?x2 ?y2 ?N ?M ?C ?V naranjas 0 manzanas 0 caquis 0 uva 0))
    ;;paramos
    (halt)
    (printout t "Dejando el paquete" crlf)
)

;;---------------------------------------------------------------------------------------------------------------------------
(defrule finalizar
    (declare (salience 100))
    ?f1 <- (robot ?x ?y ?z liniaPedido ?x2 ?y2 ?vNar ?vMan ?vCaq ?vUva naranjas ?N manzanas ?M caquis ?C uva ?V)
    ?f2 <- (pedido ?nar ?man ?caq ?uva)
    (test (= ?x ?x2) )
    (test (= ?y ?y2) )
    (test (= ?nar ?vNar) )
    (test (= ?man ?vMan) )
    (test (= ?caq ?vCaq) )
    (test (= ?uva ?vUva) )
    =>

    ;;(assert (robot $?datosRobot liniaPedido $?datosLiniaPedido naranjas ?dato1 manzanas ?dato2 caquis ?dato3 uva ?dato4))
    ;;paramos
    (halt)
    (printout t "Hemos entregado todos los pedidos" crlf)
)

    