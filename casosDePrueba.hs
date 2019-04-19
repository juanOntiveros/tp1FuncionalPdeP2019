-------------------------------------------------3.1-----------------------------------------------------------------------------
--Consultar la nafta de RochaMcQueen luego de realizar su truco
(nivelDeNafta.realizarTruco) rochaMcQueen
--Consultar la velocidad luego de realizar su truco
(velocidad.realizarTruco) biankerr
(velocidad.realizarTruco) gushtav
--Consultar la enamorada de Rodra cuando elige a Petra
(suEnamorade.realizarTruco) rodra

----------------------------------3.2 Consultar la velocidad luego de incrementar su velocidad------------------------------------
(velocidad.incrementarVelocidad) rochaMcQueen
(velocidad.incrementarVelocidad) biankerr
(velocidad.incrementarVelocidad) gushtav
(velocidad.incrementarVelocidad) rodra

----------------------------------3.3 Consultar si pueden usar sus trucos--------------------------------------------------------
puedeRealizarTruco rochaMcQueen
puedeRealizarTruco gushtav
puedeRealizarTruco rodra

----------------------------------3.4 Consultar si pueden usar sus trucos--------------------------------------------------------
--Consultar la nafta de Rocha luego de realizar comboLoco
(nivelDeNafta.realizarTruco.(cambiarTruco rochaMcQueen)) comboLoco
--Consultar la velocidad de Rocha luego de realizar comboLoco
(velocidad.realizarTruco.(cambiarTruco rochaMcQueen)) comboLoco
--Consultar la velocidad de Rodra luego de utilizar queTrucazo cambiando su enamorada a Murcielago
(velocidad.realizarTruco.(cambiarTruco rodra)) (queTrucazo "Murcielago")
--Consultar la velocidad de Gushtav luego de utilizar turbo
(velocidad.realizarTruco.(cambiarTruco gushtav)) turbo
--Consultar la nafta de Gushtav luego de utilizar turbo
(nivelDeNafta.realizarTruco.(cambiarTruco gushtav)) turbo
--Consultar la velocidad de Rodra luego de utilizar turbo
(velocidad.realizarTruco.(cambiarTruco rodra)) turbo
--Consultar la nafta de Rodra luego de utilizar turbo
(nivelDeNafta.realizarTruco.(cambiarTruco rodra)) turbo