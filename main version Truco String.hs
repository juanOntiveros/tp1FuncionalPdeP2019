--3.1.1Modelado del Auto
data Auto = Auto {
    nombre :: String,
    nivelDeNafta :: Int,
    velocidad :: Int,
    suEnamorade :: String,
    truco:: String
}deriving (Show)

--3.1.2Trucos-------------------------------------------------------------------------------------------------------------------
deReversaRocha :: Auto -> Auto
deReversaRocha unAuto = unAuto {nivelDeNafta = ((+200).nivelDeNafta) unAuto}

impresionar :: Auto -> Auto
impresionar unAuto = unAuto {velocidad = ((*2).velocidad) unAuto}

nitro :: Auto -> Auto
nitro unAuto = unAuto {velocidad = ((+15).velocidad) unAuto} --Preguntar--

fingirAmor :: Auto -> String -> Auto
fingirAmor unAuto nuevoAmor = unAuto {suEnamorade = nuevoAmor}

--3.1.3Modelado de Autos--------------------------------------------------------------------------------------------------------
rochaMcQueen = Auto "RochaMcQueen" 300 0 "Ronco" "deReversaRocha"
biankerr = Auto "Biankerr" 500 20 "Tinch" "impresionar"
gushtav = Auto "Gushtav" 200 130 "PetiLaLinda" "nitro"
rodra = Auto "Rodra" 0 50 "Taisa" "fingirAmor"

--3.2Incrementar Velocidad------------------------------------------------------------------------------------------------------
--Contar Vocales
valorDeLaLetra :: Char->Bool
valorDeLaLetra 'a' = True
valorDeLaLetra 'e' = True
valorDeLaLetra 'i' = True
valorDeLaLetra 'o' = True
valorDeLaLetra 'u' = True
valorDeLaLetra _ = False

contarVocales::String->Int
contarVocales = length.(filter valorDeLaLetra)
--Potenciar segun su Enamorade
potenciarSegun :: String->Int
potenciarSegun enamorade
 |contarVocales enamorade <= 2 = 15
 |contarVocales enamorade <= 4 = 20
 |otherwise = 30
sumarPotencia::Auto->(Int->Int)
sumarPotencia  = (+).potenciarSegun.suEnamorade

incrementarVelocidad :: Auto->Auto
incrementarVelocidad unAuto = unAuto {velocidad= ((sumarPotencia unAuto).velocidad) unAuto }

--3.3 Puede Realizar Truco-------------------------------------------------------------------------------------------------------
puedeRealizarTruco::Auto->Bool
puedeRealizarTruco unAuto = (nivelDeNafta unAuto > 0) && (velocidad unAuto <100)

--3.4 Nuevos Trucos--------------------------------------------------------------------------------------------------------------
comboLoco::Auto -> Auto
comboLoco = nitro.deReversaRocha

queTrucazo :: Auto -> String -> Auto
queTrucazo unAuto = incrementarVelocidad.(fingirAmor unAuto)

explotarNafta :: Auto -> Int
explotarNafta unAuto = ((*10).nivelDeNafta) unAuto
turbo :: Auto -> Auto
turbo unAuto = unAuto {nivelDeNafta = 0, velocidad = ((+ explotarNafta unAuto).velocidad) unAuto}
