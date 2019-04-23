import Text.Show.Functions

--3.1.1Modelado del Auto
type Truco = Auto -> Auto

data Auto = Auto {
    nombre :: String,
    nivelDeNafta :: Int,
    velocidad :: Int,
    suEnamorade :: String,
    truco :: Truco
} deriving Show

modificarVelocidad :: (Int->Int) -> Auto -> Auto
modificarVelocidad modificacion unAuto  = unAuto {velocidad = (modificacion.velocidad) unAuto}

--3.1.2Trucos-------------------------------------------------------------------------------------------------------------------
deReversaRocha :: Auto -> Auto
deReversaRocha unAuto = unAuto {nivelDeNafta = ((+ 200).nivelDeNafta) unAuto}

impresionar :: Auto -> Auto
impresionar = modificarVelocidad (*2)

nitro :: Auto -> Auto
nitro = modificarVelocidad (+15)

fingirAmor :: String -> Auto -> Auto
fingirAmor enamorade unAuto = unAuto {suEnamorade = enamorade}

--3.1.3Modelado de Autos--------------------------------------------------------------------------------------------------------
rochaMcQueen :: Auto
rochaMcQueen = Auto "RochaMcQueen" 300 0 "Ronco" (deReversaRocha)

biankerr :: Auto
biankerr = Auto "Biankerr" 500 20 "Tinch" (impresionar)

gushtav :: Auto
gushtav = Auto "Gushtav" 200 130 "PetiLaLinda" (nitro)

rodra :: Auto
rodra = Auto "Rodra" 0 50 "Taisa" (fingirAmor "Petra")

--3.2Incrementar Velocidad------------------------------------------------------------------------------------------------------
esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"

contarVocales :: String -> Int
contarVocales = length.(filter esVocal)

potenciarSegun :: String -> Int
potenciarSegun enamorade
 |contarVocales enamorade <= 2 = 15
 |contarVocales enamorade <= 4 = 20
 |otherwise = 30

sumarPotencia :: Auto -> (Int -> Int)
sumarPotencia = (+).potenciarSegun.suEnamorade

incrementarVelocidad :: Auto -> Auto
incrementarVelocidad unAuto = modificarVelocidad (sumarPotencia unAuto) unAuto

--3.3 Puede Realizar Truco-------------------------------------------------------------------------------------------------------
quedaNafta :: Auto -> Bool
quedaNafta unAuto = ((> 0).nivelDeNafta) unAuto

tieneVelocidadBaja :: Auto -> Bool
tieneVelocidadBaja unAuto = ((< 100).velocidad) unAuto

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco unAuto = ((&& quedaNafta unAuto).tieneVelocidadBaja) unAuto

--3.4 Nuevos Trucos--------------------------------------------------------------------------------------------------------------
comboLoco :: Auto -> Auto
comboLoco unAuto = (nitro.deReversaRocha) unAuto

queTrucazo :: String -> Auto -> Auto
queTrucazo unAuto = incrementarVelocidad.(fingirAmor unAuto)

explotarNafta :: Auto -> Int
explotarNafta unAuto = ((*10).nivelDeNafta) unAuto

turbo :: Auto -> Auto
turbo unAuto = unAuto {nivelDeNafta = 0, velocidad = ((+ explotarNafta unAuto).velocidad) unAuto}

--- Sujeto a revision.

realizarTruco :: Auto -> Auto
realizarTruco unAuto = (truco unAuto) unAuto

cambiarTruco :: Auto -> Truco -> Auto
cambiarTruco unAuto nuevoTruco = unAuto {truco = nuevoTruco}