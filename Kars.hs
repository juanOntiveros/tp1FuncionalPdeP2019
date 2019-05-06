module Kars where

import Text.Show.Functions

--Parte 1 del TP

--3.1.1Modelado del Auto
type Truco = Auto -> Auto

data Auto = Auto {
    nombre :: String,
    nivelDeNafta :: Float,
    velocidad :: Float,
    suEnamorade :: String,
    truco :: Truco
} deriving (Show)

--3.1.2Trucos-------------------------------------------------------------------------------------------------------------------
deReversaRocha :: Auto -> Auto
deReversaRocha unAuto = unAuto {nivelDeNafta = ((+ (velocidad unAuto/5)).nivelDeNafta) unAuto} // delegar

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

potenciarSegun :: String -> Float
potenciarSegun enamorade
 |contarVocales enamorade <= 2 = 15
 |contarVocales enamorade <= 4 = 20
 |otherwise = 30

sumarPotencia :: Auto -> (Float -> Float)
sumarPotencia = (+).potenciarSegun.suEnamorade

incrementarVelocidad :: Auto -> Auto
incrementarVelocidad unAuto = modificarVelocidad (sumarPotencia unAuto) unAuto

--3.3 Puede Realizar Truco-------------------------------------------------------------------------------------------------------
quedaNafta :: Auto -> Bool
quedaNafta unAuto = ((> 0).nivelDeNafta) unAuto

cumpleLaVelocidadLaCondicion :: (Float -> Bool) -> Auto -> Bool
cumpleLaVelocidadLaCondicion condicion unAuto = (condicion.velocidad) unAuto

cumpleElNivelDeNaftaLaCondicion :: (Float -> Bool) -> Auto -> Bool
cumpleElNivelDeNaftaLaCondicion condicion unAuto = (condicion.nivelDeNafta) unAuto

tieneVelocidadBaja :: Auto -> Bool
tieneVelocidadBaja = cumpleLaVelocidadLaCondicion (< 100)

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco unAuto = ((&& quedaNafta unAuto).tieneVelocidadBaja) unAuto

--3.4 Nuevos Trucos--------------------------------------------------------------------------------------------------------------
comboLoco :: Auto -> Auto
comboLoco unAuto = (nitro.deReversaRocha) unAuto

queTrucazo :: String -> Auto -> Auto
queTrucazo unAuto = incrementarVelocidad.(fingirAmor unAuto)

explotarNafta :: Auto -> (Float -> Float)
explotarNafta unAuto = (+ ((*10).nivelDeNafta) unAuto)

turbo :: Auto -> Auto
turbo unAuto = (quitarTodaLaNafta.(modificarVelocidad (explotarNafta unAuto))) unAuto

--Funciones agregadas------------------------------------------------------------------------------------------------------------
realizarTruco :: Auto -> Auto
realizarTruco unAuto = (truco unAuto) unAuto

cambiarTruco :: Truco -> Auto -> Auto
cambiarTruco nuevoTruco unAuto = unAuto {truco = nuevoTruco}

modificarVelocidad :: (Float -> Float) -> Auto -> Auto
modificarVelocidad modificacion unAuto  = unAuto {velocidad = (modificacion.velocidad) unAuto}

modificarNafta :: (Float -> Float) -> Auto -> Auto
modificarNafta modificacion unAuto = unAuto {nivelDeNafta = (modificacion.nivelDeNafta) unAuto}

quitarTodaLaNafta :: Auto -> Auto
quitarTodaLaNafta unAuto = unAuto {nivelDeNafta = 0}

--Parte 2 del TP

--3.1 Modelado de Carrera
data Carrera = Carrera {
    cantidadDeVueltas :: Int,
    longitudPista :: Float,
    publico :: [String],
    trampa :: Trampa,
    participantes :: [Auto]
} deriving Show

type Trampa = Carrera -> Carrera

potreroFunes = Carrera 3 5.0 ["Ronco", "Tinch", "Dodain"] sacarAlPistero [rochaMcQueen, biankerr, gushtav, rodra]

--3.2 Trampas
aplicarALosParticipantes :: ([Auto]->[Auto]) -> Carrera -> Carrera
aplicarALosParticipantes aplicable unaCarrera = unaCarrera {participantes = (aplicable.participantes) unaCarrera}

sacarAlPistero :: Carrera -> Carrera
sacarAlPistero = aplicarALosParticipantes tail

lluvia :: Carrera -> Carrera
lluvia = aplicarALosParticipantes (map (modificarVelocidad (-(10.0)))) // delegar

inutilidad :: Auto -> Auto
inutilidad = id

neutralizarTrucos :: Carrera -> Carrera
neutralizarTrucos = aplicarALosParticipantes (map (cambiarTruco inutilidad))

pocaReserva :: Carrera -> Carrera
pocaReserva = aplicarALosParticipantes (filter (cumpleElNivelDeNaftaLaCondicion (> 30)))

podio :: Carrera -> Carrera
podio = aplicarALosParticipantes sacarALosPrimerosTres

sacarALosPrimerosTres :: [Auto] -> [Auto]
sacarALosPrimerosTres = take 3

--3.3 Vueltas

calcularNaftaAQuitar :: Float -> Auto -> Float
calcularNaftaAQuitar longitud = ((longitud/10)*).velocidad

modificarNaftaSegunPista :: Float -> Auto -> Auto
modificarNaftaSegunPista longitud unAuto = modificarNafta (+(- calcularNaftaAQuitar longitud unAuto)) unAuto // fijarse los floats

restarNafta :: Carrera -> Carrera
restarNafta unaCarrera = aplicarALosParticipantes (map (modificarNaftaSegunPista (longitudPista unaCarrera))) unaCarrera // delegar

aplicarTrucoRevisandoEnamorados :: [String] -> Auto -> Auto
aplicarTrucoRevisandoEnamorados enamorados unAuto 
    | elem (suEnamorade unAuto) enamorados = realizarTruco unAuto
    | otherwise = unAuto

realizarTrucoDeLosParticipantes :: Carrera -> Carrera
realizarTrucoDeLosParticipantes unaCarrera = aplicarALosParticipantes (map (aplicarTrucoRevisandoEnamorados (publico unaCarrera))) unaCarrera // delegar

aplicarTrampa :: Carrera -> Carrera
aplicarTrampa unaCarrera = (trampa unaCarrera) unaCarrera

cambiarTrampa :: Trampa -> Carrera -> Carrera
cambiarTrampa nuevoTrampa unaCarrera = unaCarrera {trampa = nuevoTrampa}

restarVuelta :: Carrera -> Carrera
restarVuelta unaCarrera = unaCarrera {cantidadDeVueltas = cantidadDeVueltas unaCarrera - 1} 

darVuelta :: Carrera -> Carrera
darVuelta = restarVuelta.aplicarTrampa.realizarTrucoDeLosParticipantes.restarNafta

correrCarrera :: Carrera -> Carrera
correrCarrera unaCarrera = (iterate (darVuelta) unaCarrera) !! cantidadDeVueltas unaCarrera

{- Para mostrar en recursivo.

correrCarrera2 :: Carrera -> Carrera
correrCarrera unaCarrera = darMultiplesVueltas (cantidadDeVueltas unaCarrera) unaCarrera

darMultiplesVueltas :: Int -> Carrera -> Carrera
darMultiplesVueltas 0 unaCarrera = unaCarrera
darMultiplesVueltas cantidadDeVueltas unaCarrera = darMultiplesVueltas (cantidadDeVueltas - 1) (darVuelta unaCarrera)

-}

-- 3.4 Punto

maximaVelocidad :: Auto -> Auto -> Auto
maximaVelocidad primerAuto segundoAuto 
    | velocidad primerAuto >= velocidad segundoAuto = primerAuto
    | otherwise = segundoAuto

-- Hay que revisar
darParticipanteConMayorVelocidad :: [Auto] -> Auto
darParticipanteConMayorVelocidad = foldl1 maximaVelocidad

quienGana :: Carrera -> Auto
quienGana unaCarrera = (darParticipanteConMayorVelocidad.participantes.correrCarrera) unaCarrera

-- 3.5 El gran truco
elGranTruco :: [Truco] -> Auto -> Auto
elGranTruco trucos = foldl1 (flip (.)) trucos

elGranTruco2 :: [Truco] -> Auto -> Auto
elGranTruco trucos unAuto = (foldl (flip ($)) unAuto) trucos
