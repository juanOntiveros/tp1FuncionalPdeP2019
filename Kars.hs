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
deReversaRocha unAuto = unAuto {nivelDeNafta = ((+ (velocidad unAuto/5)).nivelDeNafta) unAuto}

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

tieneVelocidadBaja :: Auto -> Bool
tieneVelocidadBaja unAuto = ((< 100).velocidad) unAuto

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

cambiarTruco :: Auto -> Truco -> Auto
cambiarTruco unAuto nuevoTruco = unAuto {truco = nuevoTruco}

modificarVelocidad :: (Float -> Float) -> Auto -> Auto
modificarVelocidad modificacion unAuto  = unAuto {velocidad = (modificacion.velocidad) unAuto}

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
sacarAlPistero :: Carrera -> Carrera
sacarAlPistero unaCarrera = unaCarrera {participantes = (tail.participantes) unaCarrera}
