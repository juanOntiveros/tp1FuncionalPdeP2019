-- Punto 1
data Auto = Auto {
    nombre :: String,
    nafta :: Float,
    velocidad :: Float,
    nombreDeSuEnamorade :: String,
    truco :: Truco
} deriving Show

type Truco = Auto

deReversaRocha :: Auto -> Auto
deReversaRocha unAuto = unAuto {nafta = ((+ 0.2 * 1000).nafta) unAuto}

impresionar :: Auto -> Auto
impresionar unAuto = unAuto {velocidad = ((*2).velocidad) unAuto}

nitro :: Auto -> Auto
nitro unAuto = unAuto {velocidad = ((+15).velocidad) unAuto}

fingirAmor :: Auto -> String -> Auto
fingirAmor unAuto enamorade = unAuto {nombreDeSuEnamorade = enamorade}

-- Personajes
rochaMcQueen = Auto "RochaMcQueen" 300 0 "Ronco" (deReversaRocha rochaMcQueen)
biankerr = Auto "Biankerr" 500 20 "Tinch" (impresionar biankerr)
gushtav = Auto "Gushtav" 200 130 "PetiLaLinda" (nitro gushtav)
rodra = Auto "Rodra" 0 50 "Taisa" (fingirAmor rodra "Petra")

-- Punto 2 
esVocal :: Char -> Bool
esVocal letra = letra == 'a' || letra == 'e' || letra == 'i' || letra == 'o' || letra == 'u'

calcularVocales :: Auto -> Int
calcularVocales unAuto = (length.(filter esVocal).nombreDeSuEnamorade) unAuto

calcularAumentoVelocidad :: Int -> Float
calcularAumentoVelocidad numero
 | numero > 0 && numero < 3 = 15
 | numero > 2 && numero < 5 = 20
 | numero >= 5 = 30
 | otherwise = 0

incrementarVelocidad :: Auto -> Auto
incrementarVelocidad unAuto = unAuto {velocidad = ((+ (calcularAumentoVelocidad.calcularVocales) unAuto).velocidad) unAuto}

-- Punto 3
quedaNafta :: Auto -> Bool
quedaNafta unAuto = ((> 0).nafta) unAuto

tieneVelocidadBaja :: Auto -> Bool
tieneVelocidadBaja unAuto = ((< 100).velocidad) unAuto

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco unAuto = ((&& quedaNafta unAuto).tieneVelocidadBaja) unAuto

-- Punto 4
comboLoco :: Auto -> Auto
comboLoco unAuto = (nitro.deReversaRocha) unAuto

queTrucazo :: Auto -> String -> Auto
queTrucazo unAuto = incrementarVelocidad.(fingirAmor unAuto)

explotarNafta :: Auto -> Float
explotarNafta unAuto = ((*10).nafta) unAuto

turbo :: Auto -> Auto
turbo unAuto = unAuto {nafta = 0, velocidad = ((+ explotarNafta unAuto).velocidad) unAuto}



activarTruco :: Auto -> Auto
activarTruco unAuto = truco unAuto

cambiarTruco :: Auto -> Truco -> Auto
cambiarTruco unAuto nuevoTruco = unAuto {truco = nuevoTruco}