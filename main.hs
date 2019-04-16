-- Punto 1
data Auto = Auto {
    nombre :: String,
    nafta :: Float,
    velocidad :: Float,
    nombreDeSuEnamorade :: String
    -- Falta Modelar Trucos
} deriving Show

deReversaRocha :: Auto -> Auto
deReversaRocha unAuto = unAuto {nafta = ((+ 0.2 * 1000).nafta) unAuto}

impresionar :: Auto -> Auto
impresionar unAuto = unAuto {velocidad = ((*2).velocidad) unAuto}

nitro :: Auto -> Auto
nitro unAuto = unAuto {velocidad = ((+15).velocidad) unAuto}

fingirAmor :: Auto -> String -> Auto
fingirAmor unAuto enamorade = unAuto {nombreDeSuEnamorade = enamorade}

-- Personajes
rochaMcQueen = Auto "RochaMcQueen" 300 0 "Ronco"
biankerr = Auto "Biankerr" 500 20 "Tinch"
gushtav = Auto "Gushtav" 200 130 "PetiLaLinda"
rodra = Auto "Rodra" 0 50 "Taisa"

-- Punto 3
quedaNafta :: Auto -> Bool
quedaNafta unAuto = ((> 0).nafta) unAuto

tieneVelocidadBaja :: Auto -> Bool
tieneVelocidadBaja unAuto = ((< 100).velocidad) unAuto

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco unAuto = ((&& quedaNafta unAuto).tieneVelocidadBaja) unAuto

-- Punto 4 "Falta queTrucazo"
comboLoco :: Auto -> Auto
comboLoco unAuto = (nitro.deReversaRocha) unAuto

explotarNafta :: Auto -> Auto
explotarNafta unAuto = ((*10).nafta) unAuto

turbo :: Auto -> Auto
turbo unAuto = unAuto {nafta = 0, velocidad = ((+ explotarNafta unAuto).velocidad) unAuto}
