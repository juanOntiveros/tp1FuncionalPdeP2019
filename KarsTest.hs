import Kars
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "Pruebas de la punto 0" $ do
        it "1. El nivelDeNafta de rochaMcQueen, luego de hacer el truco deReversaRocha, es 300" $ do
            (nivelDeNafta.realizarTruco) rochaMcQueen `shouldBe` (300 :: Float)

    describe "Pruebas de la punto 1" $ do
        it "1. El numero de vueltas de potreroFunes es 3" $ do
            cantidadDeVueltas potreroFunes `shouldBe` (3 :: Int)

        it "2. La longitud de la pista de potreroFunes es 5.0" $ do
            longitudPista potreroFunes `shouldBe` (5.0 :: Float)

        it "3. Hay 3 personajes en el publico de potreroFunes" $ do
            (length.publico) potreroFunes `shouldBe` (3 :: Int)

        it "4. El primer personaje del publico de potreroFunes es Ronco" $ do
            (head.publico) potreroFunes `shouldBe` ("Ronco" :: String)

        it "5. El ultimo personaje del publico de potreroFunes es Dodain" $ do
            (last.publico) potreroFunes `shouldBe` ("Dodain" :: String)

        it "6. Hay 4 participantes en la carrera de potreroFunes" $ do
            (length.participantes) potreroFunes `shouldBe` (4 :: Int)

        it "7. La primera posicion de potreroFunes es de rochaMcQueen" $ do
            (nombre.head.participantes) potreroFunes `shouldBe` (nombre rochaMcQueen :: String)

        it "8. La ultima posicion de potreroFunes es de rodra" $ do
            (nombre.last.participantes) potreroFunes `shouldBe` (nombre rodra :: String)

    describe "Pruebas de la punto 2" $ do
        it "1. sacarAlPistero de potreroFunes deja 3 participantes en juego" $ do
            (length.participantes.sacarAlPistero) potreroFunes `shouldBe` (3 :: Int)

        it "2. Luego de sacarAlPistero de potreroFunes el primero es biankerr" $ do
            (nombre.head.participantes.sacarAlPistero) potreroFunes `shouldBe` (nombre biankerr :: String)

        it "3. Luego de sacarAlPistero de potreroFunes el ultimo es rodra" $ do
            (nombre.last.participantes.sacarAlPistero) potreroFunes `shouldBe` (nombre rodra :: String) 

        it "4. rochaMcQueen ya no participa en potreroFunes tras sacarAlPistero" $ do
            elem (nombre rochaMcQueen) (((map nombre).participantes.sacarAlPistero) potreroFunes) `shouldBe` (False :: Bool)

        it "5. La cantidad de participantes despues de aplicar pocaReserva en potreroFunes es 3" $ do
            (length.participantes.pocaReserva) potreroFunes `shouldBe` (3 :: Int)

        it "6. rodra ya no participa en potreroFunes tras pocaReserva" $ do
            elem (nombre rodra) (((map nombre).participantes.pocaReserva) potreroFunes) `shouldBe` (False :: Bool)

        it "7. La cantidad de participantes despues de aplicar podio en potreroFunes es 3" $ do
            (length.participantes.podio) potreroFunes `shouldBe` (3 :: Int)

        it "8. La velocidad del ultimo despues de aplicar lluvia sobre potreroFunes es 40" $ do
            (velocidad.last.participantes.lluvia) potreroFunes `shouldBe` (40 :: Float)

    describe "Pruebas de la punto 3" $ do
        it "1. Consultar la nafta del primero luego de darVuelta es 490" $ do
            (nivelDeNafta.head.participantes.darVuelta) potreroFunes `shouldBe` (490 :: Float)

        it "2. Consultar la velocidad del primero luego de darVuelta es 40" $ do
            (velocidad.head.participantes.darVuelta) potreroFunes `shouldBe` (40 :: Float)

        it "3. Consultar la cantidad de participantes luego de dar dos vueltas es 2" $ do
            (length.participantes.darVuelta.darVuelta) potreroFunes `shouldBe` (2 :: Int)

        it "4. Consultar la nafta del primero luego de dar dos vueltas es 70" $ do
            (nivelDeNafta.head.participantes.darVuelta.darVuelta) potreroFunes `shouldBe` (70 :: Float)
            
        it "5. Consultar la cantidad de participantes luego de correr la carrera es 1" $ do
            (length.participantes.correrCarrera) potreroFunes `shouldBe` (1 :: Int)
        
        it "6. Consultar el primero luego de correr la carrera es Rodra" $ do
            (nombre.head.participantes.correrCarrera) potreroFunes `shouldBe` ("Rodra" :: String)

    describe "Pruebas de la punto 4" $ do
        it "1. Consultar el ganador de potreroFunes es Rodra" $ do
            (nombre.quienGana) potreroFunes `shouldBe` ("Rodra" :: String)

    describe "Pruebas de la punto 5" $ do
        it "1. Consultar la velocidad de rodra tras realizar elGranTruco con nitro, deReversa e impresionar es 130" $ do
            (velocidad.(elGranTruco [nitro,deReversaRocha,impresionar])) rodra `shouldBe` (130 :: Float)
        
        it "2. Consultar la nafta de rodra tras realizar elGranTruco con nitro, deReversa e impresionar es 13" $ do
            (nivelDeNafta.(elGranTruco [nitro,deReversaRocha,impresionar])) rodra `shouldBe` (13 :: Float)        
            
