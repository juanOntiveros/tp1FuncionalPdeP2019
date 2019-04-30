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