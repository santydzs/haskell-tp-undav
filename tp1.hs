--necesario para utlizar la funcion delete, la cual recibe un elemento y una lista y si esta, elimina la primer aparicion del elemento en la lista
import Data.List

data Super = Heroe { nombre :: String, poder :: Float, vida :: Float} | Villano { nombre :: String, poder :: Float, vida :: Float, maldad :: Float} deriving (Show, Eq)

--punto 2
poderReal :: Super -> Float
poderReal (Heroe _ poder _) = poder
poderReal (Villano _ poder _ maldad) = poder - (maldad / 6)

-- punto 3
over9000 :: Super -> Bool
over9000 = (9000<).poderReal

--punto 4
reducirvida vida = (vida-).poderReal

golpear :: Super -> Super -> Super
golpear super (Villano nombre poder vida maldad) = (Villano nombre poder (reducirvida vida super) maldad) 
golpear super (Heroe nombre poder vida) = (Heroe nombre poder (reducirvida vida super))

--punto 5
ganadorCombate :: Super -> Super -> Super
ganadorCombate super1 super2 | 0 >= (vida super2) = super1
                            | 0 >= (vida super1) = super2
                            | otherwise = ganadorCombate (golpear super2 super1) (golpear super1 super2)

--punto 6
ataquePandilla :: Foldable t => Super -> t Super -> Super
ataquePandilla super pandilla = foldl (\ supergolpeado pandillero -> golpear pandillero supergolpeado) super pandilla

--punto 7
estadisticas :: (Super -> Bool) -> [Super] -> [String]
estadisticas funcion lista = map nombre (filter funcion lista)

--test

--punto 7 preubas
punto2testA = estadisticas over9000 pandilla1
punto2testB = estadisticas over9000 pandilla2
punto2testC = estadisticas (\ pandillero -> 0 < (vida (ataquePandilla pandillero (delete pandillero pandilla2)))) pandilla2
punto2testD = estadisticas (\ pandillero -> 0 < (vida (ataquePandilla pandillero (delete pandillero pandilla2)))) pandilla3

pandilla1 = [superman,ironman,goku]
pandilla2 = [ironman,joker,goku]
pandilla3 = [ironman,joker,goku,vegeta]

goku = Heroe "goku" 67888000 5000
superman = Heroe "superman" 10000 5030202
thanos = Villano "thanos" 1000000 10000 6
joker = Villano "joker" 500 5000 3000
ironman = Heroe "ironman" 1500 5030202
vegeta = Heroe "vegeta" 1000 77888000


main = putStrLn (show punto2testD )