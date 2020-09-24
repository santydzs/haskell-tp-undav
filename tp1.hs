data Super = Heroe { nombre :: String, poder :: Float, vida :: Float} | Villano { nombre :: String, poder :: Float, vida :: Float, maldad :: Float} deriving (Show)

participantes = [Heroe "batman" 2000 300000, Villano "megamente" 1800 50000 9000, Villano "Thanos" 100000 50000 9000 , Heroe "ironman" 1500 5030202, Heroe "Goku" 100000000000000 1010101010110101010, Heroe "KungFu Panda" 20  202020202, Villano "Pinguino" 1010 20202020202 253]

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
ganadorCombate super1 super2 | 0 >= (vida super1) = super2
                            | 0 >= (vida super2) = super1
                            | otherwise = ganadorCombate (golpear super2 super1) (golpear super1 super2)

--punto 6
ataquePandilla :: Foldable t => Super -> t Super -> Super
ataquePandilla super pandilla = foldl golpear super pandilla

--punto 7
estadisticas :: (Super -> Bool) -> [Super] -> [String]
estadisticas funcion lista = map nombre (filter funcion lista)

--test
batman = Heroe "batman" 1000 5000
megamente = Villano "megamente" 1500 10000 6

main = putStrLn (show (ganadorCombate megamente batman) )