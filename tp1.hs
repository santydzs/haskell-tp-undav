data Super = Heroe { nombre :: String, poder :: Float, vida :: Float} | Villano { nombre :: String, poder :: Float, vida :: Float, maldad :: Float} deriving (Show)

participantes = [ Villano "megamente" 1800 50000 9000, Villano "Thanos" 100000 50000 9000 , Villano "Pinguino" 1010 20202020202 253]

--punto 2
poderReal :: Super -> Float
poderReal (Heroe _ poder _) = poder
poderReal (Villano _ poder _ maldad) = poder - (maldad / 6)

-- punto 3
over9000 :: Super -> Bool
over9000 = (9000<).poderReal

--punto 4
reducirvida vida = (vida-).poderReal
golpear heroe (Villano nombre poder vida maldad) = (Villano nombre poder (reducirvida vida heroe) maldad) 
golpear villano (Heroe nombre poder vida) = (Heroe nombre poder (reducirvida vida villano))

--punto 5


--punto 6
ataquePandilla :: Foldable t => Super -> t Super -> Super
ataquePandilla super = foldl (\super atacante -> golpear atacante super ) super

--punto 7
estadisticas :: (Super -> Bool) -> [Super] -> [String]
estadisticas funcion lista = map nombre (filter funcion lista)

--test
batman = Heroe "batman" 2000 300000
megamente = Villano "megamente" 1800 50000 9000

main = putStrLn (show (ataquePandilla batman participantes) )