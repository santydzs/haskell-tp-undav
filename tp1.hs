data Super = Heroe { nombre :: String, poder :: Float, vida :: Float} | Villano { nombre :: String, poder :: Float, vida :: Float, maldad :: Float}

participantes = [Heroe "batman" 2000 300000, Villano "megamente" 1800 50000 9000, Heroe "ironman" 1500 5030202, Heroe "Goku" 100000000000000 1010101010110101010, Heroe "KungFu Panda" 20  202020202, Villano "Pinguino" 1010 20202020202 253]

poderReal (Heroe _ poder _) = poder
poderReal (Villano _ poder _ maldad) = poder - (maldad / 6)

over9000 = (9000<).poderReal

megamente = Villano "megamente" 1800 50000 9000
main = putStrLn (show (over9000 megamente))