data Heroe = Heroe { hnombre :: String, hpoder :: Int, hvida :: Int}
data Villano = Villano { vnombre :: String, vpoder :: Int, vvida :: Int}

hero1 = Heroe {hnombre = "batman", hpoder = 2000, hvida = 300000}

main = putStrLn (hnombre hero1)