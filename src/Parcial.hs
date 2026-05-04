module Parcial where
import Text.Show.Functions()

doble :: Int -> Int
doble = (*2)

--datas
data Perro = UnPerro{
    raza :: String,
    juguetesFavorito :: [String],
    tiempo :: Int,
    energia :: Float
    }


zara :: Perro
zara = UnPerro{
raza = "dalmata",
juguetesFavorito = ["pelota","mantita"],
tiempo = 90,
energia = 80 
}


data Guarderia= UnaGuarderia{
    nombre :: String,
    rutina :: ([String],[Int])
}


guarderia :: Guarderia 
guarderia = UnaGuarderia{
    nombre = "GuarderíaPdePerritos",
    rutina = (["Jugar","Ladrar 18","Regalar pelota","Día de spa","Día de campo"],[30,20,0,120,720])
}

--funciones auxiliares
cambiarEnergia :: Float -> Perro -> Perro
cambiarEnergia numero unperro  = unperro{
    energia = max (numero + energia unperro) 0 
}  

listaextravagante = ["dalmata","pomerania"]

ponerEn100EnergiaYRegalarJuguete :: Perro -> Perro
ponerEn100EnergiaYRegalarJuguete unperro = unperro {
energia = 100,
juguetesFavorito = juguetesFavorito unperro ++ ["peine de goma"] 
}


--funciones principales
jugar :: Float -> Perro -> Perro
jugar 1 unperro = cambiarEnergia (-10) unperro

ladrar :: Float -> Perro -> Perro 
ladrar cantidadladridos unperro = unperro{
energia = max 0 (cantidadladridos /2) 
}

regalar :: String -> Perro -> Perro
regalar juguete unperro= unperro {
juguetesFavorito = juguetesFavorito unperro ++ [juguete]    
}

diaDeSpa :: Perro -> Perro
diaDeSpa unperro 
    |tiempo unperro >= 50  || (elem . raza) unperro listaextravagante = ponerEn100EnergiaYRegalarJuguete unperro
    |otherwise = unperro

diaDeCampo :: Perro -> Perro
diaDeCampo unperro = unperro {
juguetesFavorito = drop 1 (juguetesFavorito unperro)
}      

--PARTE B 
estaEnGuarderia :: Guarderia -> Perro -> Bool 
estaEnGuarderia unaguarderia unperro = tiempo unperro >= (sum . snd . rutina) unaguarderia

perrosResponsables :: Perro -> Bool 
perrosResponsables unperro = (length . juguetesFavorito .diaDeCampo) unperro > 3 



