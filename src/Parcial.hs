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

data Guarderia= UnaGuarderia{
nombre :: String,
rutina :: ([String],[Int])
}

--modelado
zara :: Perro
zara = UnPerro{
raza = "dalmata",
juguetesFavorito = ["pelota","mantita"],
tiempo = 90,
energia = 80 
}

guarderia :: Guarderia 
guarderia = UnaGuarderia{
    nombre = "GuarderíaPdePerritos",
    rutina = (["Jugar","Ladrar 18","Regalar pelota","Día de spa","Día de campo"],[30,20,0,120,720])
}

--funciones auxiliares
cambiarenergia :: Float -> Perro -> Perro
cambiarenergia numero unperro  = unperro{
    energia = max (numero + energia unperro) 0 
}  

listaextravagante = ["dalmata","pomerania"]

ponerEn100EnergiaYRegalarJuguete :: Perro -> Perro
ponerEn100EnergiaYRegalarJuguete unperro = unperro {
energia = 100,
juguetesFavorito =  unperro { juguetesFavorito = juguetesFavorito unperro ++ "peine de goma" }
}


--funciones principales
jugar :: Float -> Perro -> Perro
jugar _ unperro = unperro{
energia = cambiarenergia 10 energia unperro 
}

ladrar :: Float -> Perro -> Perro 
ladrar cantidadladridos unperro = unperro{
energia = cantidadladridos / 2 
}

regalar :: String -> Perro -> Perro
regalar juguete unperro= unperro {
juguetesFavorito = juguete ++ [juguetesFavorito]    
}

diaDeSpa :: Perro -> Perro
diaDeSpa unperro 
    |tiempo unperro >= 50  || elem (raza unperro) listaextravagante = ponerEn100EnergiaYRegalarJuguete unperro
    |otherwise = unperro

diaDeCampo :: Perro -> Perro
diaDeCampo unperro = unperro {
    juguetesFavorito = drop 1 juguetesFavorito
}      

--PARTEB
estaEnGuarderia :: Guarderia -> Perro ->Bool
estaEnGuarderia unaguarderia unperro = (sum snd unaguarderia) >= tiempo unperro

perrosResponsables ::   Perro -> Bool
perrosResponsables unperro = length juguetesFavorito (diaDeCampo unperro) > 3




