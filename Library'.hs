{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Library where
import Data.List
import PdePreludat
import Data.String (IsString)


data Filmacion = Filmacion {
    titulo :: String,
    puntaje :: Number,
    anioFilmacion :: Number,
    duracion :: Number,
    actores :: [String]
}deriving (Show)

data Persona = Persona {
    nombre::String,
    satisfaccion:: Number,
    edad::Number,
    cantFilmaciones::Number,
    dinero::Number
}deriving(Show)

x= Persona {nombre="senior X", satisfaccion=10, edad=30, cantFilmaciones=10, dinero=20}
armaMortal=Filmacion {titulo="Arma Mortal", puntaje=7, anioFilmacion=1987, duracion=109, actores=["Mel Gibson", "Danny Glover" , "Gary Busey"]}
nueveReinas= Filmacion {titulo="9 Reinas", puntaje=8, anioFilmacion=2000, duracion=114, actores=["Gastón Pauls","Ricardo Darín","Leticia Bredice","Pochi Ducasse"]}
laOdiseaDeLosGiles= Filmacion {titulo="La odisea de los giles", puntaje=8, anioFilmacion=2019, duracion=116, actores=["Ricardo Darín", "Luis Brandoni", "Verónica Llinás", "Daniel Aráoz", "Rita Cortese"]}
laFlor= Filmacion {titulo="La Flor", puntaje=7, anioFilmacion=2018, duracion=840, actores=["Pilar Gamboa"]}
speed= Filmacion {titulo="Speed", puntaje=7, anioFilmacion=1994, duracion=116, actores=[ "Keanu Reeves", "Sandra Bullock", "Dennis Hopper", "Jeff Daniels" , "Alan Ruck"]}
indianaJones4= Filmacion {titulo="Indiana Jones IV", puntaje=6, anioFilmacion=2007, duracion=125, actores=["Harrison Ford"]}
indianaJones1= Filmacion {titulo="Indiana Jones I", puntaje=8, anioFilmacion=1981, duracion=115, actores=["Harrison Ford"]}

type Filmaciones=[Filmacion]

filmaciones=[armaMortal,nueveReinas,laOdiseaDeLosGiles, laFlor, speed, indianaJones1,indianaJones4]

cantArray::[a]->Number
cantArray = genericLength

mayorIgual:: (Number,Number)->Bool
mayorIgual (numCant,num) = num <= numCant

diceSiPinta::Bool->String
diceSiPinta True ="pinta buena"
diceSiPinta False= "No pinta buena"

--Saber si una filmación pinta buena, esto ocurre si tiene 5 ó más actores
pintaBuena :: Filmacion->String
pintaBuena films = diceSiPinta (mayorIgual (cantArray (actores films),5))


pintaBuena' :: Filmaciones->[String]
pintaBuena' = Data.List.map pintaBuena

--MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM



precioExtra::Filmacion->Number
precioExtra films|(115< duracion films) && (120>duracion films)= (duracion films-115)*10
                |120<= duracion films=100
                |1990<= anioFilmacion films= 50
                |otherwise=0

precioExtra'::Filmaciones->[Number]
precioExtra' = Data.List.map precioExtra

--MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

data Genero= Drama {escenas::Number}|Accion{peli::Filmacion}


modificarSatisfaccion:: (Number->Number)->Persona->Persona
modificarSatisfaccion num persona= persona {satisfaccion=(num.satisfaccion) persona}

modificarEdad:: (Number->Number)->Persona->Persona
modificarEdad num persona= modificarSatisfaccion num persona{edad=((+1).edad)  persona}

queVio:: (Persona, Genero) ->Persona
queVio (persona, Drama{escenas})|3>escenas = modificarEdad (+escenas) persona
                                |3<=escenas = modificarEdad (+3) persona
queVio (persona,Accion{peli})|mayorIgual (cantArray (actores peli),5)  =  modificarSatisfaccion (+100) persona
                             |otherwise = persona


