{-# LANGUAGE NamedFieldPuns #-}
module Library where
import PdePreludat
    ( otherwise,
      Eq((==)),
      Ord((>), (<=), (<), min, (>=)),
      Show,
      String,
      fromInteger,
      fromRational,
      (*),
      (+),
      (-),
      (.),
      head,
      length,
      Number,
      negate,
      Bool (True, False) )
import Data.String (IsString)
import qualified GHC.Real as PdePreludat
import qualified Data.String as PdePreludat
import Data.List


data Genero= Terror{litroSangre::Number}|Comedia|Drama {escenas::Number}|Accion{peli::Filmacion}|Tragicomico{importe::Number}|Aventura{critica::Bool, version::String, peli::Filmacion}
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
armaMortal=Filmacion {
                        titulo="Arma Mortal",
                        puntaje=7,
                        anioFilmacion=1987,
                        duracion=109,
                        actores=["Mel Gibson", "Danny Glover" , "Gary Busey"]}
nueveReinas= Filmacion {
                        titulo="9 Reinas",
                        puntaje=8,
                        anioFilmacion=2000,
                        duracion=114,
                        actores=["Gastón Pauls","Ricardo Darín","Leticia Bredice","Pochi Ducasse"]}
laOdiseaDeLosGiles= Filmacion {
                        titulo="La odisea de los giles",
                        puntaje=8,
                        anioFilmacion=2019,
                        duracion=116,
                        actores=["Ricardo Darín", "Luis Brandoni", "Verónica Llinás", "Daniel Aráoz", "Rita Cortese"]}
laFlor= Filmacion {
                        titulo="La Flor",
                        puntaje=7,
                        anioFilmacion=2018,
                        duracion=840,
                        actores=["Pilar Gamboa"]}
speed= Filmacion {
                        titulo="Speed",
                        puntaje=7,
                        anioFilmacion=1994,
                        duracion=116,
                        actores=[ "Keanu Reeves", "Sandra Bullock", "Dennis Hopper", "Jeff Daniels" , "Alan Ruck"]}
indianaJones4 :: Filmacion
indianaJones4= Filmacion {
                        titulo="Indiana Jones IV",
                        puntaje=6,
                        anioFilmacion=2007,
                        duracion=125,
                        actores=["Harrison Ford"]}
indianaJones1= Filmacion {
                        titulo="Indiana Jones I",
                        puntaje=8,
                        anioFilmacion=1981,
                        duracion=115,
                        actores=["Harrison Ford"]}



--Ex 1
-- Saber si una filmación es darinesca, que son aquellas en las que el primer actor es “Ricardo Darín”.

esDarinesca :: Filmacion -> String
esDarinesca peli
  | ((== "Ricardo Darín") . PdePreludat.head . actores) peli = "Es darinesca"
  | otherwise = "No es darinesca"

--Saber si una filmación pinta buena, esto ocurre si tiene 5 ó más actores
pintaBuena :: Filmacion->String
pintaBuena films| 5 <= (genericLength.actores) films ="pinta buena"
                |otherwise="No pinta buena"

--Saber los minutos excedentes de una filmación que se calcula como el valor absoluto entre la diferencia de 115 minutos con una duración de una filmación

minutosExcedentes :: Filmacion -> Number
minutosExcedentes peli
  | duracion peli > 115 = duracion peli - 115
  | otherwise = 115 - duracion peli


--Ex 2

-- Queremos saber el precioBase, que se da de la siguiente manera:
--si pinta grosa (tiene más de 4 actores) sale $200.
--si en cambio es vieja (año menor a 1990) es el doble del largo del título.
--de lo contrario, es $100 más el puntaje * 3

precioBase :: Filmacion -> Number
precioBase peli
  | PdePreludat.length (actores peli) > 4 = 200
  | anioFilmacion peli < 1990 = 2 * (genericLength . titulo) peli
  | otherwise = 100 + puntaje peli * 3


--Queremos saber el precioExtra, que se da de la siguiente manera:
--Si es larga (tiene más de 115 minutos) sale $10 por el excedente de minutos con un tope de $100 como máximo. Por ejemplo si la filmación dura 120 minutos, son $50 debido a que son 5 minutos excedidos a $10 cada minuto.si la filmación dura 200 minutos, solamente tiene un costo extra de $100. 
--Si en cambio no es vieja (año mayor o igual a 1990) $50.
--De lo contrario, no suma precio extra.

precioExtra :: Filmacion -> Number
precioExtra peli
  | ((> 115) . duracion) peli = min 100 ((duracion peli - 115) * 10)
  | ((>= 1990) . anioFilmacion) peli = 50
  | otherwise = 0


  --Queremos saber el precioTotal que se compone de la suma del precio base más el precio extra, pero con la condición que si la sumatoria es mayor a $200, se le aplica un 10% de descuento. 

precioTotal :: Filmacion -> Number
precioTotal peli
  | precioBase peli + precioExtra peli > 200 = 0.9 * (precioBase peli + precioExtra peli)
  | otherwise = precioBase peli + precioExtra peli


--Ex 3


--Terror que depende de la cantidad de litros de sangre utilizada en la película, decrementa ese mismo nivel de satisfacción de la persona. Es decir que por ejemplo para una filmación donde utilizaron 5 litros de sangre, tiene 5 puntos menos de satisfacción.  El nivel de satisfacción puede quedar negativo.
--Comedia que duplica el nivel de satisfacción de una persona y le agrega al final de su nombre “ muy alegre”. 

--Drama que produce aburrimiento y vuelve más viejo a la persona un año. Pero ojo que además aumenta su satisfacción por  la cantidad de escenas felices que aparecen con un tope de 3. Es decir que si hay dos escenas se incrementa en dos, pero si hay 5 se incrementa solamente en 3.
--Acción que si la filmación pintaBuena aumenta la satisfacción en 100 unidades, en caso contrario no. 

--Tragicómico que combina el efecto de una comedia y luego un drama de 4 escenas felices. Cómo combina tanto la comedia como el drama, se le descuenta el importe dos veces e incrementa en dos la cantidad de películas vistas. 
--Aventura que depende del número de versión que es mala en la saga y la filmación (para muchos, la "III" es la mala de Star Wars, aunque para otros la mala de Star Wars es la "I", la "IV" es la mala de Duro de Matar, siempre es una la mala). En caso de ser la versión mala no produce efecto, caso contrario produce el mismo efecto que la comedia.  Por ejemplo la aventura para la filmación “duro de matar IV” para una persona que piensa que la "IV" es la mala de la saga, no produce efecto alguno en la persona ya que finaliza con “IV”. En cambio la acción aplicada a “duro de matar II” para la misma persona que piensa que la "IV" es mala produce el mismo efecto que una comedia. 

modificarSatisfaccion:: (Number->Number)->Persona->Persona
modificarSatisfaccion num persona= persona {satisfaccion=(num.satisfaccion) persona}

modificarEdad:: (Number->Number)->Persona->Persona
modificarEdad num persona= modificarSatisfaccion num persona{edad=((+1).edad)  persona}

modificarNombre::(String->String)->Persona->Persona
modificarNombre name persona= modificarSatisfaccion (+satisfaccion persona) persona {nombre=(name.nombre) persona}

funcDrama::(Bool,Number,Persona)->Persona
funcDrama (True,escenas,person) = modificarEdad (+escenas) person
funcDrama (False,escenas,person)= modificarEdad (+3) person

modificarImporte::(Number->Number)->Persona->Persona
modificarImporte importe persona= modificarPeliculasVistas(+2)persona{dinero=(importe.dinero) persona}

modificarPeliculasVistas::(Number->Number)->Persona->Persona
modificarPeliculasVistas importe persona= persona{cantFilmaciones=(importe.cantFilmaciones) persona}
funcAventura::(Bool,String,String,Persona)->Persona
funcAventura (False,version,nombre,persona) |isInfixOf version nombre == True =persona
                                            |otherwise=modificarNombre (++" muy alegre") persona
funcAventura(True,version,nombre,persona)   |isInfixOf version nombre == False =persona
                                            |otherwise=modificarNombre (++" muy alegre") persona

queGenero:: (Persona, Genero) ->Persona
queGenero (persona,Terror{litroSangre})= modificarSatisfaccion (+ (-1)*litroSangre) persona
queGenero (persona, Comedia)=modificarNombre (++" muy alegre") persona
queGenero (persona, Drama{escenas})=funcDrama (3>escenas, escenas, persona)

queGenero (persona,Accion{peli})|"pinta buena" == pintaBuena peli  =  modificarSatisfaccion (+100) persona
                             |"No pinta buena" == pintaBuena peli = persona
queGenero(persona,Tragicomico{importe})=modificarImporte(+ (-2)*importe) (funcDrama(False,4,modificarNombre(++" muy alegre")persona)) 
queGenero(persona, Aventura{critica,version,peli})=funcAventura(critica,version,titulo peli,persona)
