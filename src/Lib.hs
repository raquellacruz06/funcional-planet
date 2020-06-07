module Lib where
import Text.Show.Functions

laVerdad = True
{-Nos piden modelar en Haskell un sistema para simular una competencia de talentos de mascotas. 
De las mascotas sabemos su nombre, edad, su dueño (que a su vez tiene un nombre y años de experiencia 
entrenando mascotas), su nivel de energía y un conjunto de trucos que sabe hacer (en un orden de presentación). 
Además puede, o no, estar distraída en un momento específico.
 
Las mascotas hacen sus presentaciones efectuando todos los trucos que conocen. Tener en cuenta que una mascota 
no podrá realizar un truco si luego de hacerlo su energía quedase negativa o si estuviese distraído. Si en el 
momento de efectuar el truco se encontrara distraido, su dueño lo despertará para el proximo truco.
 
Los trucos son los siguientes:
·  Sentarse: Es el truco más básico. Consume 5 puntos de energía.
·  Tomar agua: Recupera 5 puntos de energía.
·     Perro mojado: Hace su mejor cara de lástima y agrega “Pobre” al principio del nombre. Consume 5 puntos
·      Hacerse el muerto: Aprovecha para descansar y agrega 10 puntos de energía, pero le agarra sueño y queda distraído.
·       Mortal triple: Es tan increíble que agrega 10 años de experiencia a su dueño. Consume 20 puntos de energía.-}

data Mascota = UnaMascota {nombre :: String,
                            edad :: Int,
                            dueño :: Dueño,
                            energia :: Int,
                            trucos :: [Truco],
                            distraido :: Bool} deriving Show

data Dueño = UnDueño {nombreDueño :: String,
                      experiencia :: Int } deriving (Show, Eq)       



type Truco = Mascota -> Mascota

sentarse :: Truco
sentarse mascota = cambiarEnergia (+(-5)) mascota          

tomarAgua :: Truco
tomarAgua mascota = cambiarEnergia (+5) mascota

perroMojado :: Mascota->Mascota
perroMojado = cambiarNombre ("pobre"++) . cambiarEnergia (+(-5)) 
   
cambiarEnergia :: (Int -> Int) -> Truco
cambiarEnergia funcion mascota = mascota {energia = (funcion.energia) mascota }

cambiarNombre :: (String -> String) -> Truco
cambiarNombre funcion mascota = mascota {nombre = (funcion.nombre) mascota }

hacerseElMuerto :: Truco 
hacerseElMuerto  = cambiarEnergia (+10) . cambiarDistraido True

cambiarDistraido :: Bool-> Truco
cambiarDistraido bool mascota = mascota {distraido = bool}

mortalTriple :: Truco 
mortalTriple = cambiarEnergia (+(-10)) . cambiarDueño (cambiarExperiencia (+10))

cambiarDueño :: (Dueño -> Dueño) -> Mascota -> Mascota
cambiarDueño funcion mascota = mascota {dueño = (funcion.dueño) mascota }

cambiarExperiencia :: (Int-> Int) -> Dueño -> Dueño
cambiarExperiencia funcion dueño = dueño {experiencia = (funcion.experiencia) dueño}



{-AyudanteDeSanta: Tiene 10 años, su dueño es Bart Simpson que lo entrena hace 5 años. Comenzará su presentación 
con 50 puntos de energía y realizará los siguientes trucos en este orden:
Sentarse
Hacerse el muerto
Tomar agua
Mortal triple-}

ayudanteDeSanta :: Mascota
ayudanteDeSanta = UnaMascota "Ayudante de Santa" 10 (UnDueño "Bart Simpson" 5 ) 50 [hacerseElMuerto, tomarAgua, mortalTriple] False

bolt :: Mascota
bolt = UnaMascota "Bolt" 5 (UnDueño "Penny" 1) 100 [perroMojado, hacerseElMuerto, sentarse, mortalTriple] False

laTortuga :: Mascota
laTortuga = UnaMascota "La tortuga" 32 (UnDueño "Fede Escarpa" 30) 30 [sentarse, sentarse, sentarse, tomarAgua] True


{-Bolt: Tiene 5 años, su dueña es Penny que tiene 1 año de experiencia entrenando. 
Tiene 100 puntos de energía y realizará:
Perro mojado
Hacerse el muerto
Sentarse
Mortal triple-}

{-LaTortuga: Tiene 32 años, su dueño es Fede Scarpa y la entrena hace 30 años. Es tan lenta que empezará distraída y con 30 puntos de energía para realizar
Sentarse (tres veces seguida)
Tomar agua
-}

{-Las mascotas hacen sus presentaciones efectuando todos los trucos que conocen. 
Tener en cuenta que una mascota no podrá realizar un truco si luego de hacerlo su energía 
quedase negativa o si estuviese distraído. 
Si en el momento de efectuar el truco se encontrara distraido, su dueño lo despertará para el proximo truco.-}

aplicarTrucos :: Mascota -> Truco ->  Mascota
aplicarTrucos mascota truco
 | energia mascota <= 0 || distraido mascota == True =  cambiarDistraido False mascota
 | otherwise = truco mascota

realizarPresentacion :: Mascota -> Mascota
realizarPresentacion mascota = foldl aplicarTrucos mascota (trucos mascota)

{-Realizar la función resultados para obtener los resultados de la presentación de una mascota siguiendo el formato: 
(Nombre Mascota, Puntuación Energía, Puntuación Habilidad, Puntuación Ternura) 

La puntuación de las presentaciones se basan en tres criterios:
Energía: Cuya puntuación se obtiene multiplicando la energía sobrante de la mascota luego de 
la presentación con la edad de la mascota.
Habilidad: Que se calcula como el producto entre la cantidad de trucos en su presentación y los
 años de experiencia de su entrenador.
Ternura: Si la mascota hizo (o planeaba hacer) el truco “Perro Mojado”, entonces obtendrá 20 puntos. 
Si no, se otorgarán los 20 puntos menos la edad de la mascota.

-}
type Criterio = Mascota -> Int
puntuacionEnergia :: Criterio
puntuacionEnergia  mascota = (energia.realizarPresentacion) mascota * edad mascota

puntuacionHabilidad :: Criterio
puntuacionHabilidad mascota = (length.trucos) mascota * (experiencia.dueño) mascota


---------POR CORREGIR TERNURA
puntuacionTernura :: Criterio
puntuacionTernura mascota 
    |elem "pobre" ((words.nombre) mascota) = 20
    |otherwise = 20 -(edad mascota)

--Realizar la función resultados para obtener los resultados de la presentación de una mascota siguiendo el formato: 
--(Nombre Mascota, Puntuación Energía, Puntuación Habilidad, Puntuación Ternura

type Resultado = (String, Int, Int, Int)
resultados :: Mascota -> Resultado
resultados mascota= (nombre mascota, puntuacionEnergia mascota, puntuacionHabilidad mascota, puntuacionTernura mascota)

{-Realizar la función ganadorDeCategoria que recibe un criterio y una lista de mascotas 
y nos devuelve la mejor mascota según ese criterio luego de realizar sus presentaciones.-}

ganadorDeCategoria :: [Mascota] -> Criterio -> Mascota
ganadorDeCategoria mascotas criterio = foldl1 (elMejorDeDos criterio) (nuevasMascotas mascotas)

nuevasMascotas :: [Mascota] -> [Mascota]
nuevasMascotas mascotas = map realizarPresentacion mascotas

--------- OPCION 2 CON WHERE
--nuevasMascotas 
--where nuevasMascotas = map realizarPresentacion mascotas

elMejorDeDos :: Criterio ->Mascota -> Mascota ->  Mascota
elMejorDeDos criterio mascota1 mascota2 
    |criterio mascota1 >= criterio mascota2 = mascota1
    |otherwise = mascota2


{-Realizar la función ganadorDelConcurso que recibe una lista de mascotas y nos devuelve la que ha obtenido la mayor 
cantidad de puntos luego de realizar sus presentaciones (La suma de los puntos obtenidos de los 3 criterios).-}

ganadorDelConcurso :: [Mascota] -> Mascota
ganadorDelConcurso  mascotas = ganadorDeCategoria mascotas puntuacionTotal

puntuacionTotal :: Criterio
puntuacionTotal mascota = (puntuacionEnergia mascota) + (puntuacionHabilidad mascota) + (puntuacionTernura mascota)

{-Queremos agregar que el truco Tomar agua además de recuperar energía, le saque tiempo a 
nuestra mascota y haga que no pueda realizar el último truco de su presentación.
¿Se podría resolver de la misma manera?
Esperamos sus soluciones! si se animan.
-}
------Nuevo Tomar Agua

nuevaTomarAgua :: Truco
nuevaTomarAgua = cambiarEnergia (+5) . cambiarTruco quitarTruco

cambiarTruco :: ([Truco]-> [Truco]) -> Mascota -> Mascota
cambiarTruco funcion mascota = mascota {trucos = (funcion.trucos) mascota}

quitarTruco :: [Truco] -> [Truco]
quitarTruco trucos = init trucos







