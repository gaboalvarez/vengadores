import Text.Show.Functions
data Personaje = Personaje {nombre::String, ataqueFavorito::Ataque,elementos::[String], energia::Int} deriving(Show)
ejemplo = Personaje{nombre="ejemplo",ataqueFavorito=estudiar,elementos=["lapiz","goma","escudo"],energia=100}
type Ataque = (Personaje->Personaje)
hulk = Personaje "hulk" superFuerza ["pantalones"] 90
thor = Personaje "thor" (relampagos 50) ["mjolnir"] 100
viuda = Personaje "viuda negra" artesMarciales [] 91
capitan = Personaje "capitan américa" arrojarEscudo ["escudo"] 80
halcon = Personaje "ojo de halcon" arqueria ["arco", "flechas"] 70
vision = Personaje "vision" (proyectarRayos 5) ["gema del infinito"] 100
ironMan = Personaje "iron man" (ironia (relampagos (-50))) ["armadura", "jarvis", "plata"] 60
ultron = Personaje "robot ultron"  corromperTecnologia [] 100 

estudiar personaje = personaje{energia=(energia personaje)-1}
superFuerza personaje = personaje{energia=0}
relampagos cant personaje = personaje{energia=(energia personaje)-cant}
arqueria personaje | not(elem "escudo" (elementos personaje)) = superFuerza personaje
 | otherwise = personaje
proyectarRayos cant personaje = relampagos cant personaje
arrojarEscudo personaje = personaje{elementos=[]}
artesMarciales personaje = personaje{ataqueFavorito=id}
ironia nuevoAtaque personaje = personaje{ataqueFavorito=nuevoAtaque}
corromperTecnologia personaje = (artesMarciales.superFuerza.arrojarEscudo) personaje

saberEnergia = energia

esRobot personaje = (take 5 (nombre personaje))=="robot"
poseeElemento elemento personaje = elem elemento (elementos personaje)
potencia personaje = (energia personaje)*(length(elementos personaje))

pelea atacante victima | saberEnergia((ataqueFavorito((ataqueFavorito atacante) victima)) atacante) > saberEnergia( (ataqueFavorito atacante) victima) = atacante
 | otherwise = victima

tienenGema personajes =  any (poseeElemento "gema del infinito") personajes
batallaFinal vengadores robots | (tienenGema vengadores)&&not(tienenGema robots) = "ganan vengadores"
 | (tienenGema robots)&&not(tienenGema vengadores) = "ganan robots"
 | length(filter esRobot (listaGanadores vengadores robots)) < (div (length vengadores) 2) = "ganan los vengadores"
 | otherwise = "gana ultron"
listaGanadores [] robots = []
listaGanadores vengadores [] = []
listaGanadores (vengador:restoVengadores) (robot:restoRobots) = [(pelea vengador robot)]++(listaGanadores restoVengadores restoRobots)

vengadoress = [hulk,thor,capitan,ironMan]
robotss = [Personaje ("robot "++show n) (proyectarRayos 1) [] 100 | n <-[1..]]