module Lib where
import Text.Show.Functions

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Int,
  superficie :: Int,
  precio :: Int,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

deptosDeEjemplo = [Depto 3 80 7500 "Palermo", Depto 1 45 3500 "Villa Urquiza", Depto 2 50 5000 "Palermo", Depto 1 45 5500 "Recoleta"]

---------1

mayor :: (Ord a, Ord b) => (a->b) -> a -> a -> Bool
mayor funcion valor1 valor2 = funcion valor1 > funcion valor2

mayor2 :: (Ord a, Ord b) => (a->b) -> a -> a -> Bool
mayor2 f val1 = (f val1 >).f

menor :: (Ord a, Ord b)=> (a->b) -> a -> a -> Bool
menor funcion valor1 valor2 = funcion valor1 < funcion valor2

{-
ordenarSegun (mayor length) ["caro","y","camila"] --> ["camila","caro","y"]
 -}

ordenarSegun :: (a->a -> Bool)-> [a]->[a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = 
    (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs


------------2

ubicadoEn :: [Barrio]-> Requisito
ubicadoEn listaB depto = elem (barrio depto) listaB

ubicadoEn2 listaB depto = flip elem listaB .barrio $ depto
ubicadoEn3 listaB depto = flip elem listaB .barrio

cumpleRango :: (Depto -> Int) -> Int -> Int ->Requisito
cumpleRango funcion num1 num2 = between num1 num2 .funcion

between cotaInferior cotaSuperior valor = valor <= cotaSuperior && valor >= cotaInferior

-------------3
cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda depto = all (flip condicionBusqueda depto) 

condicionBusqueda ::  Requisito -> Depto -> Bool
condicionBusqueda requisito = requisito 

-----EJEMPLO
recoPaler = ["Recoleta", "Palermo"]

busquedaEjemplo :: Busqueda
busquedaEjemplo = [ubicadoEn recoPaler, cumpleRango ambientes 1 2, (<6000).precio]
  
  --menor id (precio depto) 6000 | cumpleRango precio 0 6000

departamentosBuscadosEnOrdenDeInteres :: Busqueda -> (Depto->Depto->Bool) -> [Depto] -> [Depto]
departamentosBuscadosEnOrdenDeInteres busqueda criterioO deptos = ordenarSegun criterioO . cumplenRequisito busqueda $ deptos --(cumplenRequisitos busqueda deptos)

cumplenRequisito :: Busqueda -> [Depto] -> [Depto]
cumplenRequisito busqueda = filter (`cumpleBusqueda` busqueda) 

---------------4
mailsDePersonasInteresadas :: [Persona]->Depto -> [Mail]
mailsDePersonasInteresadas personas = map mail . personasInteresadas personas

personasInteresadas :: [Persona] -> Depto -> [Persona]
personasInteresadas personas depto = filter (busquedasUtiles depto) personas

busquedasUtiles :: Depto -> Persona -> Bool
busquedasUtiles depto persona = any (cumpleBusqueda depto) (busquedas persona)