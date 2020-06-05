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

menor :: (Ord a, Ord b)=> (a->b) -> a -> a -> Bool
menor funcion valor1 valor2 = funcion valor1 < funcion valor2

{-ordenarStrings :: [String] -> [String]
ordenarStrings = ordenarSegun (mayor length) 
 -}

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = 
    (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs


------------2
ubicadoEn :: [Barrio]-> Depto -> Bool
ubicadoEn listaB depto = elem (barrio depto) listaB

cumpleRango :: (Depto -> Int) -> Int -> Int ->Depto -> Bool
cumpleRango funcion num1 num2 = between num1 num2 .funcion

between cotaInferior cotaSuperior valor = valor <= cotaSuperior && valor >= cotaInferior

-------------3
cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda depto = all (flip condicionBusqueda depto) 

condicionBusqueda requisito = requisito 

--buscar :: Busqueda -> () -> [Depto] -> [Depto]
buscar busqueda criterioO listaD = ordenarSegun criterioO (cumplenCriterio busqueda listaD)

cumplenCriterio :: Busqueda -> [Depto] -> [Depto]
cumplenCriterio busqueda = filter (`cumpleBusqueda` busqueda) 

busquedaEjemplo depto = [ubicadoEn recoPaler depto,cumpleRango ambientes 1 2 depto, menor id (precio depto) 6000]

recoPaler = ["Recoleta", "Palermo"]

--ejemplo = buscar busquedaEjemplo (mayor superficie) deptosDeEjemplo

---------------4
mailsDePersonasInteresadas :: [Persona]->Depto -> [Mail]
mailsDePersonasInteresadas personas = map mail . personasCumplen personas

personasCumplen :: [Persona] -> Depto -> [Persona]
personasCumplen personas depto = filter (busquedasUtiles depto) personas

busquedasUtiles :: Depto -> Persona -> Bool
busquedasUtiles depto persona = any (cumpleBusqueda depto) (busquedas persona)