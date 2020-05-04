import Data.List
import Test.HUnit

data Tarea =
  Basica String Int |
  Independientes Tarea Tarea |
  DependeDe Tarea Tarea Int deriving Eq

instance Show Tarea where
  show = foldTarea (\i h -> i)
    (\a b -> "(" ++ a ++ " y " ++ b ++ ")")
    (\a b h -> "(" ++ b ++ " tras " ++ a ++ ")")

-- Ejercicio 1

-- recTarea
recTarea :: (String -> Int -> a) -> (Tarea -> Tarea -> a -> a -> a) -> (Tarea -> Tarea -> a-> a -> Int -> a) -> Tarea -> a
recTarea casoBasico casoIndependientes casoDependeDe t =  case t of
  Basica n h            -> casoBasico n  h
  Independientes t1 t2  -> casoIndependientes t1 t2 (rec t1) (rec t2)
  DependeDe t1 t2 h     -> casoDependeDe t1 t2 (rec t1) (rec t2) h
  where rec = recTarea casoBasico casoIndependientes casoDependeDe



-- foldTarea

foldTarea :: (String -> Int -> a) -> (a -> a -> a) -> (a -> a -> Int -> a ) -> Tarea -> a

foldTarea casoBasico casoIndependientes casoDependeDe = recTarea (\n h -> b n h) (\t1 t2 r1 r2 -> i r1 r2) (\t1 t2 r1 r2 h -> dd r1 r2 h)
          where b  = casoBasico
                i  = casoIndependientes
                dd = casoDependeDe

-- Ejercicio 2

-- cantidadDeTareasBasicas

basicas :: Tarea -> Int 
basicas = foldTarea (\n h -> 1) (\t1 t2 -> t1 + t2) (\t1 t2 h -> t1 + t2) 

cantidadDeTareasBasicas :: [Tarea] -> Int
cantidadDeTareasBasicas = foldr ((+).(basicas)) 0 
 
-- cantidadMaximaDeHoras

maximasHoras :: Tarea -> Int 
maximasHoras = foldTarea (\n h -> h) (\t1 t2 -> t1 + t2) (\t1 t2 h -> t1 + t2 + h) 

cantidadMaximaDeHoras :: [Tarea] -> Int
cantidadMaximaDeHoras = foldr ((+).maximasHoras) 0

-- tareasMasLargas

tareasMasLargas :: Int -> [Tarea] -> [Tarea]

tareasMasLargas h ts = filter (\t -> maximasHoras(t) > h) ts


-- Ejercicio 3

-- chauListas

chauListas :: [Tarea] -> Tarea
chauListas xs = foldr (Independientes) (head xs) (tail xs)

-- Ejercicio 4

-- tareasBasicas

tareasBasicas :: Tarea -> [Tarea]
tareasBasicas = foldTarea (\n h -> [Basica n h]) (\t1 t2 -> t1 ++ t2) (\t1 t2 h -> t1 ++ t2)

-- esSubTareaDe

esSubTareaDe :: String -> Tarea -> Bool
esSubTareaDe s = foldTarea (\n h -> s == n) (\t1 t2 -> t1 || t2) (\t1 t2 h -> t1 || t2)

-- tareasBasicasIniciales

tareasBasicasIniciales :: Tarea -> [Tarea]
tareasBasicasIniciales = foldTarea (\n h -> [Basica n h]) (\t1 t2 -> t1 ++ t2) (\t1 t2 h -> t2)

-- tareasBasicasQueDependenDe

tareasBasicasQueDependenDe :: String -> Tarea -> [Tarea]
tareasBasicasQueDependenDe n = recTarea (\s i -> []) (\t1 t2 a1 a2 -> a1 ++ a2) (\t1 t2 a1 a2 h -> if (esSubTareaDe n t2) then (tareasBasicas t1) else a1 ++ a2)

-- Ejercicio 5

-- cuelloDeBotella

cantDep :: Tarea -> Int
cantDep = foldTarea (\n h -> 0) (\t1 t2 -> t1 + t2) (\t1 t2 h -> 1 + t1 + t2)

ordCantDep :: Tarea -> Tarea -> Ordering
ordCantDep t1 t2 | cantDep t1 > cantDep t2  = LT
                 | cantDep t1 <= cantDep t2 = GT

ordPorDep :: Tarea -> [Tarea]
ordPorDep t = sortBy (ordCantDep) (tareasBasicas t)

nombre :: Tarea -> String
nombre (Basica n h) = n

cuelloDeBotella :: Tarea -> String
cuelloDeBotella t = nombre (head (ordPorDep t))

-- Ejercicio 6

type LuzMagica a = (a -> a)

-- pasos
pasos = undefined
--pasos :: (Eq a) => a -> [LuzMagica a] -> a -> Int 
--pasos zf (m:lm) zi = if zi == zf then 0 else 1 + pasos zf lm (m zi)

-- Tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6
  ]

tarea1 = Basica "a" 3
tarea2 = Basica "b" 1
tarea3 = Basica "c" 1
tarea4 = Basica "d" 2
tarea5 = DependeDe (Independientes tarea2 tarea3) tarea4 2
tarea6 = Independientes tarea1 tarea2
lista1 = [tarea1]
lista2 = [tarea2,tarea3,tarea4]
lista3 = [tarea1,tarea5]
lista4 = [tarea6]

sumas1 :: [LuzMagica Int]
sumas1 = ((+1):sumas1)
sumas123 :: [LuzMagica Int]
sumas123 = ((+1):((+2):((+3):sumas123)))

testsEj1 = test [
  "a" ~=? recTarea (\n h -> n) (\t1 t2 s1 s2 -> s1) (\t1 t2 s1 s2 h -> s1) tarea1,
  "a" ~=? foldTarea (\n h -> n) (\s1 s2 -> s1) (\s1 s2 h -> s1) tarea1,
  "b" ~=? foldTarea (\n h -> n) (\s1 s2 -> s2) (\s1 s2 h -> s2) tarea6,
  "b" ~=? recTarea (\n h -> n) (\t1 t2 s1 s2 -> s2) (\t1 t2 s1 s2 h -> s2) tarea6,
  "b"  ~=? foldTarea (\n h -> n) (\s1 s2 -> s1) (\s1 s2 h -> s1) tarea5
  ]

testsEj2 = test [
  1 ~=? cantidadDeTareasBasicas lista1,
  4 ~=? cantidadDeTareasBasicas lista3,
  3 ~=? cantidadMaximaDeHoras lista1,
  9 ~=? cantidadMaximaDeHoras lista3,
  [] ~=? tareasMasLargas 3 lista1,
  [tarea5] ~=? tareasMasLargas 3 lista3,
  2 ~=? cantidadDeTareasBasicas lista4
  ]

testsEj3 = test [
  tarea1 ~=? chauListas lista1
  ]

testsEj4 = test [
  lista1 ~=? tareasBasicas tarea1,
  lista2 ~=? tareasBasicas tarea5,
  False ~=? esSubTareaDe "b" tarea1,
  True ~=? esSubTareaDe "b" tarea5,
  [tarea1] ~=? tareasBasicasIniciales tarea1,
  [tarea4] ~=? tareasBasicasIniciales tarea5,
  [] ~=? tareasBasicasQueDependenDe "b" tarea5,
  [tarea2,tarea3] ~=? tareasBasicasQueDependenDe "d" tarea5
  ]

testsEj5 = test [
  "a" ~=? cuelloDeBotella tarea1,
  "d" ~=? cuelloDeBotella tarea5
  ]

testsEj6 = test [
  5 ~=? pasos 10 sumas1 5,
  30 ~=? pasos 60 sumas123 0
  ]
