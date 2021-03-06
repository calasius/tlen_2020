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

basicas :: Tarea -> Int --dada una tarea devuelve la cantidad de tareas basicas de esa tarea
basicas = foldTarea (\n h -> 1) (\t1 t2 -> t1 + t2) (\t1 t2 h -> t1 + t2) 

cantidadDeTareasBasicas :: [Tarea] -> Int
cantidadDeTareasBasicas = foldr ((+).(basicas)) 0 
 
-- cantidadMaximaDeHoras

maximasHoras :: Tarea -> Int -- dada una tarea devuelve la cant max de horas para completar esa tarea
maximasHoras = foldTarea (\n h -> h) (\t1 t2 -> t1 + t2) (\t1 t2 h -> t1 + t2 + h) 

cantidadMaximaDeHoras :: [Tarea] -> Int
cantidadMaximaDeHoras = foldr ((+).maximasHoras) 0

-- tareasMasLargas

tareasMasLargas :: Int -> [Tarea] -> [Tarea]

--------version recursiva directa, hay que hacerla con foldTarea, lo dejo por si sirve de guia
--tareasMasLargas h []  =  []
--tareasMasLargas h [t] =  case t of 
  --Basica n h'            -> if h' > h then [Basica n h'] else []
  --Independientes t1 t2   -> if cantidadMaximaDeHoras [Independientes t1 t2] > h then [Independientes t1 t2] else []
  --DependeDe t1 t2 h      -> if cantidadMaximaDeHoras [DependeDe t1 t2 h] > h then [DependeDe t1 t2 h] else []

--tareasMasLargas h (t:ts) = tareasMasLargas h [t] ++ tareasMasLargas h ts

tareasMasLargas h ts = filter (\t -> maximasHoras(t) > h) ts


-- Ejercicio 3

-- chauListas

--chauListas :: [Tareas] -> Tarea
chauListas xs = foldr Independientes (head xs) (tail xs)

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

--tareasBasciasQueDependenDe :: String -> Tarea -> [Tarea]
--(String -> Int -> a) -> (Tarea -> Tarea -> a -> a -> a) -> (Tarea -> Tarea -> a-> a -> Int -> a) -> Tarea -> a
tareasBasicasQueDependenDe n = recTarea (\s i -> []) (\t1 t2 a1 a2 -> a1 ++ a2) (\t1 t2 a1 a2 h -> if (esSubTareaDe n t2) then (tareasBasicas t1) else a1 ++ a2)


tarea1 = Basica "a" 3
tarea2 = Basica "b" 1
tarea3 = Basica "c" 1
tarea4 = Basica "d" 2
tarea5 = DependeDe (Independientes tarea2 tarea3) tarea4 2
lista1 = [tarea1]
lista2 = [tarea2,tarea3,tarea4]
lista3 = [tarea1,tarea5]

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

type LuzMagica a = (a -> a)

-- pasos
--pasos :: (Eq a) => a -> [LuzMagica a] -> a -> Int 

compose lms = foldr (\lm rec -> lm . rec) (head lms) (tail lms)
potencias lms = [compose(take i lms) | i <- [1..]]
pasos zi lms zf = foldr (\p rec -> if (p zi) == zf then 0 else rec+1) 0 (potencias (id:lms))