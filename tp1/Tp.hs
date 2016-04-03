module Tp where

import Data.List

type Texto = String
type Feature = Float
type Instancia = [Feature]
type Extractor = (Texto -> Feature)

type Datos = [Instancia]
type Etiqueta = String
type Modelo = (Instancia -> Etiqueta)
type Medida = (Instancia -> Instancia -> Float)

tryClassifier :: [Texto] -> [Etiqueta] -> Float
tryClassifier x y = let xs = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x in
    nFoldCrossValidation 5 xs y

mean :: [Float] -> Float
mean xs = realToFrac (sum xs) / genericLength xs

split :: Eq a => a -> [a] -> [[a]]
split = undefined

longitudPromedioPalabras :: Extractor
longitudPromedioPalabras s = mean(map genericLength (split ' ' s))

contar :: Eq a => a -> [a] -> Int
contar x =  length . filter (==x)

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = [(contar x xs,x) | x <- nub xs]

repeticionesPromedio :: Extractor
repeticionesPromedio = undefined --mean[fromInteger(x)|(x, y) <- cuentas split ' ' s]

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
-- frecuenciaTokens = [\txt -> head([x|(x, y) <- cuentas txt, y == t]) /genericLength(txt)| t <- tokens]
frecuenciaTokens = [\txt -> realToFrac(contar t txt)/genericLength(txt)| t <- tokens]

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor ts = \e t -> e t/maximum (map (abs.e) ts)

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures = undefined

distEuclideana :: Medida
distEuclideana i1 i2 = sqrt sum (map (^2) (zipWith (-) i1 i2))

distCoseno :: Medida -- toma dos [Feature] y devuelve un float
distCoseno i1 i2 = (div) (f i1 i2) ((*) (g i1) (g i2)) where
					f l1 l2 = sum (zipWith (*) l1 l2)
					g xs = sqrt (f xs xs) 

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn = undefined

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy xs ys = mean (map (\x y -> if ((==) x y) then 1 else 0) (zip xs ys))

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos = undefined

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined
