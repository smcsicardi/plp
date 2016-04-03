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
longitudPromedioPalabras s = mean([genericLength(a) | a <- split " " s])

contar :: Eq a => a -> [a] -> Int
contar x =  length . filter (==x)

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = [contar x xs | x <- nub xs] 

repeticionesPromedio :: Extractor
repeticionesPromedio = mean[fromInteger(x)|(x, y) <- cuentas split " " s]

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = undefined

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor = undefined

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures = undefined

distEuclideana :: Medida
distEuclideana = undefined

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
