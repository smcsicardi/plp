module Tp where

import Data.List
import Data.Function

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
split x xs = filter (not.null) (foldr (\y rec -> if y == x then []:rec else (y:(head rec)):(tail rec)) [[]] xs)

longitudPromedioPalabras :: Extractor
longitudPromedioPalabras s = mean(map genericLength (split ' ' s))

contar :: Eq a => a -> [a] -> Int
contar x =  length . filter (==x)

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = map (\x -> (contar x xs,x)) (nub xs)

repeticionesPromedio :: Extractor
repeticionesPromedio txt = mean (map (\(x,y) -> fromIntegral x) (cuentas (split ' ' txt)))


tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = [\txt -> realToFrac(contar t txt)/genericLength(txt)| t <- tokens]

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor ts e = let mx = maximum (map (abs.e) ts) in \t -> e t/mx
--El let me evita volver a calcular el máximo cada vez

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures ext txt = let normalizadoExt = map (\ex -> normalizarExtractor txt ex) ext
                           in foldr (\t rec -> (map (\normEx -> normEx t ) normalizadoExt):rec) [] txt

distEuclideana :: Medida
distEuclideana i1 i2 = (sqrt.realToFrac) (foldr (\(x,y) a -> (x-y)^2+a) 0 (zip i1 i2)) --(sqrt.realToFrac.sum) (map (^2) (zipWith (-) i1 i2))

distCoseno :: Medida
distCoseno i1 i2 = realToFrac( (f i1 i2) / ((*) (g i1) (g i2)) ) where
					f l1 l2 = sum (zipWith (*) l1 l2)
					g xs = sqrt (f xs xs) 

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k insts labels fDist = \x -> (snd.maximum.cuentas) (map (snd) (kintanciasmascerca x)) where
							kintanciasmascerca x = take k (calcDists x)
							calcDists x = sort (zipWith (\a b -> (fDist x a, b)) insts labels)

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy xs ys = mean (zipWith (\x y -> if ((==) x y) then 1 else 0) xs ys)

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos da et n p = (take primeros da ++ take ultimos (drop inclusiveP da), take part (drop primeros da), take primeros et ++ take ultimos (drop inclusiveP et), take part (drop primeros et)) where
							part = div (length da) n
							primeros = part*(p-1)
							ultimos = part*n-part*p
							inclusiveP = part*p

nFoldAux:: (Datos, Datos, [Etiqueta], [Etiqueta]) -> Float
nFoldAux (xt, xv, yt, yv) = accuracy (map (\ins -> (knn 15 xt yt distEuclideana) ins) xv) yv
-- Entrena sabiendo que a los datos xt le corresponden las etiquetas yt. Para validar, agarra xv y calcula las etiquetas con los puntos que ya tiene. Comparamos eso con las etiquetas correctas (yv) para ver cómo le fue

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n d es = mean [nFoldAux (separarDatos d es n p) | p<- [1..n]]
				
