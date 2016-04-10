-- Para correr los tests:
-- ghc Tests-alu.hs && ./Tests-alu

import Tp
import Test.HUnit
import Data.List

-- evaluar main para correr todos los tests
main = runTestTT allTests

allTests = test [
 	"split" ~: testsSplit,
 	"cuentas" ~: testsCuentas,
	"longitudPromedioPalabras" ~: testsLongPromPal,
	"repeticionesPromedio" ~: testsRepProm,
	"frecuenciaTokens" ~: testsFrecTok,
	"normalizarExtractor" ~: testsNormExt,
	"distEuclideana" ~: testsDistEucl,
	"distCoseno" ~: testsDistCos,
	"knn" ~: testsknn,
	"accuracy" ~: testsAcc,
	"separarDatos" ~: testsSepDat
 	]

testsSplit = test [
 	split ',' ",PLP," ~?= ["PLP"],
 	split ',' " ,PLP, " ~?= [" ","PLP"," "]
  	]

testsCuentas = test [
	cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")],
	cuentas "invisible" ~?= [(3,'i'),(1,'n'),(1,'v'),(1,'s'),(1,'b'),(1,'l'),(1,'e')]
	]

testsLongPromPal = test [
	longitudPromedioPalabras "vete de mi, cuervo negro" ~?= 4,
	longitudPromedioPalabras "toma el tren hacia el sur" ~?= 20/6
	]

testsRepProm = test [
	repeticionesPromedio "ya despiertate nena sube al rayo al fin ya despiertate rayo sube a la nena" ~?= 15/9,
	repeticionesPromedio "a a a b b b c c c" ~?= 3
	]

testsFrecTok = test [
	(last frecuenciaTokens) "999" ~=? 1,
	(last frecuenciaTokens) "0909" ~=? 0.5,
	(head frecuenciaTokens) "000" ~=? 0
	]

testsNormExt = test [
	(normalizarExtractor ["ya despiertate nena sube al rayo al fin ya despiertate rayo sube a la nena","a a a b b b c c c"] repeticionesPromedio) "ya despiertate nena sube al rayo al fin ya despiertate rayo sube a la nena" ~?= 5/9,
	(normalizarExtractor ["vete de mi, cuervo negro", "toma el tren hacia el sur"] longitudPromedioPalabras) "vete de mi, cuervo negro" ~?= 1
	]

testsDistEucl = test [
	distEuclideana [3,0] [0,4] ~?= 5,
	distEuclideana [3,0] [4,0] ~?= 1,
	distEuclideana [1.0,0.75,0.8125] [0.75,1.0,0.5] ~?= 0.47186464  
	]

testsDistCos = test [
	distCoseno [3,0] [0,4] ~?= 0,
	distCoseno [3,0] [4,0] ~?= 1,
	distCoseno [0,3,4] [0,-3,-4] ~?= -1 
	]

testsknn = test [
	(knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","f","f","i","f"] distEuclideana) [0,0] ~?= "i",
	(knn 5 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","f","f","i","f"] distEuclideana) [0,0] ~?= "f"
	]

testsAcc = test [
	accuracy ["f", "f", "f", "f"] ["f", "f", "f", "f"] ~?=  1,
	accuracy ["f", "f", "f", "f"] ["i", "i", "f", "f"] ~?=  0.5,
	accuracy ["f", "f", "f", "f"] ["i", "i", "i", "i"] ~?=  0
	]

testsSepDat = test [
	separarDatos (map (\x->[x]) [0..9]) (map (\x-> show x) [0..9]) 5 1 ~?= ([[2.0],[3.0],[4.0],[5.0],[6.0],[7.0],[8.0],[9.0]],[[0.0],[1.0]],["2","3","4","5","6","7","8","9"],["0","1"]),
	separarDatos (map (\x->[x]) [0..9]) (map (\x-> show x) [0..9]) 5 2 ~?= ([[0.0],[1.0],[4.0],[5.0],[6.0],[7.0],[8.0],[9.0]],[[2.0],[3.0]],["0","1","4","5","6","7","8","9"],["2","3"])
	]
