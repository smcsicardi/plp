-- Para correr los tests:
-- ghc Tests-alu.hs && ./Tests-alu

import Tp
import Test.HUnit
import Data.List

-- evaluar main para correr todos los tests
main = runTestTT allTests

allTests = test [
 	"split" ~: testsSplit,
 	"cuentas" ~: testsCuentas
 	]

testsSplit = test [
 	split ',' ",PLP," ~?= ["PLP"],
 	split ',' " ,PLP, " ~?= [" ","PLP"," "]
  	]

testsCuentas = test [
	cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")]
	]

testsLongPromPal = test [
	longitudPromedioPalabras "vete de mi, cuervo negro" ~?= 4
	]

testsRepProm = test [
	repeticionesPromedio "xyz" ~?= 1
	repeticionesPromedio "xxyxz" ~?= 5/3
	repeticionesPromedio "spinetta" ~?= 8/7
	]

testsFrecTok = test [
]

testsNormExt = test [
	(normalizarExtractor ["xyz","xxyxz","spinetta"] repeticionesPromedio) "xyz" ~?= 3/5
]

testsDistEucl = test [
	distEuclideana [3,0] [0,4] ~?= 5
]
