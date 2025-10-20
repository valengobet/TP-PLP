module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      alinearDerecha 8 "Haskell" ~?= " Haskell", -- un espacio
      alinearDerecha 5 "AB" ~?= "   AB", -- varios espacios
      alinearDerecha 3 "" ~?= "   " -- string vacío pero largo
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      actualizarElem 0 (const 99) [7, 8, 9] ~?= [99, 8, 9], -- reemplazo directo
      actualizarElem 1 (subtract 5) [10, 20, 30] ~?= [10, 15, 30],
      actualizarElem 4 (* 10) [1 .. 4] ~?= [1, 2, 3, 4] -- índice fuera
    ]

testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],
      -- Caso con 2 casilleros en un rango más grande
      casilleros (vacio 2 (0, 20))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 20 0 0,
              Casillero 20 infinitoPositivo 0 0
            ],
      -- Caso con 4 casilleros en rango negativo
      casilleros (vacio 4 (-8, 0))
        ~?= [ Casillero infinitoNegativo (-8) 0 0,
              Casillero (-8) (-6) 0 0,
              Casillero (-6) (-4) 0 0,
              Casillero (-4) (-2) 0 0,
              Casillero (-2) 0 0 0,
              Casillero 0 infinitoPositivo 0 0
            ],
      -- Caso con rango que no empieza en 0
      casilleros (vacio 2 (5, 9))
        ~?= [ Casillero infinitoNegativo 5 0 0,
              Casillero 5 7 0 0,
              Casillero 7 9 0 0,
              Casillero 9 infinitoPositivo 0 0
            ]
    ]

testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6) -- casilleros: (-∞,0), [0,2), [2,4), [4,6), [6,+∞)
   in test
        [ -- Valor en el límite inferior del rango: cae en [0,2)
          casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          -- Valor en el segundo casillero finito [2,4)
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          -- Valor menor al rango: cae en -∞
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          -- Valor dentro del último casillero finito [4,6)
          casilleros (agregar 5.5 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 1 100,
                  Casillero 6 infinitoPositivo 0 0
                ],
          -- Valor mayor al rango: cae en +∞
          casilleros (agregar 10 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 1 100
                ]
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ -- Caso del enunciado: los tres valores caen en casilleros dentro del rango
      histograma 4 (1, 5) [1, 2, 3]
        ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),
      -- Lista vacía: todos los casilleros vacíos
      histograma 3 (0, 6) []
        ~?= vacio 3 (0, 6),
      -- Valores fuera del rango: todos en -∞ o +∞
      histograma 2 (0, 10) [-5, -1, 15, 20]
        ~?= agregar 20 (agregar 15 (agregar (-1) (agregar (-5) (vacio 2 (0, 10))))),
      -- Mezcla de valores dentro y fuera del rango
      histograma 2 (0, 4) [-2, 0, 1.5, 3.9, 10]
        ~?= agregar 10 (agregar 3.9 (agregar 1.5 (agregar 0 (agregar (-2) (vacio 2 (0, 4))))))
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ -- Histograma vacío: todos los casilleros con 0
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      -- Un valor en el casillero [2,4)
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      -- Dos valores, repartidos en casilleros diferentes
      casilleros (agregar 1 (agregar 5 (vacio 3 (0, 6))))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 1 50.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 1 50.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      -- Valores fuera del rango (uno a -∞ y otro a +∞)
      casilleros (agregar (-1) (agregar 10 (vacio 3 (0, 6))))
        ~?= [ Casillero infinitoNegativo 0.0 1 50.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 1 50.0
            ]
    ]

testsRecr :: Test
testsRecr =
  let contarNodos =
        recrExpr
          (\_ -> 1)
          (\_ _ -> 1)
          (\_ r1 _ r2 -> 1 + r1 + r2)
          (\_ r1 _ r2 -> 1 + r1 + r2)
          (\_ r1 _ r2 -> 1 + r1 + r2)
          (\_ r1 _ r2 -> 1 + r1 + r2)
   in test
        [ contarNodos (Const 5) ~?= 1,
          contarNodos (Suma (Const 1) (Const 2)) ~?= 3,
          contarNodos (Mult (Const 1) (Suma (Const 2) (Const 3))) ~?= 5
        ]

testsFold :: Test
testsFold =
  let sumarConsts =
        foldExpr
          id
          (\a b -> a + b)
          (+)
          (+)
          (+)
          (+)
   in test
        [ sumarConsts (Const 5) ~?= 5,
          sumarConsts (Suma (Const 2) (Const 3)) ~?= 5,
          sumarConsts (Mult (Suma (Const 1) (Const 2)) (Const 10)) ~?= 13
        ]

testsEval :: Test
testsEval =
  test
    [ 
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      fst (eval (Suma (Const 4) (Const 1)) (genNormalConSemilla 0)) ~?= 5.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
      fst (eval (Mult (Const 2) (Const 5)) genFijo) ~?= 10.0,
      fst (eval (Mult (Rango 1 2) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 4.5297704,
      fst (eval (Div (Const 10) (Const 2)) genFijo) ~?= 5.0,
      fst (eval (Div (Rango 1 10) (Rango 1 2)) (genNormalConSemilla 0)) ~?= 3.2950761,
      fst (eval (Resta (Const 10) (Const 3)) genFijo) ~?= 7.0,
      fst (eval (Resta (Rango 1 10) (Rango 1 3)) (genNormalConSemilla 0)) ~?= 2.9830952,
      fst (eval (Suma (Mult (Rango 1 2) (Rango 1 5)) (Resta (Rango 1 10) (Rango 1 3)))(genNormalConSemilla 0)) ~?= 13.310371
    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  test
        [ sum (map casCantidad c1) ~?= 4, -- cuatro muestras
          length c1 ~?= 4, -- 2 finitos + extremos
          maximum (map casCantidad c1) ~?= 4, -- todo cae en el mismo casillero
          map casCantidad c1 ~?= [0,0,4,0], 
          ---------------------------------------------------------------------------------------- Caso 2
          sum (map casCantidad c2) ~?= 5, -- cinco muestras
          length c2 ~?= 5, -- 2 finitos + extremos
          maximum (map casCantidad c2) ~?= 3, -- el casillero con mayor casos es el del medio
          map casCantidad c2 ~?= [0,1,3,1,0], -- todo cae dentro del rango
          ---------------------------------------------------------------------------------------- Caso 3
          sum (map casCantidad c3) ~?= 100, -- cien muestras
          length c3 ~?= 5, -- 2 finitos + extremos
          maximum (map casCantidad c3) ~?= 46,  -- el casillero con mayor casos es el del medio
          map casCantidad  c3 ~?= [1,27,46,24,2] -- tenemos que caen fuera del rango
        ]
  where
    c1 = casilleros (fst (armarHistograma 2 4 (dameUno (10, 10)) genFijo))
    c2 = casilleros (fst (armarHistograma 3 5 (dameUno (1, 10)) (genNormalConSemilla 0)))
    c3 = casilleros (fst (armarHistograma 3 100 (dameUno (1, 100)) (genNormalConSemilla 0)))
        

testsEvalHistograma :: Test
testsEvalHistograma =
  test
        [ sum (map casCantidad c1) ~?= 6, -- seis muestras
          length c1 ~?= 5, -- 3 finitos + extremos
          all ((> 0) . casCantidad) (tail (init c1)) ~?= True, -- algo cayó en los finitos
          casCantidad (head c1) > 0 ~?= False, -- nada cayó en los -infinito
          casCantidad (last c1) > 0 ~?= False, -- nada cayó en los +infinito  
          ---------------------------------------------------------------------------------------- Caso 2
          sum (map casCantidad c2) ~?= 5, -- cinco muestras
          length c2 ~?= 5, -- 3 finitos + extremos
          all ((> 0) . casCantidad) (tail (init c2)) ~?= False, -- nada cayó en los finitos
          casCantidad (head c2) > 0 ~?= False, -- nada cayó en los -infinito
          casCantidad (last c2) > 0 ~?= True, -- algo cayó en los +infinito  
          ---------------------------------------------------------------------------------------- Caso 3
          sum (map casCantidad c3) ~?= 100, -- cien muestras
          length c3 ~?= 7, -- 5 finitos + extremos
          all ((> 0) . casCantidad) (tail (init c3)) ~?= True, -- algo cayó en los finitos
          casCantidad (head c3) > 0 ~?= True, -- algo cayó en los -infinito
          casCantidad (last c3) > 0 ~?= True -- algo cayó en los +infinito

        ]
  where
    c1 = casilleros (histograma 3 (3, 6) [3.1, 3.5, 4.9, 5.2, 3.8, 4.1])
    c2 = casilleros (histograma 3 (1, 10) [5.0456104,5.781319,11.044029,6.6854277,2.1621087])
    c3 = casilleros (histograma 5 (1, 100) [45.501717,53.59451,111.48433,63.539703,13.7831955,64.872116,86.615265,86.44969,13.265369,10.532959,75.42795,27.453726,47.60898,68.85931,58.537766,24.811525,72.2526,50.3162,72.52614,22.280722,35.06108,63.61824,54.711525,55.383976,83.98961,65.36496,54.137203,70.30834,60.301933,63.22765,72.91609,22.67203,75.55327,32.6649,73.041176,46.792557,61.542305,26.72534,52.592934,90.36452,71.682274,35.293846,77.70097,21.917797,54.306896,93.21026,55.236195,94.413376,40.84056,72.50311,79.348206,62.428783,27.32771,56.08609,68.74971,81.5275,33.228027,51.265343,37.590656,24.146284,70.33746,59.489983,66.27703,23.797844,45.445568,-10.730633,49.82262,70.28327,49.720715,33.206406,59.64248,67.36187,27.97368,62.245274,23.614923,15.639008,41.43367,57.68116,85.21019,27.133512,55.427933,38.608833,45.2938,65.92268,43.467045,97.29263,95.24162,70.78,70.48694,63.09217,81.36019,29.611803,37.329765,87.872116,77.16374,15.447952,51.863373,10.897507,89.55332,102.77849])

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar =
  test
    [ mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
