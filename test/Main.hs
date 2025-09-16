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
    [ -- "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      -- "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      -- "Ej 3 - Histograma.vacio" ~: testsVacio,
      -- "Ej 4 - Histograma.agregar" ~: testsAgregar,
      -- "Ej 5 - Histograma.histograma" ~: testsHistograma
      -- "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      -- "Ej 7 - Expr.recrExpr" ~: testsRecr,
      -- "Ej 7 - Expr.foldExpr" ~: testsFold,
      -- "Ej 8 - Expr.eval" ~: testsEval,
      -- "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      -- "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      -- "Ej 11 - Expr.mostrar" ~: testsMostrar,
      -- "Expr.Parser.parse" ~: testsParse,
      -- "App.mostrarFloat" ~: testsMostrarFloat,
      -- "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      alinearDerecha 8 "Haskell" ~?= " Haskell", -- un espacio
      alinearDerecha 5 "AB" ~?= "   AB",         -- varios espacios
      alinearDerecha 3 "" ~?= "   ",             -- string vacío pero largo
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      actualizarElem 0 (const 99) [7, 8, 9] ~?= [99,8,9], -- reemplazo directo
      actualizarElem 1 (subtract 5) [10, 20, 30] ~?= [10,15,30], 
      actualizarElem 4 (*10) [1..4] ~?= [1,2,3,4],       -- índice fuera
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
            ],

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
                ],
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
        ~?= agregar 10 (agregar 3.9 (agregar 1.5 (agregar 0 (agregar (-2) (vacio 2 (0, 4)))))),

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
            ],
    ]


testsRecr :: Test
testsRecr =
  let contarNodos = recrExpr (\_ rs -> 1 + sum rs)
   in test
        [ contarNodos (Const 5) ~?= 1,
          contarNodos (Suma (Const 1) (Const 2)) ~?= 3,
          contarNodos (Mult (Const 1) (Suma (Const 2) (Const 3))) ~?= 5
        ]

testsFold :: Test
testsFold =
  let sumarConsts = foldExpr id (\a b -> a + b)
   in test
        [ sumarConsts (Const 5) ~?= 5,
          sumarConsts (Suma (Const 2) (Const 3)) ~?= 5,
          sumarConsts (Mult (Suma (Const 1) (Const 2)) (Const 10)) ~?= 13
        ]


testsEval :: Test
testsEval =
  test
    [ fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
      fst (eval (Mult (Const 2) (Const 5)) genFijo) ~?= 10.0,
      fst (eval (Div (Const 10) (Const 2)) genFijo) ~?= 5.0,
      fst (eval (Resta (Const 10) (Const 3)) genFijo) ~?= 7.0
    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  let h = fst (armarHistograma 2 4 (dameUno (10,10)) genFijo) -- siempre devuelve 10
   in test
        [ sum (map casCantidad (casilleros h)) ~?= 4, -- cuatro muestras
          length (casilleros h) ~?= 4,                -- 2 finitos + extremos
          maximum (map casCantidad (casilleros h)) ~?= 4, -- todo cae en el mismo casillero
        ]


testsEvalHistograma :: Test
testsEvalHistograma =
  let h = fst (evalHistograma 3 6 (Const 2) genFijo)
   in test
        [ sum (map casCantidad (casilleros h)) ~?= 6, -- seis muestras
          length (casilleros h) ~?= 5,                -- 3 finitos + extremos
          all (>0) (map casCantidad (tail (init (casilleros h)))), -- algo cayó en los finitos
        ]


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
