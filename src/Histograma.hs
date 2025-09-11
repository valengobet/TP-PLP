-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Data.List (zipWith4)
import GHC.Arr (accum)
import Util (actualizarElem, infinitoNegativo, infinitoPositivo)

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u) = Histograma l ((u - l) / fromIntegral n) (replicate (n + 2) 0)

-- | Agrega un valor al histograma.
agregar :: Float -> Histograma -> Histograma
agregar x (Histograma l n xs) =
  Histograma l n (actualizarElem indice (+ 1) xs)
  where
    m = length xs
    indice
      | x < l = 0
      | x >= l + fromIntegral (m - 2) * n = m - 1
      | otherwise = 1 + floor ((x - l) / n)

-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma n r xs = foldr agregar (vacio n r) xs

-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p

-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
casilleros :: Histograma -> [Casillero]
casilleros (Histograma l n xs) = zipWith4 (\a b c p -> Casillero a b c p) (limitesInferiores l n xs) (limitesSuperiores l n xs) xs (porcentajes (foldr (+) 0 xs) xs)

limitesSuperiores :: Float -> Float -> [Int] -> [Float]
limitesSuperiores l n xs =
  zipWith
    ( \x i ->
        if i == (length xs - 1)
          then infinitoPositivo
          else l + n * fromIntegral i
    )
    xs
    [0 .. length xs - 1]

limitesInferiores :: Float -> Float -> [Int] -> [Float]
limitesInferiores l n xs =
  zipWith
    ( \x i ->
        if i == 0
          then infinitoNegativo
          else l + (n * fromIntegral (i - 1))
    )
    xs
    [0 .. length xs - 1]

porcentajes :: Int -> [Int] -> [Float]
porcentajes 0 xs = replicate (length xs) 0.0
porcentajes _ [] = []
porcentajes total (x : xs) = (fromIntegral (x * 100) / fromIntegral total) : porcentajes total xs
