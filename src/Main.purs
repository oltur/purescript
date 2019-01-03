module Main where

-- import Math (sqrt)
import Data.Array
import Prelude

import Control.MonadZero (guard)
import Control.Plus (empty)
import Data.Array as Array
import Data.Array.NonEmpty (dropEnd)
import Data.Array.Partial (head, tail)
import Data.Foldable (product)
import Data.List.NonEmpty (length)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Math (pi, sqrt)
import Partial.Unsafe (unsafePartial)

infix 8 filter as <$?>


isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n `mod` 2)

countEvens :: Array Int -> Int
countEvens arr =
  if null arr
    then 0
    else
      if isEven(unsafePartial(head arr))
      then 1 + countEvens(unsafePartial (tail arr))
      else 0 + countEvens(unsafePartial (tail arr))

makeSquares :: forall a. (Semiring a) => Array a -> Array a
makeSquares arr =
  (\n -> n * n) <$> arr

removeOdds :: Array Int -> Array Int
removeOdds arr =
  (not isEven) <$?> arr

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = do
  let f = factors n
  let t2 = if Array.length(f) < 2  then true else false
  t2

cartesian :: forall a. Array a -> Array a -> Array (Array a)
cartesian x y =
  concatMap (\i -> map (\j -> [i, j]) x) y

cartesian3 :: forall a. Array a -> Array a -> Array a -> Array( Array (Array a))
cartesian3 x y z =
  concatMap (\i -> map (\j -> map(\k -> [i, j, k]) z) y) x

triples :: Int -> Array (Array Int)
triples n = do
  i <- 1 .. n
  j <- 1 .. n
  k <- 1 .. n
  guard $ i*i + j*j == k*k
  pure [i, j, k]

factorizations' :: Int -> Int -> Int -> Array Int -> Array Int
factorizations' n n' lastFactor factorsArray = do
  i <- lastFactor .. (n+1)
  guard $ n' `mod` i == 0
  j <- snoc factorsArray i
  k <- factorizations' n (n/i) i j
  pure [k]

factorizations :: Int -> Array Int
factorizations 1 = [1]
factorizations n = do
  pure [factorizations' n, n, 0, []]

main :: Effect Unit
main = do
  -- logShow(isEven 901)
  let arr = 1..5
  let arr2 = 10..15
  let arr3 = 20..25
  -- logShow(countEvens arr)
  -- logShow ((\n -> isEven(n)) <$?> ((\n -> n + 1) <$> arr))

  let result = triples 20

  logShow result