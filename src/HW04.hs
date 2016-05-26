{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Control.Monad
import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P left) == (P right) = leftCoef == rightCoef
      where leftCoef = reverse . trimLeadingZeros . reverse $ left
            rightCoef = reverse . trimLeadingZeros . reverse $ right

trimLeadingZeros :: (Num a, Eq a) => [a] -> [a]
trimLeadingZeros (0:xs) = trimLeadingZeros xs
trimLeadingZeros p = p

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P xs) =
      let terms = reverse $ intersperse " + " (show' xs 0)
      in
        case terms of
          [] -> "0"
          _ -> join $ terms

show' :: (Num a, Eq a, Show a) => [a] -> Int -> [String]
show' [] _ = []
show' (y:ys) degree =
  let others = show' ys (degree + 1) in
  case showTerm y degree of
    Nothing -> others
    Just term -> term:others

showTerm :: (Num a, Eq a, Show a) => a -> Int -> Maybe String
showTerm 0 _ = Nothing
showTerm coeff 0 = Just $ showCoeff coeff 0
showTerm coeff 1 = Just $ showCoeff coeff 0 ++ "x"
showTerm coeff degree =
  Just $ (showCoeff coeff degree) ++ "x^" ++ (show degree)

showCoeff :: (Num a, Eq a, Show a) => a -> Int -> String
showCoeff coeff 0 = show coeff
showCoeff 1 _ = ""
showCoeff coeff _ = show coeff

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P ys) (P zs) = P $ plus' ys zs

plus' :: Num a => [a] -> [a] -> [a]
plus' [] zs = zs
plus' ys [] = ys
plus' (y:ys) (z:zs) = (y + z) : plus' ys zs

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P []) _ = P []
times _ (P []) = P []
times (P ys) (P zs) =
  let yis = zip ys [0..]
      terms = do
                (y, degree) <- yis
                let zeros = replicate degree 0
                return $ P $ zeros ++ (map (* y) zs)
  in
    foldr (+) (P [0]) terms

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P ys) = P $ map (* (-1)) ys
    fromInteger n = P $ [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P ys) v = foldr (+) 0 $ zipWith (*) ys vs
  where vs = iterate (*v) 1

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n y = (iterate deriv y) !! n

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P []) = (P [])
    deriv (P (y:ys)) = P (zipWith (*) ys naturals)

naturals :: Num a => [a]
naturals = iterate (+1) 1


