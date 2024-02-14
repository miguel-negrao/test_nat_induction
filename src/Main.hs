{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Prelude
import Data.Singletons
import Data.Singletons.TH
import GHC.Natural (Natural)

$(singletons [d|
    data Nat = Zero | Succ Nat

    plus :: Nat -> Nat -> Nat
    plus Zero m = m
    plus (Succ n) m = Succ (plus n m)

    pred :: Nat -> Nat
    pred Zero = Zero
    pred (Succ n) = n

    minus :: Nat -> Nat -> Nat
    minus a Zero = a
    minus a (Succ b) = pred (minus a b)

    mult :: Nat -> Nat -> Nat
    mult  _ Zero = Zero
    mult a (Succ b) = plus (mult a b) a
    
    pow :: Nat -> Nat -> Nat
    pow _ Zero = Succ Zero
    pow n (Succ m) = mult n (pow n m)

    min :: Nat -> Nat -> Nat
    min Zero _ = Zero
    min (Succ _) Zero = Zero
    min (Succ n) (Succ m) = Succ (min n m)

    max :: Nat -> Nat -> Nat
    max Zero m = m
    max n@(Succ _) Zero = n
    max (Succ n) (Succ m) = Succ (max n m) 
    |])

type N0 = Zero
type N1 = Succ Zero
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4
type N6 = Succ N5
type N7 = Succ N6
type N8 = Succ N7
type N9 = Succ N8
type N10 = Succ N9
type N11 = Succ N10
type N12 = Succ N11
type N13 = Succ N12
type N14 = Succ N13
type N15 = Succ N14
type N16 = Succ N15

type family (-) a b where
  a - b = Minus a b

type family (+) a b where
  a + b = Plus a b

type family (*) a b where
  a * b = Mult a b

deriving instance Show Nat
deriving instance Eq Nat
deriving instance Show (SNat n)
deriving instance Eq (SNat n)

toNatural :: Nat -> Natural
toNatural Zero = 0
toNatural (Succ n) = toNatural n + 1

-- |
-- Convert a type level natural to an integral
natKindToIntegral:: forall (n :: Nat) a . (Integral a, SingI n) => a  
natKindToIntegral = fromIntegral $ toNatural $ fromSing $ sing @n

-- time cabal run

-- Pow N2 N7                real    0m0,117s
-- Pow N2 N8                real    0m45,997s (256)
-- Mult N2 N8               real    0m1,567s
-- Mult N10 N10             real    0m1,817s
-- Mult N2 (Mult N10 N10)   real    0m20,147s (200) 
-- Plus N16 N16             real    0m1,276s (32)

main :: IO ()
main = print (natKindToIntegral @(Plus N16 N16) :: Int)
