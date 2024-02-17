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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import Prelude
import Data.Singletons
import Data.Singletons.TH
import GHC.Natural (Natural)
import Data.Kind (Constraint)
import GHC.Base (Type)

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

type LEProof :: Nat -> Nat -> Type
data LEProof n m where
    LEZero ::  LEProof Zero n
    LESucc :: LEProof n m -> LEProof (Succ n) (Succ m)

deriving instance Show (LEProof n m)
deriving instance Eq (LEProof n m)

class LE n m where
    leProof :: LEProof n m

type LE :: Nat -> Nat -> Constraint
instance LE Zero n where
    leProof :: LEProof 'Zero n
    leProof = LEZero

-- | This class will automatically search for a proof of n < m
-- using GHC's solver. This is kind of an automated proof search.
instance (LE n m) => LE (Succ n) (Succ m) where
    leProof :: LE n m => LEProof ('Succ n) ('Succ m)
    leProof = LESucc (leProof @n @m)

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

-- ok type erased
testF1 :: Proxy (Pow N2 N12) -> Int
testF1 _ = 3

-- ok type erased
testF2 :: SNat (Pow N2 N12) -> Int
testF2 _ = 3

-- not ok
-- Error: cabal: Failed to build exe:prog from inductiveNatTest-0.1.0.0. The
-- build process was killed (i.e. SIGKILL). The typical reason for this is that
-- there is not enough memory available (e.g. the OS killed a process using lots
-- of memory).
-- testF3 :: SNat (Pow N2 N12) -> Int
-- testF3 = fromIntegral . toNatural . fromSing

-- not ok
-- ghc blows up
--testF4 :: (LE n (Pow N2 N12)) => SNat n -> Int
--testF4 = undefined

main :: IO ()
main = print (natKindToIntegral @(Plus N1 N1) :: Int)
