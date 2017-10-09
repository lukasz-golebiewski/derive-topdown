{-# LANGUAGE StandaloneDeriving,ConstraintKinds,UndecidableInstances,GADTs,DeriveDataTypeable,DeriveGeneric #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Data.Derive.TopDown.Test where

import Data.Derive.TopDown
import Data.Data
import GHC.Generics

-- testing class contex generation
data T1 k a b = T11 (k a) b | T12 (k (k b)) a b Int

derivings [''Eq] ''T1

data T2 = T21 String

derivings [''Eq] ''T2

data T3 k a b = T31 {n1 :: (k a) , b1 :: b} | T32 { n2 :: (k (k b)) , a2 :: a, b2 :: b}

derivings [''Eq] ''T3

-- GADT test

data T4 k a b where
  T41 :: a -> b -> T4 k a b

derivings [''Eq, ''Ord, ''Generic] ''T4

---- topdown generation test
data Gender = Male | Female
type Age = Int
data Person a = P {name :: String , age :: Int, gender :: Gender}
data Department a = D {dname :: String , head :: Person a, staff :: [Person a]}
data Company a = C {cname :: String, departments :: [Department a]}

derivings [''Eq, ''Ord, ''Generic] ''Company

