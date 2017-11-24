{-# LANGUAGE StandaloneDeriving , TemplateHaskell, DeriveGeneric, DeriveDataTypeable ,GADTs,DataKinds,ConstraintKinds,UndecidableInstances, TypeOperators,RankNTypes,GeneralizedNewtypeDeriving,DeriveAnyClass, DeriveLift,CPP#-}

#if __GLASGOW_HASKELL__ >= 802
{-# LANGUAGE DerivingStrategies #-}
#endif  

{-# OPTIONS_GHC -ddump-splices #-}
module Main where

import Data.Derive.TopDown
import GHC.Generics
import Data.Typeable
import Data.Data
import Data.DeriveTH
import Test.QuickCheck
import Data.Binary
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (Module)
import Language.Haskell.Syntax
import Data.Ratio
import Text.Show.Functions
import Language.SQL.SimpleSQL.Syntax
import Data.Word
import Text.PrettyPrint.GenericPretty

-- Test for deriving strategy
newtype A = A (Int,B)
newtype B = B1 String

#if __GLASGOW_HASKELL__ >= 802
strategy_deriving newtype_ ''Show ''A
strategy_deriving newtype_ ''Binary ''A
#endif

-- Simple cases
type Age = Int

data Person = Person {pname :: String, age :: Age}

data Department = Department {head :: Person, staffs :: [Person]}

data Company = Company {manager :: Person , departments :: [Department]}

-- error test
-- deriving_ ''Eq ''Age

deriving_ ''Eq ''Company

derivings [''Ord] ''Company

derivings [''Typeable] ''Company

derivingss [''Show, ''Generic] [''Company]

-- Test Lift class
deriving_ ''Lift ''Company

data Corp = Corp {ceo :: Person, comapany :: Company}

deriving_with_breaks ''Typeable ''Corp [''Company, ''Person]


-- GADT Test
data Aggregator = SUM | AVG | MIN | MAX | CNT | CNTD | ATTR deriving Enum

data Granularity = G0 | G1 deriving Enum

data DataQL a where
  DimRaw :: String -> DataQL G0
  MsrRaw :: String -> DataQL G0
  Agg :: Aggregator -> DataQL G0 -> DataQL G1
  Include :: [DataQL G0] -> DataQL G1 -> DataQL G0
  Exclude :: [DataQL G0] -> DataQL G1 -> DataQL G0
  Fixed   :: [DataQL G0] -> DataQL G1 -> DataQL G0

-- deriving_with_breaks ''Show ''DataQL [''G0]

-- Only for GHC 8.2. Some problems are with GHC 8.0
#if __GLASGOW_HASKELL__ >= 802
deriving_ ''Show ''DataQL
#endif
-- Data types with higher kinds
data T1 k a b = T11 (k a) b | T12 (k (k a)) a b String

derivings [''Show] ''T1

data T2 k a b = T21 {n1 :: (k a) , b1 :: b} | T22 {n2 :: (k (k b)) , a2 :: a ,b2 :: b}

derivings [''Show,''Eq,''Ord] ''T2

data a :.. b = Product a b

derivings [''Show] ''(:..)

data GadtForall where
    GFT1 :: Show a => a -> GadtForall

deriving_ ''Show ''GadtForall

data T4 k a b where
  T31 :: a -> k b -> T4 k a b

deriving_ ''Show ''T4

-- deriving_th
deriving_th (''Arbitrary, derive makeArbitrary) ''Company


-- Cannot do it
-- deriving_th (''Arbitrary, derive makeArbitrary) ''DataQL


-- This is caused by https://ghc.haskell.org/trac/ghc/ticket/10512
-- some primitive types are not Generic, in order to prevent genStandaloneDerivingDecl from generating Generic for Int,
-- I used primitive package. However, it is appearent not enough. Ratio a is an example. I should write a package to do this generic this. 
-- instance Generic (Ratio a)
--deriving_ ''Generic ''HsModule
deriving_with_breaks ''Generic ''HsModule [''Ratio]

deriving_th (''Arbitrary, derive makeArbitrary) ''HsModule

-- random Haskell code generation


-- Test derive-topdown with Info type in TH
#if __GLASGOW_HASKELL__ >= 802
strategy_deriving newtype_ ''Binary ''OccName
strategy_deriving anyclass ''Binary ''ModName
strategy_deriving newtype_ ''Binary ''PkgName
strategy_derivings anyclass [''Binary] ''Info
#else
deriving_ ''Binary ''Info
#endif
-- deriving_ ''Binary ''Info

-- Forall test
data TForall a = TF (forall b. Show b => b) a

-- Not possible to derive or declare instances
-- derivings [''Show] ''TForall
-- deriving instance Show a => Show (TForall a)
-- instance Show a => Show (TForall a) where
--  show (TF b a) = show b ++ show a

data TForall2 a = TF1 (forall b . b -> b) a

deriving_ ''Show ''TForall2

-- Phantom
data P1 a = P1C1 (P2 a)
data P2 b = P2C1 Int

deriving_ ''Show ''P1

-- deriving `data` keyword defined type with newtype. If the type it composed with used newtype, then it will be derived with newtype.
data P3 a = P31C1 (NP4 Int)
newtype  NP4 b = NP4C b

#if __GLASGOW_HASKELL__ >= 802
strategy_deriving newtype_ ''Show ''P3
#else
deriving_ ''Show ''P3
#endif

data T = T Word8

deriving_ ''Typeable ''Word8

derivings [''Out, ''Generic] ''QueryExpr


main = putStrLn "Test passed"

