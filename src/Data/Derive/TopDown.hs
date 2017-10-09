{-|
> {-# LANGUAGE TemplateHaskell #-}
> {-# OPTIONS_GHC -ddump-splices #-}

> data Gender = Male | Female
> type Age = Int
> data Person a = P {name :: String , age :: Int, gender :: Gender}
> data Department a = D {dname :: String , head :: Person a, staff :: [Person a]}
> data Company a = C {cname :: String, departments :: [Department a]}

> derivings [''Eq, ''Ord, ''Generic] ''Company

You will get:

>     derivings [''Eq, ''Ord, ''Generic] ''Company
>   ======>
>     deriving instance Eq Gender
>     deriving instance Eq (Person a_adHv)
>     deriving instance (Eq a_adHu, Eq a_adHu) => Eq (Department a_adHu)
>     deriving instance Eq a_adHt => Eq (Company a_adHt)
>     deriving instance Ord Gender
>     deriving instance Ord (Person a_adHv)
>     deriving instance (Ord a_adHu, Ord a_adHu) =>
>                       Ord (Department a_adHu)
>     deriving instance Ord a_adHt => Ord (Company a_adHt)
>     deriving instance Generic Gender
>     deriving instance Generic (Person a_adHv)
>     deriving instance Generic (Department a_adHu)
>     deriving instance Generic (Company a_adHt)


This will make sense if you have a deep composited data types, nomally an AST of a language.
'instance_' and 'instances' function will just generate empty instances. But when GHC7.10 or newer versions have 'DeriveAnyClasses' language extension, it is not quite useful. 
-}

module Data.Derive.TopDown (
  module Data.Derive.TopDown.Standalone,
  module Data.Derive.TopDown.Instance)
where

import Data.Derive.TopDown.Standalone
import Data.Derive.TopDown.Instance
