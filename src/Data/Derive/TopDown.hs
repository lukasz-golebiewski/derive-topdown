{-|

Module      : Data.Derive.TopDown
Description : Help Haskellers derive class instances for composited data types.
Copyright   : (c) songzh
License     : BSD3
Maintainer  : Haskell.Zhang.Song@hotmail.com
Stability   : experimental


@derive-topdown@ will make it easier to derive class instance for complex composited data types by using Template Haskell. For example:

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

'instance_' and 'instances' functions will generate empty instances. It is not quite useful with GHC newer 7.10 since they ahve 'DeriveAnyClass' extension. However, with older GHC, it may help. The useage is the same with 'derive' and 'derives'.

For other classes whose instance can only be generated by using a function @Name -> Q [Dec]@ like 'Arbitrary' in 'QuickCheck', the @derive@ package provides @derive makeArbitrary@ function. For doing the top-down derive in these cases, 'deriving_th' and 'deriving_ths' are defined. Other example can the @deriveXXXX@ functions in @Data.Aeson.TH@.

> deriving_th (''FromJSON, deriveFromJSON defaultOptions) ''Company
> deriving_th (''ToJSON, deriveToJSON defaultOptions)     ''Company

However, the poblem could be that the instance context is generated by the template haskell derive function instead mine, so it could be wrong in some circumtances. For example, type with high order type constructor:

> data T1 k a b = T11 (k a) b | T12 (k (k b)) a b Int

It cannot be derived 'FromJSON' and 'ToJSON' with 'deriveFromJSON' and 'deriveToJSON' since it does not generate @(k a)@ and @(k (k b))@ for in the instance context.

Also, there are some data types which are impossible to be derived as instances of a certain type class. For example, Word cannot be derived as Functor or Generic. Using @derive-topdown@ is the same with hand-written code, so it is your responsiblity to make that right.

* __NOTE!__ @derive-topdown@ will __NOT__ work with 'Typeable' type class. See [here](https://ghc.haskell.org/trac/ghc/ticket/11251).

-}

module Data.Derive.TopDown (
  module Data.Derive.TopDown.Standalone,
  module Data.Derive.TopDown.Instance,
  module Data.Derive.TopDown.TH
#if __GLASGOW_HASKELL__ >= 802
   ,DerivStrategy(StockStrategy,AnyclassStrategy,NewtypeStrategy)
#endif
  )
where
#if __GLASGOW_HASKELL__ >= 802
import Language.Haskell.TH.Syntax
#endif
import Data.Derive.TopDown.Standalone
import Data.Derive.TopDown.Instance
import Data.Derive.TopDown.TH
