# derive-topdown
[![Hackage version](https://img.shields.io/hackage/v/derive-topdown.svg?label=Hackage)](https://hackage.haskell.org/package/derive-package) [![Stackage version](https://www.stackage.org/package/derive-topdown/badge/lts?label=Stackage)](https://www.stackage.org/package/derive-topdown)

This is a Haskell project which will derive type class instances from top for a composite data type


### 1. Standalone deriving
There are functions named `deriving_`, `derivings`, `derivingss`. Please see the API for their types.

	{-# LANGUAGE StandaloneDeriving,
		ConstraintKinds,
		UndecidableInstances,
		GADTs,
		TemplateHaskell,
		DeriveGeneric #-}
	{-# OPTIONS_GHC -ddump-splices #-}
	
	import Data.Derive.TopDown
	import GHC.Generics
	import Data.Binary
	imoprt Data.Aeson
	import Data.Aeson.TH

	data Gender = Male | Female
	type Age = Int
	data Person a = P {name :: String , age :: Int, gender :: Gender}
	data Department a = D {dname :: String , 
						   head :: Person a, 
						   staff :: [Person a]}
	data Company a = C {cname :: String, 
	                    departments :: [Department a]}
	
	derivings [''Eq, ''Ord, ''Generic] ''Company

You will get:

		derivings [''Eq, ''Ord, ''Generic] ''Company
	  ======>
	    deriving instance Eq Gender
	    deriving instance Eq (Person a_acKV)
	    deriving instance Eq a_acKU => Eq (Department a_acKU)
	    deriving instance Eq a_acKT => Eq (Company a_acKT)
	    deriving instance Ord Gender
	    deriving instance Ord (Person a_acKV)
	    deriving instance Ord a_acKU => Ord (Department a_acKU)
	    deriving instance Ord a_acKT => Ord (Company a_acKT)
	    deriving instance Generic Gender
	    deriving instance Generic (Person a_acKV)
	    deriving instance Generic (Department a_acKU)
	    deriving instance Generic (Company a_acKT)

### 2. Empty Instances generation

For empty class instances deriving, `instance_`, `instances`, `instancess` are provided. We can use it in this way.

	    instances [''Binary] ''Company
	  ======>
	    instance Binary Gender
	    instance Binary (Person a_af50)
	    instance Binary a_af4Z => Binary (Department a_af4Z)
	    instance Binary a_af4Y => Binary (Company a_af4Y)

### 3. Usage with Template Haskell
For generating instances with a template Haskell function, `derivingTH`, `derivingTHs` and `derivingTHss` can be used:
	
	   derivingTHs
	      [(''ToJSON, deriveToJSON defaultOptions),
	       (''FromJSON, deriveFromJSON defaultOptions)]
	      ''Company
	  ======>
	    instance ToJSON Gender where
	      toJSON
	        = \ value_amQG
	            -> case value_amQG of {
	                 Male -> String (text-1.2.2.2:Data.Text.pack "Male")
	                 Female -> String (text-1.2.2.2:Data.Text.pack "Female") }
	      toEncoding
	        = \ value_amQH
	            -> case value_amQH of {
	                 Male
	                   -> Data.Aeson.Encoding.Internal.text
	                        (text-1.2.2.2:Data.Text.pack "Male")
	                 Female
	                   -> Data.Aeson.Encoding.Internal.text
	                        (text-1.2.2.2:Data.Text.pack "Female") }
	    instance ToJSON a_amqg => ToJSON (Person a_amqg) where
	      toJSON
	        = \ value_amQy
	        ...
	        ...
You can use this this function with `derive`(http://hackage.haskell.org/package/derive) package.

### 4. Deriving with strategies in GHC 8.2
`strategy_deriving`, `strategy_derivings` and `strategy_derivingss` can be used.
The 3 strategies for deriving`StockStrategy`,`AnyclassStrategy`,`NewtypeStrategy` are exposed when you import `TopDown`

#### **NOTE**:  About deriving instances of Typeable
There is a bug with `isInstance` function when working with Typeable class. See (https://ghc.haskell.org/trac/ghc/ticket/11251). So I use Data type class to replace Typeable when using `isInstance`. This means that  if you used a data type from other library or module, it is an instance of `Typeable` but not an instance of `Data`, there might be errors when you try to derive `Typeable` in this top-down manner.

#### **NOTE**: You cannot derive a type synonym. It will not work with `-XTypeSynonymInstances` language extension. The top node in the data declaration tree has to be a data or newtype.

More discussion please see https://ghc.haskell.org/trac/ghc/ticket/10607