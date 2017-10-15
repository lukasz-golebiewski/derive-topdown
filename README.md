# derive-topdown
This is a Haskell project which will derive type class instances from top for a composite data type
### standalone deriving

	{-# LANGUAGE
		TemplateHaskell,
		StandaloneDeriving,
		ConstraintKinds,      
		UndecidableInstances,
		-- You maybe need a lot of other extensions like FlexibleInstances and DerivingStrategies.
		DeriveGeneric
	#-}
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


For empty class instances deriving we can use it in this way. With DeriveAnyClasses and Generic class, we can use standalone deriving to do it. However, this is no reason to prevent you from doing this.

	    instances [''Binary] ''Company
	  ======>
	    instance Binary Gender
	    instance Binary (Person a_af50)
	    instance Binary a_af4Z => Binary (Department a_af4Z)
	    instance Binary a_af4Y => Binary (Company a_af4Y)

For generating instances with a template Haskell function, `derivingTHs` can be used:
	
	   deriving_ths
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
		
You can use this function with `derive`(http://hackage.haskell.org/package/derive) package. It can handle more type classes, like Arbitrary in QuickCheck, especially. 