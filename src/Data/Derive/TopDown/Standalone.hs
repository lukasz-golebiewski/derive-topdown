{-# LANGUAGE TemplateHaskell #-}
module Data.Derive.TopDown.Standalone (deriving_, derivings, derivingss) where

import Data.Derive.TopDown.Lib
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import qualified GHC.Generics as G
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Data.List (foldl')
import Data.Primitive.Types

genStandaloneDerivingDecl :: ClassName -> TypeName -> StateT [Type] Q [Dec]
genStandaloneDerivingDecl cn tn = do
                   (tvbs,cons) <- lift $ getTyVarCons cn tn
                   classContext <- lift $ generateClassContext cn tn
                   let typeNames = map getTVBName tvbs
                   instanceType <- lift $ foldl' appT (conT tn) $ map varT typeNames
                    -- Stop generating further instances
                   -- 1. it is already a member of that type class
                   -- 2. we have already generated it, which is kind of same with case 1
                   -- 3. for GHC.Generic, if it is a primitive type like Int, Double
                   isMember <- lift $ isInstance' cn [instanceType]              
                   isPrimitive <-lift $ isInstance ''Prim [instanceType]
                   let isGeneric = ''G.Generic == cn             
                   table <- get
                   if isMember || elem instanceType table || (isPrimitive && isGeneric)
                     then return []
                     else do
                       let context = case classContext of
                                       Nothing -> []
                                       Just cc -> if isGeneric then [] else [cc]
#if __GLASGOW_HASKELL__ >= 802
                       let c = [StandaloneDerivD Nothing context (AppT (ConT cn) instanceType)]
#else
                       let c = [StandaloneDerivD context (AppT (ConT cn) instanceType)]
#endif
                       modify (instanceType:)
                       names <- lift $ fmap concat $ mapM getCompositeTypeNames cons
                       xs <- mapM (\n -> genStandaloneDerivingDecl cn n) names
                       return $ concat xs ++ c

deriving_ :: Name -- ^ class name
          -> Name -- ^ type name
          -> Q [Dec]
deriving_ cn tn = evalStateT (genStandaloneDerivingDecl cn tn) []

derivings :: [Name] -- ^ class names
          -> Name   -- ^ type name
          -> Q [Dec]
derivings cns tn = fmap concat (mapM (\x -> deriving_ x tn) cns)

derivingss :: [Name] -- ^ class names
           -> [Name] -- ^ type names
           -> Q [Dec]
derivingss cns tns = fmap concat (mapM (\x -> derivings cns x) tns)
