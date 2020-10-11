{-# LANGUAGE TemplateHaskell #-}
module Data.Derive.TopDown.Instance (instance_, instances, instancess, genEmptyInstanceDecl2) where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.Derive.TopDown.Lib
import           Data.List                  (foldl')
import           Data.Primitive.Types
import qualified GHC.Generics               as G
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax hiding (lift)

genEmptyInstanceDecl :: ClassName -> TypeName -> StateT [Type] Q [Dec]
genEmptyInstanceDecl cn tn = do
                   (tvbs,cons) <- getTyVarCons cn tn
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
#if __GLASGOW_HASKELL__> 710
                       let c = [InstanceD Nothing context (AppT (ConT cn) instanceType) []]
#else
                       let c = [InstanceD context (AppT (ConT cn) instanceType) []]
#endif
                       modify (instanceType:)
                       names <- lift $ fmap concat $ mapM getCompositeTypeNames cons
                       xs <- mapM (\n -> genEmptyInstanceDecl cn n) names
                       return $ concat xs ++ c

genEmptyInstanceDecl2 :: ClassName -> TypeName -> Q [Dec]
genEmptyInstanceDecl2 cn tn = do
                   (tvbs,_) <- evalStateT (getTyVarCons cn tn) []
                   classContext <- generateClassContext cn tn
                   let typeNames = map getTVBName tvbs
                   instanceType <- foldl' appT (conT tn) $ map varT typeNames
                   -- Stop generating further instances
                   -- 1. it is already a member of that type class
                   -- 2. we have already generated it, which is kind of same with case 1
                   -- 3. for GHC.Generic, if it is a primitive type like Int, Double
                   isMember <- isInstance' cn [instanceType]
                   isPrimitive <-isInstance ''Prim [instanceType]
                   let isGeneric = ''G.Generic == cn
                   if isMember || (isPrimitive && isGeneric)
                     then return []
                     else do
                       let context = case classContext of
                                       Nothing -> []
                                       Just cc -> if isGeneric then [] else [cc]
#if __GLASGOW_HASKELL__> 710
                       let c = [InstanceD Nothing context (AppT (ConT cn) instanceType) []]
#else
                       let c = [InstanceD context (AppT (ConT cn) instanceType) []]
#endif
                       return $ c

instance_ :: Name -- ^ class name
          -> Name -- ^ type name
          -> Q [Dec]
instance_ cn tn = evalStateT (genEmptyInstanceDecl cn tn) []

instances :: [Name] -- ^ class names
          -> Name   -- ^ type name
          -> Q [Dec]
instances cns tn = fmap concat (mapM (\x -> instance_ x tn) cns)

instancess :: [Name] -- ^ class names
           -> [Name] -- ^ type names
           -> Q [Dec]
instancess cns tns = fmap concat (mapM (\x -> instances cns x) tns)
