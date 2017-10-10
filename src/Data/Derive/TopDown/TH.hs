{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Derive.TopDown.TH (derivingTH, derivingTHs, derivingTHss) where
import Data.Derive.TopDown.Lib

import Language.Haskell.TH.Lib
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

import Control.Monad.State
import Control.Monad.Trans
import Data.List (foldl')

genTH :: (ClassName, Name -> Q [Dec]) -> TypeName -> StateT [Type] Q [Dec]
genTH (className,deriveFunction) typeName = do
                       (tvbs, cons) <- lift $ getTyVarCons className typeName
                       compositeNames <- lift $ fmap concat $ mapM getCompositeTypeNames cons
                       let typeNames = map getTVBName tvbs
                       instanceType <- lift $ foldl' appT (conT typeName) $ map varT typeNames
                       isMember <- lift $ isInstance' className [instanceType]
                       table <- get
                       if isMember || elem instanceType table
                          then return []
                          else do
                             decl <- lift $ deriveFunction typeName
                             (modify (instanceType :))
                             subTypeNames <- lift $ fmap concat $ mapM getCompositeTypeNames cons
                             decls <- mapM (\n -> genTH (className,deriveFunction) n) subTypeNames
                             return $ concat decls ++ decl


derivingTH :: (Name, Name -> Q [Dec]) -- ^ class name and corresponding isntance generation function
           -> Name -- ^ type name
           -> Q [Dec]
derivingTH cd tname = evalStateT (genTH cd tname) []

derivingTHs :: [(Name, Name -> Q [Dec])] -- ^ class names and corresponding instance generation functions
            -> Name -- ^ type name
            -> Q [Dec]
derivingTHs cds typeName = fmap concat (mapM (\c -> derivingTH c typeName) cds)

derivingTHss :: [(Name, Name -> Q [Dec])] -- ^ class names and corresponding instance generation functions
             -> [Name] -- ^ type names
             -> Q [Dec]
derivingTHss cds typeNames = fmap concat (mapM (\t -> derivingTHs cds t) typeNames)
