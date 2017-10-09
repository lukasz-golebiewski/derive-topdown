{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Derive.TopDown.DeriveTH (deriveTHs) where
import Data.Derive.TopDown.Lib

import Language.Haskell.TH.Lib
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

import Control.Monad.State
import Control.Monad.Trans 
import Data.List (foldl')
import Data.DeriveTH

-- I am not sure how can I remove a component in the first argument. If you know, please tell me
genTH :: (ClassName,Derivation) -> TypeName -> StateT [Type] Q [Dec]
genTH (className,derivation) typeName = do
                       (tvbs, cons) <- lift $ getTyVarCons className typeName
                       compositeNames <- lift $ fmap concat $ mapM getCompositeTypeNames cons
                       let classNameBase = nameBase className                      
                       let typeNames = map getTVBName tvbs
                       instanceType <- lift $ foldl' appT (conT typeName) $ map varT typeNames
                       isMember <- lift $ isInstance' className [instanceType]
                       table <- get
                       if isMember || elem instanceType table
                          then return []
                          else do                           
                             decl <- lift $ derive derivation typeName
                             (modify (instanceType :))
                             subTypeNames <- lift $ fmap concat $ mapM getCompositeTypeNames cons
                             decls <- mapM (\n -> genTH (className,derivation) n) subTypeNames
                             return $ concat decls ++ decl

deriveTHs :: [(ClassName, Derivation)] -> TypeName -> Q [Dec]
deriveTHs cds typeName = evalStateT (fmap concat $ mapM (\cd -> genTH cd typeName) cds) []
