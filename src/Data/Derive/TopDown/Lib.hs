{-# LANGUAGE TemplateHaskell,ExplicitForAll,ScopedTypeVariables,StandaloneDeriving,PolyKinds,ExistentialQuantification,FlexibleContexts,UndecidableInstances #-}

module Data.Derive.TopDown.Lib (isInstance', generateClassContext,getTyVarCons,getTVBName, getCompositeTypeNames, ClassName,TypeName) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Generics (mkT,everywhere,mkQ,everything)
import GHC.Exts
import Language.Haskell.TH.ExpandSyns (expandSyns)
import Data.List (nub)

-- This is an isInstance function with polymorphic type replaced by Any in GHC.Exts
-- This is inspired by Ryan Scott
-- see https://ghc.haskell.org/trac/ghc/ticket/10607
-- isInstance will not work with Typeable.
-- See https://ghc.haskell.org/trac/ghc/ticket/11251
isInstance' :: Name -> [Type] -> Q Bool
isInstance' className tys = isInstance className (map (removeExplicitForAllTrans. replacePolyTypeTrans) tys)

replacePolyType :: Type -> Type
replacePolyType (VarT t) = ConT ''Any
replacePolyType x = x

replacePolyTypeTrans = everywhere (mkT replacePolyType)

removeExplicitForAll :: Type -> Type
removeExplicitForAll (ForallT _ _ t) = t
removeExplicitForAll t = t

removeExplicitForAllTrans :: Type -> Type
removeExplicitForAllTrans = everywhere (mkT removeExplicitForAll)

constructorTypesVars :: Type -> [Type]
-- constructorTypesVars f@(ForallT _ _ _) = error "forall"
constructorTypesVars a@(AppT (VarT tvn) t2) = [a]
constructorTypesVars c@(AppT (ConT name) t) = constructorTypesVars t
constructorTypesVars c@(AppT t1 t2) = constructorTypesVars t1 ++ constructorTypesVars t2
constructorTypesVars v@(VarT name) = [v]
constructorTypesVars c@(ConT name) = []
-- constructorTypesVars (PromotedT name) = undefined
constructorTypesVars (InfixT t1 name t2) = constructorTypesVars t1 ++ constructorTypesVars t2
constructorTypesVars (UInfixT t1 name t2) = constructorTypesVars t1 ++ constructorTypesVars t2
constructorTypesVars (ParensT t) = constructorTypesVars t
constructorTypesVars (TupleT i) = []
constructorTypesVars (ListT ) = [] 
-- constructorTypesVars (UnboxedTupleT i) = undefined
-- constructorTypesVars (UnboxedSumT t) = undefined -- ghc 8.2.1
constructorTypesVars (ArrowT) = [ArrowT]
constructorTypesVars t = error $ "unsupported type " ++ show t

expandSynsAndGetContextTypes :: Type -> Q [Type]
expandSynsAndGetContextTypes t = do
                             t' <- (expandSyns t)
                             return $ (constructorTypesVars t')

third (a,b,c) = c

getContextType :: Con -> Q [Type]
getContextType (NormalC name bangtypes) = fmap concat $ mapM expandSynsAndGetContextTypes (map snd bangtypes)
getContextType (RecC name varbangtypes) = fmap concat $ mapM expandSynsAndGetContextTypes (map third varbangtypes)
getContextType (InfixC bangtype1 name bangtype2) =  fmap concat $ mapM expandSynsAndGetContextTypes (map snd [bangtype1, bangtype2])
getContextType (ForallC binding context con) = getContextType con -- is forall allowed in class instances context?
getContextType (GadtC name bangtypes result_type) = fmap concat $ mapM expandSynsAndGetContextTypes (map snd bangtypes)
getContextType (RecGadtC name bangtypes result_type) = fmap concat $ mapM expandSynsAndGetContextTypes (map third bangtypes)

getVar :: Type -> [Type]
getVar v@(VarT t) = [v]
getVar v = []

getAllVars :: Type -> [Type]
getAllVars = everything (++) (mkQ [] getVar)

getTyVarCons :: ClassName -> TypeName -> Q ([TyVarBndr], [Con])
getTyVarCons cn name = do
            info <- reify name
            case info of
              TyConI dec -> case dec of
                              DataD _ _ tvbs _ cons _  -> return (tvbs, cons)
                              NewtypeD _ _ tvbs _ con _-> return (tvbs, [con])
                              TySynD name tvbs t -> undefined
                              _ -> error "not a data or newtype definition"
              _ -> error $ "cannot generate "++ show cn ++ " instances for " ++ show name

type ClassName = Name
type TypeName = Name

-- In the future of GHC, this will be removed.
-- See https://ghc.haskell.org/trac/ghc/ticket/13324
generateClassContext :: ClassName -> TypeName -> Q (Maybe Type)
generateClassContext classname dataname = do
                            (tvbs, cons) <- getTyVarCons classname dataname
                            types <- fmap nub $ fmap concat $ mapM getContextType cons
                            let len = length types
                            if len == 0
                              then return Nothing
                              else do
                                  -- Eq a, Eq b ...
                                  let contexts = map (AppT (ConT classname)) types                            
                                  -- (Eq a, Eq b ...)
                                  let contextTuple = foldl1 AppT $ (TupleT len) : contexts
                                  return $ Just contextTuple

getTVBName :: TyVarBndr -> Name
getTVBName (PlainTV name )   = name
getTVBName (KindedTV name _) = name

getTypeNames :: Type -> [Name]
getTypeNames (ForallT tvbs cxt t) = getTypeNames t
getTypeNames (ConT n) = [n]
getTypeNames (AppT t1 t2) = getTypeNames t1 ++ getTypeNames t2
getTypeNames _ = []

expandSynsAndGetTypeNames :: [Type] -> Q [TypeName]
expandSynsAndGetTypeNames ts = do
                          ts' <- mapM expandSyns ts
                          return $ concatMap getTypeNames ts'

getCompositeTypeNames :: Con -> Q [TypeName]
getCompositeTypeNames (NormalC n bts) = expandSynsAndGetTypeNames (map snd bts)
getCompositeTypeNames (RecC n vbts) = expandSynsAndGetTypeNames (map third vbts)
getCompositeTypeNames (InfixC st1 n st2) = expandSynsAndGetTypeNames (map snd [st1 , st2])
getCompositeTypeNames (ForallC bind context con) = getCompositeTypeNames con
getCompositeTypeNames (GadtC name bangtype resulttype) = expandSynsAndGetTypeNames (map snd bangtype)
getCompositeTypeNames (RecGadtC name bangtypes result_type) = expandSynsAndGetTypeNames (map third bangtypes)

                                   


