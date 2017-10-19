{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Data.Derive.TopDown.Lib (isInstance', generateClassContext,getTyVarCons,getTVBName, getCompositeTypeNames, ClassName,TypeName) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Generics (mkT,everywhere,mkQ,everything)
import GHC.Exts
import Language.Haskell.TH.ExpandSyns (expandSyns)
import Data.List (nub,intersect)

#ifdef __GLASGOW_HASKELL__
import Data.Typeable
import Data.Data
#endif
-- `isInstance` in template library does not work with polymorphic types.
-- The follwoing is an isInstance function with polymorphic type replaced by Any in GHC.Exts so that it can work with polymorphic type.
-- This is inspired by Ryan Scott
-- see https://ghc.haskell.org/trac/ghc/ticket/10607
-- isInstance will not work with Typeable.
-- See https://ghc.haskell.org/trac/ghc/ticket/11251

-- For fixing deriving Typeable problem, I use Data type calss to replace Typeable since the are always pairing with each other. So if the data type is already an instance of Typeable and not an instance of Data, this will not work.
isInstance' :: Name -> [Type] -> Q Bool
isInstance' className tys =
#ifdef __GLASGOW_HASKELL__
               if className == ''Typeable
                then
                 isInstance' ''Data tys
                else
#endif
                 isInstance className (map (removeExplicitForAllTrans. replacePolyTypeTrans) tys)

replacePolyType :: Type -> Type
replacePolyType (VarT t) = ConT ''Any
replacePolyType x = x

replacePolyTypeTrans = everywhere (mkT replacePolyType)

removeExplicitForAll :: Type -> Type
removeExplicitForAll (ForallT _ _ t) = t
removeExplicitForAll t = t

removeExplicitForAllTrans :: Type -> Type
removeExplicitForAllTrans = everywhere (mkT removeExplicitForAll)

getVarName :: Type -> [Name]
getVarName (VarT n) = [n]
getVarName _ = []

getAllVarNames :: Type -> [Name]
getAllVarNames = everything (++) (mkQ [] getVarName)

constructorTypesVars :: Type -> [Type]
-- get all free variablein a forall type expression.
constructorTypesVars f@(ForallT tvbs _ t) = let scopedVarNames = map getTVBName tvbs in
                                              filter (\x -> null $ intersect (getAllVarNames x) scopedVarNames)
                                              (constructorTypesVars t)

constructorTypesVars a@(AppT (VarT tvn) t2) = [a]
constructorTypesVars c@(AppT (ConT name) t) = constructorTypesVars t
constructorTypesVars c@(AppT t1 t2) = constructorTypesVars t1 ++ constructorTypesVars t2
constructorTypesVars v@(VarT name) = [v]
constructorTypesVars c@(ConT name) = []
constructorTypesVars (PromotedT name) = []
#if __GLASGOW_HASKELL__ > 710
constructorTypesVars (InfixT t1 name t2) = constructorTypesVars t1 ++ constructorTypesVars t2
constructorTypesVars (UInfixT t1 name t2) = constructorTypesVars t1 ++ constructorTypesVars t2
constructorTypesVars (ParensT t) = constructorTypesVars t
#endif
constructorTypesVars (TupleT i) = []
constructorTypesVars (ListT ) = [] 
-- constructorTypesVars (UnboxedTupleT i) = undefined
-- constructorTypesVars (UnboxedSumT t) = undefined -- ghc 8.2.1
constructorTypesVars (EqualityT) = []
constructorTypesVars (PromotedTupleT i) = []
constructorTypesVars (PromotedNilT) = []
constructorTypesVars (PromotedConsT) = []
constructorTypesVars (LitT lit) = []
constructorTypesVars (ConstraintT) = []
-- constructorTypesVars (WildCardT lit) = undefined
constructorTypesVars (ArrowT) = [ArrowT]
constructorTypesVars t = error $ pprint t ++ " is not support"

expandSynsAndGetContextTypes :: Type -> Q [Type]
expandSynsAndGetContextTypes t = do
                             t' <- (expandSyns t)
                             return $ (constructorTypesVars t')

third (a,b,c) = c

getContextType :: Con -> Q [Type]
getContextType (NormalC name bangtypes) = fmap concat $ mapM expandSynsAndGetContextTypes (map snd bangtypes)
getContextType (RecC name varbangtypes) = fmap concat $ mapM expandSynsAndGetContextTypes (map third varbangtypes)
getContextType (InfixC bangtype1 name bangtype2) = fmap concat $ mapM expandSynsAndGetContextTypes (map snd [bangtype1, bangtype2])
-- need to remove types which contains scoped variables
getContextType (ForallC tvbs _ con) =  let scopedVarNames = map getTVBName tvbs in
                                         do
                                           types <- getContextType con
                                           let ty_vars = filter (\ty -> null $ intersect (getAllVarNames ty) scopedVarNames) types
                                           fmap concat $ mapM expandSynsAndGetContextTypes ty_vars
#if __GLASGOW_HASKELL__>710
getContextType (GadtC name bangtypes result_type) = fmap concat $ mapM expandSynsAndGetContextTypes (map snd bangtypes)
getContextType (RecGadtC name bangtypes result_type) = fmap concat $ mapM expandSynsAndGetContextTypes (map third bangtypes)
#endif

getTyVarCons :: ClassName -> TypeName -> Q ([TyVarBndr], [Con])
getTyVarCons cn name = do
            info <- reify name
            case info of
              TyConI dec -> case dec of
#if __GLASGOW_HASKELL__>710
                              DataD _ _ tvbs _ cons _  -> return (tvbs, cons)
                              NewtypeD _ _ tvbs _ con _-> return (tvbs, [con])
#else
                              DataD _ _ tvbs cons _  -> return (tvbs, cons)
                              NewtypeD _ _ tvbs con _-> return (tvbs, [con])
#endif
                              TySynD name tvbs t -> error $ show name ++ " is a type synonym and -XTypeSynonymInstances is not supported. If you did not derive it then This is a bug, please report this bug to the author of this package."
                              x -> error $ pprint x ++ " is not a data or newtype definition."
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
#if __GLASGOW_HASKELL__> 710
getCompositeTypeNames (GadtC name bangtype resulttype) = expandSynsAndGetTypeNames (map snd bangtype)
getCompositeTypeNames (RecGadtC name bangtypes result_type) = expandSynsAndGetTypeNames (map third bangtypes)
#endif
