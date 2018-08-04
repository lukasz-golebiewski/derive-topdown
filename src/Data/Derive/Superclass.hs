module Data.Derive.Superclass 
       (deriving_superclasses,
#if __GLASGOW_HASKELL__ >= 802        
        strategy_deriving_superclasses,
        newtype_deriving_superclasses
#endif
        )where

import Data.Derive.TopDown.Lib
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Debug.Trace
import Control.Monad
import Data.List
import Control.Monad.Trans.State
import Control.Monad.Trans
import Data.Maybe
import Language.Haskell.TH.Ppr

isHigherOrderClass :: Name -> Q Bool
isHigherOrderClass ty = do
                    cla <- reify ty
                    case cla of
                        ClassI (ClassD _ _ vars _ _) _ -> do
                                                    let (KindedTV _ k) = head vars
                                                    if k == StarT
                                                        then return True
                                                        else return False
                        _ -> error $ show ty ++ " is not a class"
                    


deriving_superclasses :: Name -> Name -> Q [Dec]
deriving_superclasses cn tn = do
                            a <- evalStateT (deriving_superclasses' Nothing cn tn) []
                            traceM $ show a
                            return a

#if __GLASGOW_HASKELL__ >= 802
strategy_deriving_superclasses :: DerivStrategy -> Name -> Name -> Q [Dec]
strategy_deriving_superclasses st cn tn = do
                            a <- evalStateT (deriving_superclasses' (Just st) cn tn) []
                            return a

newtype_deriving_superclasses = strategy_deriving_superclasses NewtypeStrategy
#endif

#if __GLASGOW_HASKELL__ >= 802
deriving_superclasses' :: Maybe DerivStrategy -> Name -> Name -> StateT [Type] Q [Dec]
deriving_superclasses' st cn tn = do
#else
deriving_superclasses' :: Name -> Name -> StateT [Type] Q [Dec]
deriving_superclasses' cn tn = do
#endif
                    (tvbs,cons) <- getTyVarCons cn tn
                    let tp = AppT (ConT cn) (ConT tn) 
                    types <- get
                    isCnHighOrderClass <- lift $ isHigherOrderClass cn
                    classContext <- if isCnHighOrderClass
                                        then lift $ generateClassContext cn tn
                                        else return Nothing
                    --
                    let Just a = classContext
                    let typeNames = map getTVBName tvbs
                    isIns <- lift $ isInstance' cn [ConT tn]
                    let context = maybeToList classContext
                    if (isIns || elem tp types)
                        then return []
                        else
                            do
                            topClassInstance <- return [StandaloneDerivD 
#if __GLASGOW_HASKELL__ >= 802
                                                            st
#endif
                                                            context tp]

                            modify (tp:)
                            ci <- lift $ reify cn
                            case ci of
                                ClassI (ClassD ctx _ _ _ _) _ -> do
                                                    let classConTs = map getTypeConstructor ctx
                                                    ss <- fmap (nub.concat) $ forM classConTs $ \(ConT className) -> do
                                                                                    superclass_decls <- deriving_superclasses' 
#if __GLASGOW_HASKELL__ >= 802
                                                                                                            st
#endif
                                                                                                            className tn
                                                                                    return superclass_decls
                                                    return $ topClassInstance ++ ss