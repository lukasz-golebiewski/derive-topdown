module Data.Derive.Superclass where

import Data.Derive.TopDown.Lib
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Debug.Trace
import Control.Monad
import Data.List
import Control.Monad.Trans.State
import Control.Monad.Trans

deriving_superclasses :: Name -> Name -> Q [Dec]
deriving_superclasses cn tn = evalStateT (deriving_superclasses' cn tn) []

deriving_superclasses' :: Name -> Name -> StateT [Type] Q [Dec]
deriving_superclasses' cn tn = do
                    isIns <- lift $ isInstance' cn [ConT tn]
                    let tp = AppT (ConT cn) (ConT tn) 
                    types <- get
                    if (isIns || elem tp types)
                        then return []
                        else
                            do
                            topClassInstance <- return [StandaloneDerivD Nothing [] tp]
                            modify (tp:)
                            ci <- lift $ reify cn
                            case ci of
                                ClassI (ClassD ctx _ _ _ _) _ -> do
                                                    let classConTs = map getTypeConstructor ctx
                                                    ss <- fmap (nub.concat) $ forM classConTs $ \(ConT className) -> do
                                                                                    superclass_decls <- deriving_superclasses' className tn
                                                                                    return superclass_decls
                                                    return $ topClassInstance ++ ss