{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, KindSignatures, ConstraintKinds, StandaloneDeriving, UndecidableInstances #-}
-- | 

module WebApi.Console.TH where

import WebApi.Console.Function
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Reflex
import Reflex.Dom
import WebApi.Console
import Data.Text (Text)
import qualified Data.Text as T
import Data.Proxy
import Data.Rank1Dynamic hiding (Dynamic)
import Control.Monad (forM)
import Data.List
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Rank1Dynamic as R1D (Dynamic)
import Data.Rank1Dynamic hiding (Dynamic)
import Data.Rank1Typeable hiding (V1)
import GHC.Exts
import Data.Data


getFunInfoQ' :: Maybe String -> Name -> Q Exp
getFunInfoQ' dispName' name = do
  info <- reify name
  let nameB = nameBase name
      dispName = maybe nameB id dispName'
      qualName = case name of
        (Name occ (NameG _ pkg mod')) -> GName (pkgString pkg) (modString mod') (occString occ)
        _                    -> error "Unable to get Func Info. Expecting only Global function name"
      funQ ty = sigE (varE name) (return ty)
  case info of
    VarI n ty _ _ -> [| (qualName , PrjFnInfo (toDynamic (Dict :: Dict (Eq Int)))  {-(toDynamic $(funQ ty))-}  (WidgetBox (Proxy :: Proxy Bool)) (pkgName qualName) (modName qualName) dispName) |]
    ClassOpI n ty parent _ -> do
      vInst <- reify parent
      error $ show vInst
      [| (qualName , PrjFnInfo undefined {-(toDynamic $(funQ ty))-}  (WidgetBox (Proxy :: Proxy Bool)) (pkgName qualName) (modName qualName) dispName) |]
    inf           -> error $ "Expection only function name of form :: T -> T -> Bool, but recieved: " ++ (show inf)


getAssertFnDyn :: Type -> Type
getAssertFnDyn (ForallT bndr cxt ty) = getAssertFnDyn' bndr cxt ty
getAssertFnDyn ty@(AppT ty1 ty2) = getAssertFnDyn' [] [] ty
getAssertFnDyn ty = error $ "Invalid Shape" ++ (show ty)

getAssertFnDyn' bndr [] (AppT (AppT ArrowT ty1) (AppT (AppT ArrowT ty2) (ConT ret)))
  | ret /= ''Bool = error $ "Return type of the assert function should be Bool, but found " ++ (show ret)
  | isVarT ty1 && ty1 /= ty2  = error "Tyvar not same "
getAssertFnDyn' bndr [](AppT (AppT ArrowT ty1) (ConT ret))
  | ret /= ''Bool = error $ "Return type of the assert function should be Bool, but found " ++ (show ret)
  | isVarT ty1 = undefined
getAssertFnDyn' _ _ ty = error $ "Invalid Function type: " ++ (show ty)

isVarT :: Type -> Bool
isVarT (VarT _) = True
isVarT _        = False

data Dict :: Constraint -> * where
  Dict :: a => Dict a

{-
instance (Typeable p, p) => Data (Dict p) where
  gfoldl _ z Dict = z Dict
  toConstr _ = dictConstr
  gunfold _ z c = case constrIndex c of
    1 -> z Dict
    _ -> error "gunfold"
  dataTypeOf _ = dictDataType
-}
dictConstr :: Constr
dictConstr = mkConstr dictDataType "Dict" [] Prefix

dictDataType :: DataType
dictDataType = mkDataType "WebApi.Console.TH.Dict" [dictConstr]

deriving instance Eq (Dict a)
deriving instance Ord (Dict a)
deriving instance Show (Dict a)


{-
getFunInfoQ :: Maybe String -> Name -> Q (TExp (Text, PrjFnInfo))
getFunInfoQ dispName name = do
  info <- reify name
  case info of
    VarI n ty _ _ -> [|| ("", PrjFnInfo undefined (WidgetBox (Proxy :: Proxy Bool)) "" "" dispName) ||]
    _           -> error "Expection only function name of form :: T -> T -> Bool"

assertFunctions :: [(Name, Maybe String)] -> Q [Dec]
assertFunctions names = do
  let funInfos = (flip map) names $ \(n, dispStr) -> getFunInfoQ' dispStr n
  [d|assertFn :: PrjMap; assertFn = HM.fromList $(listE funInfos)|]

-}

stdAssertFunNames :: [(Name, Maybe String)]
stdAssertFunNames =
  [ ('(==), Just "Equals")
  , ('(/=), Just "NotEquals")
  , ('(<), Just "LessThan")
  , ('(<=), Just "LessThanEquals")
  , ('(>), Just "GreaterThan")
  , ('(>=), Just "GreaterThanEquals")
  , ('(==), Just "Equals")
  , ('(&&), Just "And")
  , ('(||), Just "Or")
  , ('listNull, Just "IsEmpty")
  , ('listAnd, Just "And")
  , ('listOr, Just "Or")
  , ('isPrefixOf, Just "IsPrefixOf")
  , ('isSuffixOf, Just "IsSuffixOf")
  , ('isInfixOf, Just "IsInfixOf")
  , ('isSubsequenceOf, Just "IsSubsequenceOf")
  , ('listElem, Just "Contains")
  , ('listNotElem, Just "NotContains")
  , ('T.null, Just "IsEmpty")
  , ('T.isPrefixOf, Just "IsPrefixOf")
  , ('T.isSuffixOf, Just "IsSuffixOf")
  , ('T.isInfixOf, Just "IsInfixOf")
  , ('V.null, Just "IsEmpty")
  , ('V.elem, Just "Contains")
  , ('V.notElem, Just "NotContains")
  , ('V.and, Just "And")
  , ('V.or, Just "Or")
  , ('M.null, Just "IsEmpty")
  , ('M.member, Just "Contains")
  , ('M.notMember, Just "NotContains")
  , ('M.isSubmapOf, Just "IsSubmapOf")
  , ('M.isProperSubmapOf, Just "IsProperSubmapOf")
  ]

assertFunctionsE :: [(Name, Maybe String)] -> Q Exp
assertFunctionsE names = do
  let names' = nubBy eqName $ sortBy cmpName (stdAssertFunNames ++ names)
      cmpName (n1,_) (n2,_) = n1 `compare` n2
      eqName (n1,_) (n2,_) = n1 == n2
  let funInfos = (flip map) names' $ \(n, dispStr) -> getFunInfoQ' dispStr n
  [|HM.fromList $(listE funInfos)|]

projectionFunctions :: [Name] -> Q [Dec]
projectionFunctions = undefined

functions :: [(Name, Maybe String)] -> Q Exp
functions assertFns = do
  [| ConsoleFunctions { assertFunctions = $(assertFunctionsE assertFns)
                      }
   |]
     



