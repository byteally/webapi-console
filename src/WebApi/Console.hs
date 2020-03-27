{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, TypeOperators, TypeFamilies, GADTs, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, OverloadedLists, DefaultSignatures, StandaloneDeriving, StaticPointers, DeriveGeneric, TemplateHaskell, LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
-- |

module WebApi.Console
       (
         module WebApi.Console
       ) where

import qualified Data.Map as M
import qualified Data.Map.Lazy as LM
import Data.Maybe
import Reflex.Dom.Core as RD hiding (Request, Response)
import Data.Either
import Data.Proxy
import Data.Aeson as A hiding (Success)
import Data.Typeable
import WebApi.Contract
import WebApi.Util
import WebApi.Param as WebApi
import WebApi.ContentTypes
import GHC.Exts
import Network.URI
import Data.List
import           Data.CaseInsensitive  ( original )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as ASCII
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Network.HTTP.Media                    (mapContentMedia)
import Network.HTTP.Types
import Control.Monad
import Control.Monad.Fix
import GHC.Generics
import WebApi.PageTemplate
import Language.Javascript.JSaddle (MonadJSM)
import Data.Map (Map)
import Reflex.Utils
import Data.Monoid ((<>))
import Data.Functor.Contravariant
import Control.Exception
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Language.Javascript.JSaddle (JSM)

--foreign import javascript unsafe "console.log($1)" js_log :: JSString -> IO ()

type family ConsoleCtx api m r :: Constraint where
  ConsoleCtx api m r = ( ApiContract api m r
                       , MkPathFormatString r
                       , Typeable (PathParam m r)
                       , ToWidget (QueryParam m r)
                       , ToWidget (FormParam m r)
                       , ToWidget (FileParam m r)
                       , ToWidget (HeaderIn m r)
                       , ToWidget (CookieIn m r)
                       , ToPathParamWidget (PathParam m r) (IsTuple (PathParam m r))
                       , ToParam 'PathParam (PathParam m r)
                       , ToParam 'QueryParam (QueryParam m r)
                       , ToParam 'FormParam (FormParam m r)
                       , ToHeader (HeaderIn m r)
                       , ToParam 'FileParam (FileParam m r)
                       , FromHeader (HeaderOut m r)
                       , Decodings (ContentTypes m r) (ApiOut m r)
                       , Decodings (ContentTypes m r) (ApiErr m r)
                       , ToJSON (ApiOut m r)
                       , HeaderOut m r ~ ()
                       , CookieOut m r ~ ()
                       , RequestBody m r ~ '[]
                       , ParamErrToApiErr (ApiErr m r)
                       , SingMethod m
                       , IsUnitVal (IsUnit (QueryParam m r))
                       , IsUnitVal (IsUnit (PathParam m r))
                       , IsUnitVal (IsUnit (FormParam m r))
                       , IsUnitVal (IsUnit (HeaderIn m r))
                       , IsUnitVal (IsUnit (FileParam m r))
                       , IsUnitVal (IsUnit (CookieIn m r))
                       , IsUnitVal (IsUnit (ApiOut m r))
                       , IsUnitVal (IsUnit (ApiErr m r))
                       )
type family AllContracts api (tys :: [*]) :: Constraint where
  AllContracts api (Route (m ': ms) r ': rs) = (ConsoleCtx api m r, AllContracts api (Route ms r ': rs))
  AllContracts api (Route '[] r ': rs) = AllContracts api rs
  AllContracts api '[]       = ()

type family FlattenRoutes (xs :: [*]) :: [(*,*)] where
  FlattenRoutes (Route (m ': ms) r ': rs) = '(m, r) ': FlattenRoutes (Route ms r ': rs)
  FlattenRoutes (Route '[] r ': rs)       =  FlattenRoutes rs
  FlattenRoutes '[]                       = '[]

data SingRoute api (rs :: [(*,*)]) where
  RouteCons :: ConsoleCtx api m r => Proxy m -> Proxy r -> SingRoute api rs -> SingRoute api ('(m, r) ': rs)
  RouteNil  :: SingRoute api rs

data RouteBox api = forall m r.ConsoleCtx api m r => RouteBox (Proxy m) (Proxy r)

class SingRoutes api (apis :: [*]) where
  singRoutes :: Proxy api -> Proxy apis -> SingRoute api (FlattenRoutes apis)

instance ( SingRoutes api (Route ms r ': rs)
         , ConsoleCtx api m r
         ) => SingRoutes api (Route (m ': ms) r ': rs) where
  singRoutes api _ = RouteCons (Proxy :: Proxy m) (Proxy :: Proxy r) $ singRoutes api (Proxy :: Proxy (Route ms r ': rs))

instance SingRoutes api rs => SingRoutes api (Route '[] r ': rs) where
  singRoutes api _ = singRoutes api (Proxy :: Proxy rs)

instance SingRoutes api '[] where
  singRoutes _ _ = RouteNil

data ConsoleConfig = ConsoleConfig
  { baseURI :: URI
  , functions :: ConsoleFunctions
  } deriving (Show)

data ConsoleFunctions = ConsoleFunctions
  { assertFunctions :: ()
  } deriving (Show)

apiConsole :: forall api apis routes.
             ( WebApi api
             , apis ~ Apis api
             , routes ~ (FlattenRoutes apis)
             , SingRoutes api apis
             , AllContracts api apis
             ) => ConsoleConfig -> Proxy api -> JSM ()
apiConsole config api = do
  mainWidgetWithHead pageTemplate $ apiConsoleWidget config api

apiConsoleWidget :: forall api apis routes m t.
  ( WebApi api
  , apis ~ Apis api
  , routes ~ (FlattenRoutes apis)
  , SingRoutes api apis
  , AllContracts api apis
  , DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , PerformEvent t m
  , TriggerEvent t m
  , MonadJSM (Performable m)
  , HasJSContext (Performable m) -- TODO:
  ) => ConsoleConfig -> Proxy api -> m ()
apiConsoleWidget config api = do
  let routes = singRoutes api (Proxy :: Proxy apis)
      getPathSegs :: forall rs.SingRoute api rs -> [(Method, Text, (Event t () -> m (Event t ())))]
      getPathSegs (RouteCons pxyM pxyR rs)
        = let routeTpl = T.intercalate "/" $ mkRouteTplStr pathSegs (getDynTyRep $ getPathParPxy pxyM pxyR)
              pathSegs = mkPathFormatString pxyR
          in ( singMethod pxyM
             , routeTpl
             , (\x -> paramWidget api pxyM pxyR (baseURI config) (functions config) pathSegs x)
             ) : (getPathSegs rs)
      getPathSegs RouteNil           = []
      routeSegs = getPathSegs routes
      getPathParPxy :: Typeable (PathParam meth r) => Proxy meth -> Proxy r -> Proxy (PathParam meth r)
      getPathParPxy _ _ = Proxy

  el "main" $ do
    el "header" $ do
      el "h1" $ text "WebApi Console"
      el "small" $ text "v0.1.0"
    divClass "apiconsole" $ do
      dropdownTabDisplay "route-tab" "active-route-tab" $ LM.fromList $ (flip map) routeSegs $
          \(m, r, paramWid) -> ((show (m, r)), ((T.pack (ASCII.unpack m) <> ": " <> r), (void . paramWid)))
  
getDynTyRep :: Typeable a => Proxy a -> [Text]
getDynTyRep pth = map (T.pack . wrapBrace . show) $ case typeRepArgs $ typeRep pth of
  ts@(_ : _) -> ts
  []         -> [typeRep pth]
  where wrapBrace t = "{" ++ t ++ "}"

mkRouteTplStr :: [PathSegment] -> [Text] -> [Text]
mkRouteTplStr (StaticSegment p:pths) treps = p : (mkRouteTplStr pths treps)
mkRouteTplStr (Hole : pths) (trep : treps) = trep : (mkRouteTplStr pths treps)
mkRouteTplStr (Hole : _) []             = error "Panic: @mkRouteTplStr: not sufficient args to fill the hole"
mkRouteTplStr [] ["{()}"]                  = []
mkRouteTplStr [] []                        = []
mkRouteTplStr [] _                     = [] --error $ "Panic: @mkRouteTplStr: more dynamic args found than the required" ++ (show treps)

type family IsUnit t :: Bool where
  IsUnit () = 'True
  IsUnit x = 'False

class IsUnitVal (b :: Bool) where
  isUnitVal :: Proxy b -> Bool

instance IsUnitVal 'True where
  isUnitVal _ = True

instance IsUnitVal 'False where
  isUnitVal _ = False

type family IsListLike t :: Bool where
  IsListLike [x] = 'True
  IsListLike x     = 'False

class IsListVal (b :: Bool) where
  isList :: Proxy b -> Bool

instance IsListVal 'True where
  isList _ = True

instance IsListVal 'False where
  isList _ = False

paramWidget ::  forall t m meth r api.
               ( DomBuilder t m
               , MonadSample t m
               , MonadHold t m
               , PostBuild t m
               , PerformEvent t m
               , TriggerEvent t m
               , MonadFix m
               , MonadJSM (Performable m)
               , HasJSContext (Performable m) -- TODO: check with webapi-reflex client
               , ToWidget (QueryParam meth r)
               , ToWidget (FormParam meth r)
               , ToWidget (FileParam meth r)
               , ToWidget (HeaderIn meth r)
               , ToWidget (CookieIn meth r)
               , ToPathParamWidget (PathParam meth r) (IsTuple (PathParam meth r))
               , Decodings (ContentTypes meth r) (ApiOut meth r)
               , Decodings (ContentTypes meth r) (ApiErr meth r)
               , ToJSON (ApiOut meth r) -- TODO: Haack
               , HeaderOut meth r ~ ()
               , CookieOut meth r ~ ()
               , RequestBody meth r ~ '[]
               , ConsoleCtx api meth r
               , IsUnitVal (IsUnit (QueryParam meth r))
               , IsUnitVal (IsUnit (PathParam meth r))
               , IsUnitVal (IsUnit (FormParam meth r))
               , IsUnitVal (IsUnit (HeaderIn meth r))
               , IsUnitVal (IsUnit (FileParam meth r))
               , IsUnitVal (IsUnit (CookieIn meth r))
               , IsUnitVal (IsUnit (ApiOut meth r))
               , IsUnitVal (IsUnit (ApiErr meth r))
               ) => Proxy api -> Proxy meth -> Proxy r -> URI -> ConsoleFunctions -> [PathSegment] -> Event t () -> m (Event t ())
paramWidget _api meth r baseUrl _conFuns pathSegs onSubmit = divClass "box-wrapper" $ do
  onReq <- divClass "request-form" $ do
    let methDyn = constDyn $ decodeUtf8 $ singMethod (Proxy :: Proxy meth)
    (urlDyn, encodedFormParDyn, hdrIn) <- divClass "params" $ do
      let modifyAttr [] = return []
          modifyAttr ((x, False):xs) = do
            cls <- sample. current $ x
            x' <- holdDyn (M.insertWith (<>) "class" "active " cls) (updated x)
            return $ x' : (map hideIfUnit xs)
          modifyAttr (x:xs) = do
            xs' <- modifyAttr xs
            return $ hideIfUnit x : xs'
          hideIfUnit :: (Dynamic t (Map Text Text), Bool) -> Dynamic t (Map Text Text)
          hideIfUnit (x, False) = x
          hideIfUnit (_, True) =
            let cls = "class" =: "hide"
            in constDyn cls
          paramClass = "class" =: "tab"
      rec attrs <-
            mkSwitchableAttrs
              [ (onQuery, paramClass)
              , (onForm, paramClass)
              , (onPath, paramClass)
              , (onHeader, paramClass)
              , (onFile, paramClass)
              , (onCookie, paramClass)
              ] []
          let (_queryAttr, _formAttr, _pathAttr, _headerAttr, _fileAttr, _cookieAttr) = case attrs of
                [queryAttr, formAttr, pathAttr, headerAttr, fileAttr, cookieAttr]
                  -> (queryAttr, formAttr, pathAttr, headerAttr, fileAttr, cookieAttr)
                _ -> error "Panic impossible"
          (queryAttr', formAttr', pathAttr', headerAttr', fileAttr', cookieAttr') <-
            (modifyAttr $ zip attrs
              [ isUnitVal (Proxy :: Proxy (IsUnit (QueryParam meth r)))
              , isUnitVal (Proxy :: Proxy (IsUnit (FormParam meth r)))
              , isUnitVal (Proxy :: Proxy (IsUnit (PathParam meth r)))
              , isUnitVal (Proxy :: Proxy (IsUnit (HeaderIn meth r)))
              , isUnitVal (Proxy :: Proxy (IsUnit (FileParam meth r)))
              , isUnitVal (Proxy :: Proxy (IsUnit (CookieIn meth r)))
              ]) >>= \case
            [queryAttr'', formAttr'', pathAttr'', headerAttr'', fileAttr'', cookieAttr''] -> pure (queryAttr'', formAttr'', pathAttr'', headerAttr'', fileAttr'', cookieAttr'')
            _ -> error "Panic impossible"
          (onQuery, onForm, onPath, onHeader, onFile, onCookie) <- divClass "div-head" $ do
            onQuery' <- clickableSpan "Query Params" queryAttr'
            onForm' <- clickableSpan "Form Params" formAttr'
            onPath' <- clickableSpan "Path Params" pathAttr'
            onHeader' <- clickableSpan "HeaderIn" headerAttr'
            onFile' <- clickableSpan "File Params" fileAttr'
            onCookie' <- clickableSpan "Cookie" cookieAttr'
            return (onQuery', onForm', onPath', onHeader', onFile', onCookie')
      rec
        (queryWidget, formWidget, pthWidget, headWidget, fileWidget, cookWidget) <- divClass "param-form" $ do
          divClass "request-url" $ dynText linkDyn
          queryWidget' <- elDynAttr "div" queryAttr' $ do
            --divClass "param-type query" $ return ()
            toWidget (Proxy :: Proxy (QueryParam meth r)) Nothing
          formWidget' <- elDynAttr "div" formAttr' $ do
            --divClass "param-type" $ text "Form Params"
            toWidget (Proxy :: Proxy (FormParam meth r)) Nothing
          pthWidget' <- elDynAttr "div" pathAttr' $ do
            --divClass "param-type" $ text "Path Params"
            pathWidget meth r pathSegs
          headWidget' <- elDynAttr "div" headerAttr' $ do
            --divClass "param-type" $ text "Headers"
            toWidget (Proxy :: Proxy (HeaderIn meth r)) Nothing
          fileWidget' <- elDynAttr "div" fileAttr' $ do
            --divClass "param-type" $ text "Files"
            toWidget (Proxy :: Proxy (FileParam meth r)) Nothing
          cookWidget' <- elDynAttr "div" cookieAttr' $ do
            --divClass "param-type" $ text "Cookies"
            toWidget (Proxy :: Proxy (CookieIn meth r)) Nothing
          return (queryWidget', formWidget', pthWidget', headWidget', fileWidget', cookWidget')
        (requestDyn :: Dynamic t (Maybe (Request meth r))) <- pure (mkReq
                                                              <$> pthWidget
                                                              <*> queryWidget
                                                              <*> formWidget
                                                              <*> fileWidget
                                                              <*> headWidget
                                                              <*> cookWidget
                                                              <*> methDyn)
        formParMapDyn <- pure $ fmap (fmap (toFormParam . formParam)) requestDyn
        encodedFormParDyn <- pure $ fmap ((fromMaybe "") . fmap (renderSimpleQuery False)) formParMapDyn
        linkDyn <- pure $ fmap ((fromMaybe "") . (fmap (\req -> T.pack $ show $ WebApi.link (undefined :: meth, undefined :: r) (ASCII.pack $ uriPath baseUrl) (pathParam req) (Just $ queryParam req)))) requestDyn
        let toHdrStrs hdrs = M.fromList $ fmap (\(k, v) -> (T.pack $ ASCII.unpack $ original k, T.pack $ ASCII.unpack v)) hdrs
        hdrIn <- pure $ fmap (fmap (toHdrStrs . toHeader . headerIn)) requestDyn
      return (linkDyn, encodedFormParDyn, hdrIn)
      -- display encodedFormParDyn
    return $ tag (pull $ mkXhrReq methDyn urlDyn encodedFormParDyn hdrIn) onSubmit
  resE <- performRequestAsyncWithError onReq
  let res = fmap (\re -> case re of
                     Left ex -> Left ex
                     Right r' -> case _xhrResponse_responseText r' of
                       Just txt -> Right (Status (fromIntegral $ _xhrResponse_status r') (encodeUtf8 $ _xhrResponse_statusText r'), txt)
                       Nothing  -> Left (error "EMPTY BODY")
                 ) resE
      mkResponse' (Right (st, respTxt)) = mkResponse st (encodeUtf8 respTxt)
      mkResponse' (Left ex)             = Failure $ Right (OtherError $ toException ex)
      _response :: Event t (Response meth r)
      _response = mkResponse' <$> res
  divClass "right-pane" $ do
    divClass "response" $ do
      divClass "title-text" $ do
        text "response: "
--        display =<< holdDyn Nothing ((Just . A.encode) <$> response)
    pure ()
  return $ tag (constant ()) onReq
  where
    mkReq pw qw fw fiw hw cw _md = Request <$> pw
                                      <*> qw
                                      <*> fw
                                      <*> fiw
                                      <*> hw
                                      <*> cw
                                      <*> pure ()


-- TODO: Dup from WebApi package
mkResponse :: forall m r.( Decodings (ContentTypes m r) (ApiOut m r)
                   , Decodings (ContentTypes m r) (ApiErr m r)
                   , ParamErrToApiErr (ApiErr m r)
                   , CookieOut m r ~ ()
                   , HeaderOut m r ~ ()
                   ) => Status -> ByteString -> Response m r
mkResponse status respBodyBS =
  case statusIsSuccessful status of
    True  -> case Success <$> pure status
                         <*> (Validation $ toParamErr $ decodeResponse (undefined :: (m, r)) respBodyBS)
                         <*> pure ()
                         <*> pure () of
      Validation (Right success) -> success
      Validation (Left errs) -> Failure $ Left $ ApiError status (toApiErr errs) Nothing Nothing
    False -> case ApiError
                  <$> pure status
                  <*> (Validation $ toParamErr $ decodeResponse (undefined :: (m, r)) respBodyBS)
                  <*> (Just <$> pure ())
                  -- TODO: Handle cookies
                  <*> pure Nothing of
               Validation (Right failure) -> (Failure . Left) failure
               Validation (Left _errs) -> Failure $ Right (OtherError (error "unknown exception"))
  where toParamErr :: Either String a -> Either [ParamErr] a
        toParamErr (Left _str) = Left []
        toParamErr (Right r)  = Right r

decodeResponse :: ( Decodings (ContentTypes m r) a
          ) => apiRes m r -> ByteString -> Either String a
decodeResponse r o = case (Just "application/json") of -- TODO: Needs Response header from reflex
  Just ctype -> let decs = decodings (reproxy r) (fromStrict o)
               in maybe (firstRight (map snd decs)) id (mapContentMedia decs ctype)
  Nothing    -> firstRight (map snd (decodings (reproxy r) (fromStrict o)))
  where
    reproxy :: apiRes m r -> Proxy (ContentTypes m r)
    reproxy = const Proxy
    firstRight :: [Either String b] -> Either String b
    firstRight = maybe (Left "Couldn't find matching Content-Type") id . find isRight

newtype PathPar t a (isTup :: Bool) = PathPar
  { getPathPar :: ([PathSegment] -> (Dynamic t a)) }

type family IsTuple t where
  IsTuple () = 'True
  IsTuple (t1,t2) = 'True
  IsTuple t       = 'False

pathWidget :: forall t m meth r.
             ( DomBuilder t m
             , ToPathParamWidget
                        (PathParam meth r) (IsTuple (PathParam meth r))
             ) => Proxy meth -> Proxy r -> [PathSegment] -> m (Dynamic t (Maybe (PathParam meth r)))
pathWidget _meth _ pthSegs = do
  topathParamWidget (undefined :: (PathPar t (PathParam meth r) (IsTuple (PathParam meth r)))) pthSegs

class CtorInfo (f :: * -> *) where
  constructorNames :: proxy f -> [Text]

instance CtorInfo f => CtorInfo (D1 c f) where
  constructorNames _ = constructorNames (Proxy :: Proxy f)

instance (CtorInfo x, CtorInfo y) => CtorInfo (x :+: y) where
  constructorNames _ = constructorNames (Proxy :: Proxy x) ++ constructorNames (Proxy :: Proxy y)

instance (Constructor c) => CtorInfo (C1 c f) where
  constructorNames _ = [T.pack $ conName (undefined :: t c f a)]

type DynamicAttr t = Dynamic t (Map Text Text)

data GToWidgetState t = GToWidgetState
  { st_constructors :: (Event t Text, M.Map Text (DynamicAttr t))
  }

data GToWidgetOpts t f a = GToWidgetOpts
  { state        :: Maybe (GToWidgetState t)
  , widgetDefVal :: Maybe (f a)
  , arbitraryDef :: Maybe (Dynamic t (f a))
  }

class GToWidget f where
  gToWidget :: ( DomBuilder t m
               , MonadFix m
               , MonadHold t m
               , PostBuild t m
              ) => GToWidgetOpts t f a -> m (Dynamic t (Maybe (f a)))

instance (GToWidget f, CtorInfo f) => GToWidget (D1 c f) where
  gToWidget (GToWidgetOpts _ wDef aDef) = do
    {-wDef' <- case wDef of
      Just (D1 x) -> Just x
      _ -> Nothing-}
    let ctorNames = constructorNames (Proxy :: Proxy f)
    aDef' <- case aDef of
      Just dynM1 -> pure $ Just $ fmap (\(M1 a) -> a) dynM1
      _           -> return Nothing
    let wDef' = fmap (\(M1 a) -> a) wDef
        gopts' = GToWidgetOpts Nothing wDef' aDef'
    case ctorNames of
      (firstCtor:_:_) -> do  -- SumType
        divClass "sum-wrapper" $ do
          let ctorNameMap = M.fromList $ map (\x -> (x, x)) ctorNames
          dd <- dropdown firstCtor (constDyn ctorNameMap) $ def
          sumTyAttrMap <- (return . M.fromList) =<< mapM (\c -> do
            cDyn <- pure $ fmap (\ddVal -> if ddVal == c then ("class" =: "sum-ty active") else ("class" =: "sum-ty")) (_dropdown_value dd)
            return (c, cDyn)
            ) ctorNames
          (fmap . fmap . fmap) M1 $ gToWidget gopts' { state = Just $ GToWidgetState (_dropdown_change dd, sumTyAttrMap) }
      _ -> (fmap . fmap . fmap) M1 $ gToWidget gopts'

instance (GToWidget f, GToWidget g, CtorInfo f, GToWidget (g :+: h)) => GToWidget (f :+: g :+: h) where
  gToWidget (GToWidgetOpts gstate wDef _aDef) = do
    let (evt, attrMap) =
          case gstate of
            Just (GToWidgetState st) -> st
            _                        -> (never, M.empty)
        lConName         = head $ constructorNames (Proxy :: Proxy f)
        lDynAttr = fromMaybe (error $ "PANIC!: Constructor lookup failed @ GToWidget (f :+: g)" ++ (T.unpack lConName) ++ (show $ M.keys attrMap)) (M.lookup lConName attrMap)
        (lwDef, rwDef) = case wDef of
          Just (L1 x) -> (Just x, Nothing)
          Just (R1 x) -> (Nothing, Just x)
          _ -> (Nothing, Nothing)

    lDyn <- (fmap . fmap . fmap) L1 $  do
      elDynAttr "div" lDynAttr $ do
        gToWidget (GToWidgetOpts (Just $ GToWidgetState ({-M.delete lConName-} (evt, attrMap))) lwDef Nothing)
    rDyn <- (fmap . fmap . fmap) R1 $ gToWidget (GToWidgetOpts (Just $ GToWidgetState ({-M.delete lConName-} (evt, attrMap))) rwDef Nothing)

    fmap join $ foldDyn (\a _ -> if a == lConName then lDyn else rDyn) lDyn evt

instance (GToWidget f, GToWidget g, Typeable g, CtorInfo f, Constructor c) => GToWidget (f :+: C1 c g) where
  gToWidget (GToWidgetOpts gstate wDef _aDef) = do
    let (evt, attrMap) =
          case gstate of
            Just (GToWidgetState st) -> st
            _                        -> (never, M.empty)
        lConName         = head $ constructorNames (Proxy :: Proxy f)
        lDynAttr = fromMaybe (error $ "PANIC!: Constructor lookup failed @ GToWidget (f :+: C1 c f)" ++ (T.unpack lConName) ++ (show $ M.keys attrMap)) (M.lookup lConName attrMap)
        rConName         = T.pack $ conName (undefined :: t c g a)
        rDynAttr = fromMaybe (error $ "PANIC!: Constructor lookup failed @ GToWidget (f :+: C1 c f)" ++ (T.unpack lConName) ++ (show $ M.keys attrMap)) (M.lookup rConName attrMap)
        (lwDef, rwDef) = case wDef of
          Just (L1 x) -> (Just x, Nothing)
          Just (R1 x) -> (Nothing, Just x)
          _ -> (Nothing, Nothing)
    lDyn <- (fmap . fmap . fmap) L1 $ do
      elDynAttr "div" lDynAttr $ do
        gToWidget (GToWidgetOpts (Just $ GToWidgetState ({-M.delete lConName-} evt, attrMap)) lwDef Nothing)
    rDyn <- (fmap . fmap . fmap) R1 $ do
      elDynAttr "div" rDynAttr $ do
        gToWidget (GToWidgetOpts Nothing rwDef Nothing)

    fmap join $ foldDyn (\a _ -> if a == lConName then lDyn else rDyn) lDyn evt

instance (GToWidget a, GToWidget b) => GToWidget (a :*: b) where
  gToWidget (GToWidgetOpts _ wDef _)= do
    let (awDef, bwDef) = case wDef of
          Nothing -> (Nothing, Nothing)
          Just (ad :*: bd) -> (Just ad, Just bd)
        aGopts' = GToWidgetOpts Nothing awDef Nothing
        bGopts' = GToWidgetOpts Nothing bwDef Nothing
    adyn <- gToWidget aGopts'
    bdyn <- gToWidget bGopts'
    pure $ (\a b -> (:*:) <$> a <*> b) <$> adyn <*> bdyn

instance (GToWidget f, Typeable f, Constructor c) => GToWidget (C1 c f) where
  gToWidget (GToWidgetOpts _ wDef _) = do
    let wDef' = fmap (\(M1 a) -> a) wDef
        gopts' = GToWidgetOpts Nothing wDef' Nothing
    case eqT :: Maybe (f :~: U1) of
      Just Refl -> do
        (fmap . fmap . fmap) M1 (gToWidget gopts')
      _ -> do
        elClass "fieldset" "nested-field field" $ do
          el "legend" $ text $ T.pack $ conName (undefined :: C1 c f ())
          divClass "field" $ (fmap . fmap . fmap) M1 (gToWidget gopts')

instance GToWidget U1 where
  gToWidget _ = return $ constDyn (Just U1)

instance GToWidget V1 where
  gToWidget _ = return $ constDyn (error "PANIC!: Unreachable code")

instance (GToWidget f, Selector s) => GToWidget (S1 s f) where
  gToWidget (GToWidgetOpts _ wDef _) = do
    let wDef' = fmap (\(M1 a) -> a) wDef
        gopts' = GToWidgetOpts Nothing wDef' Nothing
    elClass "div" "field" $ do
      elAttr "label" ("class" =: "label") $ text $ T.pack $ selName (undefined :: S1 s f ())
      inp <- gToWidget gopts'
      pure $ fmap (fmap M1) inp

instance (ToWidget f) => GToWidget (K1 c f) where
  gToWidget (GToWidgetOpts _ wDef _) =
    let wDef' = fmap (\(K1 a) -> a) wDef
    in (fmap . fmap . fmap) K1 $ toWidget Proxy wDef'

class ToWidget a where
  toWidget :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Proxy a -> Maybe a -> m (Dynamic t (Maybe a))
  default toWidget :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Generic a, GToWidget (Rep a)) => Proxy a -> Maybe a -> m (Dynamic t (Maybe a))
  toWidget _ wDef = (fmap . fmap . fmap) to $ gToWidget (GToWidgetOpts Nothing (fmap from wDef) Nothing)

instance ToWidget Text where
  toWidget _ _wDef = do
    txt <- inputElement def
    pure $ fmap (decodeParam . ASCII.pack . T.unpack) $ _inputElement_value txt

instance ToWidget Int where
  toWidget _ wDef = do
    let def' = def
--                & inputElementConfig_inputType .~ "number"
--                & attributes .~ constDyn ("class" =: "text-box")
        intDef = case wDef of
          Nothing -> def'
          Just a -> def' & inputElementConfig_initialValue .~ (T.pack $ show a)
    txt <- inputElement intDef
    pure $ fmap (decodeParam . ASCII.pack . T.unpack) $ _inputElement_value txt

instance ToWidget Double where
  toWidget _ wDef = do
    let def' = def
        doubleDef = case wDef of
          Nothing -> def'
          Just a -> def' & inputElementConfig_initialValue .~ (T.pack $ show a)
    txt <- inputElement doubleDef
    pure $ fmap (decodeParam . ASCII.pack . T.unpack) $ _inputElement_value txt

instance ToWidget Bool where
  toWidget _ _ = do
    chk <- inputElement def
    pure $ fmap Just $ _inputElement_checked chk

instance ToWidget () where
  toWidget _ _ = pure (pure $ Just ())

instance ToWidget UTCTime where
  toWidget _ wDef = do
    let def' = def
        utcDef = maybe def' (\a -> def' & inputElementConfig_initialValue .~ (T.pack . ASCII.unpack . encodeParam $ a)) wDef
    txt <- inputElement utcDef
    dynText $ _inputElement_value txt
    pure $ fmap (decodeParam . ASCII.pack . T.unpack) $ _inputElement_value txt

instance ToWidget LocalTime where
  toWidget _ wDef = do
    let def' = def
               & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "datetime-local")
--               & attributes .~ (constDyn $ M.fromList [("class", "text-box"), ("step", "1")])
        timeDef = maybe def' (\a -> def' & inputElementConfig_initialValue .~ (T.pack . ASCII.unpack . encodeParam $ a)) wDef
    txt <- inputElement timeDef
    dynText $ _inputElement_value txt
    pure $ fmap (decodeParam . ASCII.pack . T.unpack) $ _inputElement_value txt

instance ToWidget Day where
  toWidget _ wDef = do
    let def' = def
               & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "date")
--               & attributes .~ constDyn ("class" =: "text-box")
        dayDef = maybe def' (\a -> def' & inputElementConfig_initialValue .~ (T.pack . ASCII.unpack . encodeParam $ a)) wDef
    txt <- inputElement dayDef
    dynText $ _inputElement_value txt
    pure $ fmap (decodeParam . ASCII.pack . T.unpack) $ _inputElement_value txt

instance ToWidget TimeOfDay where
  toWidget _ wDef = do
    let def' = def
               & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "time")
--               & attributes .~ (constDyn $ M.fromList [("class", "text-box"), ("step", "1")])
        todDef = maybe def' (\a -> def' & inputElementConfig_initialValue .~ (T.pack . ASCII.unpack . encodeParam $ a)) wDef
    txt <- inputElement todDef
    dynText $ _inputElement_value txt
    pure $ fmap (decodeParam . ASCII.pack . T.unpack) $ _inputElement_value txt


instance ToWidget a => ToWidget [a] where
  toWidget _ _ = do
    divClass "list-wrapper" $ do
      rec dynValMap <- listWithKeyShallowDiff (M.empty :: M.Map Int ()) (leftmost evtList) createWidget
          let setNothingAt i = do
                valMap <- sample. current $ dynValMap
                return $ Just $ M.mapWithKey (\k _ -> if k == i then Nothing else Just ()) valMap
              addElement = do
                valMap <- sample. current $ dynValMap
                lastKey <- sample . current $ lastKeyD
                return $ M.insert (lastKey + 1) (Just ()) $ M.map (const (Just ())) valMap
          dynListWithKeys <- pure $ fmap M.toList dynValMap
          dynValMap' <- pure $ fmap (M.map fst) dynValMap
          let getLastKey (x :: [Int]) = if null x then (-1) else maximum x
          lastKeyD <- pure $ fmap (getLastKey . map fst) dynListWithKeys
          let (_, evtsD) = splitDynPure $ fmap (unzip . map snd) dynListWithKeys
          let modelD = joinDynThroughMap dynValMap'
          evts <- pure $ fmap leftmost evtsD -- Remove events
          let evtList = (tag (pull addElement) addEvt) : [(push setNothingAt $ switchPromptlyDyn evts)]
          (addEvtEl, _) <- elAttr' "span" ("class" =: "plus-button") $ text "+"
          let addEvt = domEvent Click addEvtEl
      pure $ fmap (sequence . (map snd) . M.toList) modelD
    where
      -- fn :: (Reflex t, MonadSample t m) => [Dynamic t (Maybe a)] -> m (Maybe [a])
      -- fn = (\model -> do
      --   model' <- mapM (sample . current) model
      --   return $ sequence model'
      --   )
      createWidget k _ _ = initNew k
      initNew :: (DomBuilder t m , ToWidget a, MonadFix m, MonadHold t m, PostBuild t m) => Int -> m (Dynamic t (Maybe a), Event t Int)
      initNew i = do
        mDyn   <- toWidget (Proxy :: Proxy a) Nothing
        (removeEl, _) <- elAttr' "span" ("class" =: "cross-button") $ text "+"
        let onRemove = domEvent Click removeEl
        _mDyn' <- pure $ fmap (maybe [] (: [])) mDyn
        return (mDyn, tag (constant i) onRemove)

instance ToWidget a => ToWidget (Maybe a) where
  toWidget _ _ = do
    divClass "maybe-wrapper" $ do
      let checkboxDefVal = False
      chk <- inputElement def
      widget <- toWidget (Proxy :: Proxy a) Nothing
      let checkboxDyn = _inputElement_checked chk
      isActive <- toggle checkboxDefVal (updated checkboxDyn)
      pure $ (\a b -> fmap (\x -> if a then Just x else Nothing) b) <$> isActive <*> widget

class ToPathParamWidget (par :: *) (isTup :: Bool) where
  topathParamWidget :: DomBuilder t m => PathPar t par isTup -> [PathSegment] -> m (Dynamic t (Maybe par))

instance ToWidget par => ToPathParamWidget par 'False where
  topathParamWidget = undefined
{-  
  topathParamWidget _ pthSegs = do
    let (spths1, _:pthSegs1) = break (==Hole) pthSegs
--    forM spths1 $ \spth -> staticPthWid True spth -- TODO: Fix thi
    _1Wid <- toWidget (Proxy :: Proxy par) Nothing
    case break (==Hole) pthSegs1 of
      (spths2, []) -> forM spths2 $ \spth -> staticPthWid False spth
      (_,pths)     -> error "No. of Holes Invariant violated @ non-tuple case"
    return _1Wid
-}
instance ToPathParamWidget () 'True where
  topathParamWidget _ _pthSegs = pure (pure $ Just ())

instance (ToWidget t1, ToWidget t2) =>  ToPathParamWidget (t1, t2) 'True where
  topathParamWidget = undefined
{-  
  topathParamWidget _ pthSegs = do
    let (spths1, _:pthSegs1) = break (==Hole) pthSegs
--    forM spths1 $ \spth -> staticPthWid True spth -- TODO: Fix thi
    _1Wid <- toWidget (Proxy :: Proxy t1) Nothing
    let (spths2, _:pthSegs2) = break (==Hole) pthSegs1
--    forM spths2 $ \spth -> staticPthWid True spth -- TODO: Fix thi
    _2Wid <- toWidget (Proxy :: Proxy t2) Nothing
    case break (==Hole) pthSegs2 of
      (spths3, []) -> forM spths3 $ \spth -> staticPthWid False spth
      (_,pths)     -> error "No. of Holes Invariant violated @ (,) case"
    combineDyn (\a b -> (,) <$> a <*> b) _1Wid _2Wid
-}

instance ( ToWidget t1
         , ToWidget t2
         , ToWidget t2
         , ToWidget t3
         ) =>  ToPathParamWidget (t1, t2, t3) 'True where
  topathParamWidget = undefined
{-  
  topathParamWidget _ pthSegs = do
    let (spths1, _:pthSegs1) = break (==Hole) pthSegs
    forM spths1 $ \spth -> staticPthWid True spth
    _1Wid <- toWidget (Proxy :: Proxy t1) Nothing
    let (spths2, _:pthSegs2) = break (==Hole) pthSegs1
    forM spths2 $ \spth -> staticPthWid True spth
    _2Wid <- toWidget (Proxy :: Proxy t2) Nothing
    let (spths3, _:pthSegs3) = break (==Hole) pthSegs2
    forM spths3 $ \spth -> staticPthWid True spth
    _3Wid <- toWidget (Proxy :: Proxy t3) Nothing
    case break (==Hole) pthSegs3 of
      (spths4, []) -> forM spths4 $ \spth -> staticPthWid False spth
      (_,pths)     -> error "No. of Holes Invariant violated @ (,,) case"
    combineDyn3 (\a b c -> (,,) <$> a <*> b <*> c)  _1Wid _2Wid _3Wid
-}

mkXhrReq :: Reflex t
         => Dynamic t Text
         -> Dynamic t Text
         -> Dynamic t BS.ByteString
         -> Dynamic t (Maybe (Map Text Text))
         -> PullM t (XhrRequest Text)
mkXhrReq methD urlD fpD hdrInD = do
  meth <- sample . current $ methD
  url  <- sample . current $ urlD
  fp   <- sample . current $ fpD
  hdrIn <- (fromMaybe M.empty) <$> (sample . current $ hdrInD)
  let headerUrlEnc = if BS.null fp then hdrIn else M.insert "Content-type" "application/x-www-form-urlencoded" hdrIn
      body = T.pack $ ASCII.unpack fp
  return $ XhrRequest meth url
            $ def { _xhrRequestConfig_headers = headerUrlEnc
                  , _xhrRequestConfig_sendData = body
                  }

combinePredicates :: [Predicate a] -> Predicate a
combinePredicates xs = Predicate $ \x -> all id $ map (($ x) . getPredicate) xs
