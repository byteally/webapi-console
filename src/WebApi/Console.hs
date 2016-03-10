{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, TypeOperators, TypeFamilies, GADTs, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, OverloadedLists, DefaultSignatures, StandaloneDeriving #-}
-- | 

module WebApi.Console where

import Data.JSString ()
import GHCJS.Types
import Reflex.Dom
import Data.Proxy
import Data.Aeson as A
import Data.Typeable
import WebApi
import GHC.Exts
import Network.URI
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map.Lazy as LM
import Network.HTTP.Types
import Network.HTTP.Types.URI
import Network.URI
import Control.Monad
import GHC.Generics

foreign import javascript unsafe "console.log($1)" js_log :: JSString -> IO ()

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
                       , ToParam (PathParam m r) 'PathParam
                       , ToParam (QueryParam m r) 'QueryParam
                       , ToParam (FormParam m r) 'FormParam
                       , ToHeader (HeaderIn m r)
                       , ToParam (FileParam m r) 'FileParam
                       , FromHeader (HeaderOut m r)
                       , Decodings (ContentTypes m r) (ApiOut m r)
                       , Decodings (ContentTypes m r) (ApiErr m r)
                       , ParamErrToApiErr (ApiErr m r)
                       , SingMethod m
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
  } deriving (Show, Eq)
    
apiConsole :: forall api apis routes.
             ( WebApi api
             , apis ~ Apis api
             , routes ~ (FlattenRoutes apis)
             , SingRoutes api apis
             , AllContracts api apis
             ) => ConsoleConfig -> Proxy api -> IO ()
apiConsole config api = do
  let routes = singRoutes api (Proxy :: Proxy apis)
      getPathSegs :: forall t m rs.MonadWidget t m => SingRoute api rs -> [(Method, Text, m (Event t ()))]
      getPathSegs (RouteCons pxyM pxyR rs)
        = let routeTpl = T.intercalate "/" $ mkRouteTplStr pathSegs (getDynTyRep $ getPathParPxy pxyM pxyR)
              pathSegs = mkPathFormatString pxyR
          in ( singMethod pxyM
             , routeTpl
             , paramWidget api pxyM pxyR pathSegs
             ) : (getPathSegs rs)
      getPathSegs RouteNil           = []
      routeSegs = getPathSegs routes
      getPathParPxy :: Typeable (PathParam m r) => Proxy m -> Proxy r -> Proxy (PathParam m r)
      getPathParPxy _ _ = Proxy
  mainWidget $ do
    el "div" $ text "Welcome to WebApi console"
    tabDisplay "route-tab" "active-route-tab" $ LM.fromList $ (flip map) routeSegs $  
      \(m, r, paramWid) -> ((show (m, r)), ((show (m, r)), void paramWid))
    return ()

getDynTyRep :: Typeable a => Proxy a -> [Text]
getDynTyRep pth = map (T.pack . wrapBrace . show) $ case typeRepArgs $ typeRep pth of
  ts@(_ : _) -> ts
  []         -> [typeRep pth]
  where wrapBrace t = "{" ++ t ++ "}"

mkRouteTplStr :: [PathSegment] -> [Text] -> [Text]
mkRouteTplStr (StaticSegment p:pths) treps = p : (mkRouteTplStr pths treps)
mkRouteTplStr (Hole : pths) (trep : treps) = trep : (mkRouteTplStr pths treps)
mkRouteTplStr (Hole : pths) []             = error "Panic: @mkRouteTplStr: not sufficient args to fill the hole"
mkRouteTplStr [] []                        = []
mkRouteTplStr [] treps                     = error "Panic: @mkRouteTplStr: more dynamic args found than the required"

paramWidget ::  forall t m meth r api.
               ( MonadWidget t m
               , ToWidget (QueryParam meth r)
               , ToWidget (FormParam meth r)
               , ToWidget (FileParam meth r)
               , ToWidget (HeaderIn meth r)
               , ToWidget (CookieIn meth r)
               , ToPathParamWidget (PathParam meth r) (IsTuple (PathParam meth r))
               , ConsoleCtx api meth r
               ) => Proxy api -> Proxy meth -> Proxy r -> [PathSegment] -> m (Event t ())
paramWidget api meth r pathSegs = do
  let pthWidget   = pathWidget meth r pathSegs
      queryWidget = toWidget (Proxy :: Proxy (QueryParam meth r))
      formWidget :: m (Dynamic t (FormParam meth r))
      formWidget  = toWidget (Proxy :: Proxy (FormParam meth r))
      headWidget :: m (Dynamic t (HeaderIn meth r))
      headWidget  = toWidget (Proxy :: Proxy (HeaderIn meth r))
      fileWidget :: m (Dynamic t (FileParam meth r))
      fileWidget  = toWidget (Proxy :: Proxy (FileParam meth r))
      cookWidget :: m (Dynamic t (CookieIn meth r))
      cookWidget  = toWidget (Proxy :: Proxy (CookieIn meth r))
      methDyn = constDyn $ decodeUtf8 $ singMethod (Proxy :: Proxy meth)        
  (requestDyn :: Dynamic t (Request meth r)) <- join ((combineDyn7 Req)
                                                    <$> pthWidget
                                                    <*> queryWidget
                                                    <*> formWidget
                                                    <*> fileWidget
                                                    <*> headWidget
                                                    <*> cookWidget
                                                    <*> (return methDyn)
                                                   )
  let linkDyn = mapDyn (\req -> show $ WebApi.link (undefined :: meth, undefined :: r) nullURI (pathParam req) (Just $ queryParam req)) requestDyn
  formParMapDyn <- mapDyn (toFormParam . formParam) requestDyn
  encodedFormParDyn <- mapDyn (renderSimpleQuery False) formParMapDyn
  tabDisplay "params-tab" "param-tab" [ ("1-query" :: Text, ("Query Param", void queryWidget))
                                      , ("2-form", ("Form Param", void formWidget))
                                      , ("3-header", ("Request Header", void headWidget))
                                      , ("4-file", ("File Param", void fileWidget))
                                      , ("5-cookie", ("Cookie In", void cookWidget))
                                      , ("6-path", ("Path Param", void pthWidget))
                                      ]
  dynText =<< linkDyn
  display encodedFormParDyn
  button "Fire"

newtype PathPar t a (isTup :: Bool) = PathPar
  { getPathPar :: ([PathSegment] -> (Dynamic t a)) }

type family IsTuple t where
  IsTuple () = 'True
  IsTuple (t1,t2) = 'True
  IsTuple t       = 'False
  
pathWidget :: forall t m meth r.
             ( MonadWidget t m
             , ToPathParamWidget
                        (PathParam meth r) (IsTuple (PathParam meth r))
             ) => Proxy meth -> Proxy r -> [PathSegment] -> m (Dynamic t (PathParam meth r))
pathWidget meth r pthSegs = do
  topathParamWidget (undefined :: (PathPar t (PathParam meth r) (IsTuple (PathParam meth r)))) pthSegs
  
class GToWidget f where
  gToWidget :: ( MonadWidget t m
              ) => Proxy (f a) -> m (Dynamic t (f a))

instance (GToWidget f) => GToWidget (D1 c f) where
  gToWidget _ = mapDyn M1 =<< (gToWidget Proxy)

instance (GToWidget f, GToWidget g) => GToWidget (f :+: g) where
  gToWidget _ = mapDyn L1 =<< (gToWidget Proxy)

instance (GToWidget a, GToWidget b) => GToWidget (a :*: b) where
  gToWidget _ = do
    adyn <- gToWidget Proxy
    bdyn <- gToWidget Proxy
    combineDyn (:*:) adyn bdyn

instance (GToWidget f, Constructor c) => GToWidget (C1 c f) where
  gToWidget _ = do
    elClass "div" "constructor" $ do
      text $ conName (undefined :: C1 c f ())
      mapDyn M1 =<< (gToWidget Proxy)
  
instance GToWidget U1 where

instance GToWidget V1 where

instance (GToWidget f, Selector s) => GToWidget (S1 s f) where
  gToWidget _ = do
    elClass "div" "field" $ do
      text $ selName (undefined :: S1 s f ())
      inp <- gToWidget Proxy
      mapDyn M1 inp

instance (ToWidget f) => GToWidget (K1 c f) where
  gToWidget _ = mapDyn K1 =<< toWidget Proxy

class ToWidget a where
  toWidget :: MonadWidget t m => Proxy a -> m (Dynamic t a)
  default toWidget :: (MonadWidget t m, Generic a, GToWidget (Rep a)) => Proxy a -> m (Dynamic t a)
  toWidget _ = mapDyn to =<< gToWidget (Proxy :: Proxy (Rep a a))

instance ToWidget Text where
  toWidget _ = do
    txt <- textInput $ def
    mapDyn T.pack $ _textInput_value txt
    
instance ToWidget Int where
  toWidget _ = do
    txt <- textInput $ def & textInputConfig_inputType .~ "number"
    mapDyn read =<< (holdDyn "0" $ _textInput_input txt)

instance ToWidget Bool where
  toWidget _ = do
    chk <- checkbox False def
    return $ _checkbox_value chk

instance ToWidget () where
  toWidget _ = constDyn <$> divClass "unit-widget" (return ())

instance ToWidget a => ToWidget (Maybe a) where
  toWidget _ = mapDyn Just =<< toWidget (Proxy :: Proxy a)

class ToPathParamWidget (par :: *) (isTup :: Bool) where
  topathParamWidget :: MonadWidget t m => PathPar t par isTup -> [PathSegment] -> m (Dynamic t par)

instance ToWidget par => ToPathParamWidget par 'False where
  topathParamWidget _ pthSegs = do
    let (spths1, _:pthSegs1) = break (==Hole) pthSegs
    forM spths1 $ \spth -> staticPthWid True spth 
    _1Wid <- toWidget (Proxy :: Proxy par)
    case break (==Hole) pthSegs1 of
      (spths2, []) -> forM spths2 $ \spth -> staticPthWid False spth
      (_,pths)     -> error "No. of Holes Invariant violated @ non-tuple case"
    return _1Wid

instance ToPathParamWidget () 'True where
  topathParamWidget _ pthSegs = emptyParamWidget

instance (ToWidget t1, ToWidget t2) =>  ToPathParamWidget (t1, t2) 'True where
  topathParamWidget _ pthSegs = do
    let (spths1, _:pthSegs1) = break (==Hole) pthSegs
    forM spths1 $ \spth -> staticPthWid True spth
    _1Wid <- toWidget (Proxy :: Proxy t1)
    let (spths2, _:pthSegs2) = break (==Hole) pthSegs1
    forM spths2 $ \spth -> staticPthWid True spth
    _2Wid <- toWidget (Proxy :: Proxy t2)
    case break (==Hole) pthSegs2 of
      (spths3, []) -> forM spths3 $ \spth -> staticPthWid False spth
      (_,pths)     -> error "No. of Holes Invariant violated @ (,) case"
    combineDyn (,) _1Wid _2Wid

instance ( ToWidget t1
         , ToWidget t2
         , ToWidget t2
         , ToWidget t3
         ) =>  ToPathParamWidget (t1, t2, t3) 'True where
  topathParamWidget _ pthSegs = do
    let (spths1, _:pthSegs1) = break (==Hole) pthSegs
    forM spths1 $ \spth -> staticPthWid True spth
    _1Wid <- toWidget (Proxy :: Proxy t1)
    let (spths2, _:pthSegs2) = break (==Hole) pthSegs1
    forM spths2 $ \spth -> staticPthWid True spth
    _2Wid <- toWidget (Proxy :: Proxy t2)
    let (spths3, _:pthSegs3) = break (==Hole) pthSegs2
    forM spths3 $ \spth -> staticPthWid True spth
    _3Wid <- toWidget (Proxy :: Proxy t3)
    case break (==Hole) pthSegs3 of
      (spths4, []) -> forM spths4 $ \spth -> staticPthWid False spth
      (_,pths)     -> error "No. of Holes Invariant violated @ (,,) case"
    combineDyn3 (,,)  _1Wid _2Wid _3Wid
                  
combineDyn3 :: (Reflex t, MonadHold t m) => (a -> b -> c -> d) -> Dynamic t a -> Dynamic t b -> Dynamic t c -> m (Dynamic t d)
combineDyn3 f da db dc = (combineDyn (\c (a, b) -> f a b c) dc) =<< (combineDyn (,) da db)

combineDyn4 :: (Reflex t, MonadHold t m) => (a -> b -> c -> d -> e) -> Dynamic t a -> Dynamic t b -> Dynamic t c -> (Dynamic t d) -> m (Dynamic t e)
combineDyn4 f da db dc dd = (combineDyn (\d (a, b, c) -> f a b c d) dd) =<< (combineDyn3 (,,) da db dc)

combineDyn5 :: (Reflex t, MonadHold t m) => (a -> b -> c -> d -> e -> f) -> Dynamic t a -> Dynamic t b -> Dynamic t c -> (Dynamic t d) -> Dynamic t e -> m (Dynamic t f)
combineDyn5 f da db dc dd de = (combineDyn (\e (a, b, c, d) -> f a b c d e) de) =<< (combineDyn4 (,,,) da db dc dd)

combineDyn6 :: (Reflex t, MonadHold t m) => (a -> b -> c -> d -> e -> f -> g) -> Dynamic t a -> Dynamic t b -> Dynamic t c -> (Dynamic t d) -> Dynamic t e -> Dynamic t f -> m (Dynamic t g)
combineDyn6 fn da db dc dd de df = (combineDyn (\f (a, b, c, d, e) -> fn a b c d e f) df) =<< (combineDyn5 (,,,,) da db dc dd de)

combineDyn7 :: (Reflex t, MonadHold t m) => (a -> b -> c -> d -> e -> f -> g -> h) -> Dynamic t a -> Dynamic t b -> Dynamic t c -> (Dynamic t d) -> Dynamic t e -> Dynamic t f -> Dynamic t g -> m (Dynamic t h)
combineDyn7 fn da db dc dd de df dg = (combineDyn (\g (a, b, c, d, e, f) -> fn a b c d e f g) dg) =<< (combineDyn6 (,,,,,) da db dc dd de df)
                  
emptyParamWidget ::  MonadWidget t m => m (Dynamic t ())
emptyParamWidget = return $ constDyn ()

staticPthWid :: MonadWidget t m => Bool -> PathSegment -> m ()
staticPthWid sep (StaticSegment spth) = text $ (T.unpack spth) ++ if sep then "/" else ""
staticPthWid _ Hole = error "Invariant Violated @staticPthWid! Found Dynamic Hole"


