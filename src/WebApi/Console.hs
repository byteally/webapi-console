{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, TypeOperators, TypeFamilies, GADTs, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, OverloadedLists, DefaultSignatures, StandaloneDeriving #-}
{-# LANGUAGE RecursiveDo #-}
-- | 

module WebApi.Console where

import Data.JSString ()
import qualified Data.Map as M
import Data.Maybe
import GHCJS.Types
import Reflex.Dom as RD
import Data.Proxy
import Data.Aeson as A
import Data.Typeable
import WebApi
import GHC.Exts
import Network.URI
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as ASCII
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Map.Lazy as LM
import Network.HTTP.Types
import Network.HTTP.Types.URI
import Network.URI
import Control.Monad
import GHC.Generics
import WebApi.PageTemplate
import Data.Map (Map)
import Debug.Trace
import Reflex.Utils

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
             , paramWidget api pxyM pxyR (baseURI config) pathSegs
             ) : (getPathSegs rs)
      getPathSegs RouteNil           = []
      routeSegs = getPathSegs routes
      getPathParPxy :: Typeable (PathParam m r) => Proxy m -> Proxy r -> Proxy (PathParam m r)
      getPathParPxy _ _ = Proxy
  mainWidgetWithHead pageTemplate $ do
    el "main" $ do
        el "h1" $ text "Api Console"
        divClass "apiconsole" $ do
            dropdownTabDisplay "route-tab" "active-route-tab" $ LM.fromList $ (flip map) routeSegs $  
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
               ) => Proxy api -> Proxy meth -> Proxy r -> URI -> [PathSegment] -> m (Event t ())
paramWidget api meth r baseUrl pathSegs = divClass "" $ do
  onReq <- divClass "request-form" $ do
    let methDyn = constDyn $ decodeUtf8 $ singMethod (Proxy :: Proxy meth)        
    rec 
      divClass "request-url" $ dynText urlDyn
      (urlDyn, encodedFormParDyn) <- divClass "params" $ do
        queryWidget <- el "div" $ do
          divClass "param-type" $ text "Query Params"
          toWidget (Proxy :: Proxy (QueryParam meth r))
        formWidget <- el "div" $ do
          divClass "param-type" $ text "Form Params"
          toWidget (Proxy :: Proxy (FormParam meth r))
        pthWidget <- el "div" $ do
          divClass "param-type" $ text "Path Params"
          pathWidget meth r pathSegs
        headWidget <- el "div" $ do
          divClass "param-type" $ text "Headers"
          toWidget (Proxy :: Proxy (HeaderIn meth r))
        fileWidget <- el "div" $ do
          divClass "param-type" $ text "Files"
          toWidget (Proxy :: Proxy (FileParam meth r))
        cookWidget <- el "div" $ do
          divClass "param-type" $ text "Cookies"
          toWidget (Proxy :: Proxy (CookieIn meth r))
        (requestDyn :: Dynamic t (Maybe (Request meth r))) <- combineDyn7 mkReq
                                                                pthWidget
                                                                queryWidget
                                                                formWidget
                                                                fileWidget
                                                                headWidget
                                                                cookWidget
                                                                methDyn
        formParMapDyn <- mapDyn (fmap (toFormParam . formParam)) requestDyn
        encodedFormParDyn <- mapDyn ((fromMaybe "") . fmap (renderSimpleQuery False)) formParMapDyn
        linkDyn <- mapDyn ((fromMaybe "") . (fmap (\req -> show $ WebApi.link (undefined :: meth, undefined :: r) baseUrl (pathParam req) (Just $ queryParam req)))) requestDyn
        return (linkDyn, encodedFormParDyn)
      -- display encodedFormParDyn
    divClass "submit-button" $ do
      fire <- button "Fire"
      return $ tag (pull $ mkXhrReq methDyn urlDyn encodedFormParDyn) fire
  divClass "response" $ do
    text "Response"
    res <- performRequestAsync onReq
    status <- holdDyn "" (fmap (show . _xhrResponse_status) res)
    divClass "" $ do
      text "status: "
      dynText status
    (responseText :: Dynamic t (Maybe Text)) <- holdDyn Nothing (fmap (_xhrResponse_responseText) res)
    divClass "" $ do
      text "response: "
      display responseText
  return $ tag (constant ()) onReq
  where
    mkReq pw qw fw fiw hw cw md = Req <$> pw 
                                      <*> qw
                                      <*> fw
                                      <*> fiw
                                      <*> hw
                                      <*> cw
                                      <*> pure md

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
             ) => Proxy meth -> Proxy r -> [PathSegment] -> m (Dynamic t (Maybe (PathParam meth r)))
pathWidget meth r pthSegs = do
  topathParamWidget (undefined :: (PathPar t (PathParam meth r) (IsTuple (PathParam meth r)))) pthSegs
  
type IsEmpty = Bool

class CtorInfo (f :: * -> *) where
  constructorInfo  :: proxy f -> [(Text, IsEmpty)]
  constructorNames :: proxy f -> [Text]
  constructorNames _ = [""]

instance CtorInfo f => CtorInfo (D1 c f) where
  constructorInfo _  = constructorInfo (Proxy :: Proxy f)
  constructorNames _ = constructorNames (Proxy :: Proxy f)

instance (CtorInfo x, CtorInfo y) => CtorInfo (x :+: y) where
  constructorInfo _ = constructorInfo (Proxy :: Proxy x) ++ constructorInfo (Proxy :: Proxy y)
  constructorNames _ = constructorNames (Proxy :: Proxy x) ++ constructorNames (Proxy :: Proxy y)

instance (Constructor c, CtorInfo f) => CtorInfo (C1 c f) where
  constructorInfo _ = [(T.pack $ conName (undefined :: t c f a), isEmpty)]
   where [(_, isEmpty)] = constructorInfo (Proxy :: Proxy f)
  constructorNames _ = [T.pack $ conName (undefined :: t c f a)]

instance (Selector s, CtorInfo f) => CtorInfo (S1 s f) where
  constructorInfo _ = [("", False)]

instance CtorInfo (K1 i c) where
  constructorInfo _ = [("", False)]

instance CtorInfo (f :*: g) where
  constructorInfo _ = [("", False)]

instance CtorInfo U1 where
  constructorInfo _ = [("", True)]

type DynamicAttr t = Dynamic t (Map String String)

data GToWidgetState t = GToWidgetState
  { st_constructors :: M.Map Text ((Event t Text), DynamicAttr t)
  }

data GToWidgetOpts t f a = GToWidgetOpts
  { state :: Maybe (GToWidgetState t)
  , arbitraryDef :: Maybe (Dynamic t (f a))
  }

class GToWidget f where
  gToWidget :: ( MonadWidget t m
              ) => GToWidgetOpts t f a -> m (Dynamic t (Maybe (f a)))

instance (GToWidget f, CtorInfo f) => GToWidget (D1 c f) where
  gToWidget (GToWidgetOpts _ def) = do
    let ctorInfo = constructorInfo (Proxy :: Proxy f)
    gopts' <- case def of
      Just dynM1 -> ((GToWidgetOpts Nothing) . Just) <$> mapDyn (\(M1 a) -> a) dynM1
      _           -> return $ GToWidgetOpts Nothing Nothing
    case ctorInfo of
      (_:_:_) -> do  -- SumType
        sumTyInfo <- forM ctorInfo (\(txt, b) -> do
          evt <- button (T.unpack txt)
          return (txt, fmap (const txt) evt)
          )
        attrList <- mkSwitchableAttrs (map snd sumTyInfo) []
        let sumTyInfoMap = M.fromList (zipWith (\(a, b) c -> (a, (b, c))) sumTyInfo attrList)
        mapDyn (fmap M1) =<< gToWidget gopts' { state = Just $ GToWidgetState sumTyInfoMap }
      _ -> mapDyn (fmap M1) =<< gToWidget gopts'

instance (GToWidget f, GToWidget g, CtorInfo f, GToWidget (g :+: h)) => GToWidget (f :+: g :+: h) where
  gToWidget gopts@(GToWidgetOpts gstate def) = do
    let evtMap = 
          case gstate of
            Just (GToWidgetState em) -> em
            _                        -> M.empty
        lConName         = head $ constructorNames (Proxy :: Proxy f)
        (lEvt, lDynAttr) = fromMaybe (error "PANIC!: Constructor lookup failed @ GToWidget (f :+: g)") (M.lookup lConName evtMap)
    lDyn <- mapDyn (fmap L1) =<< do
      elDynAttr "div" lDynAttr $ do
        gToWidget (GToWidgetOpts (Just $ GToWidgetState (M.delete lConName evtMap)) Nothing)
    rDyn <- mapDyn (fmap R1) =<< gToWidget (GToWidgetOpts (Just $ GToWidgetState (M.delete lConName evtMap)) Nothing)

    fmap joinDyn $ foldDyn (\a _ -> if a == lConName then lDyn else rDyn) lDyn (leftmost $ map (fst .snd) (M.toList evtMap))

instance (GToWidget f, GToWidget g, CtorInfo f, Constructor c) => GToWidget (f :+: C1 c g) where
  gToWidget gopts@(GToWidgetOpts gstate def) = do
    let evtMap = 
          case gstate of
            Just (GToWidgetState em) -> em
            _                        -> M.empty
        lConName         = head $ constructorNames (Proxy :: Proxy f)
        (lEvt, lDynAttr) = fromMaybe (error "PANIC!: Constructor lookup failed @ GToWidget (f :+: C1 c f)") (M.lookup lConName evtMap)
        rConName         = T.pack $ conName (undefined :: t c g a)
        (rEvt, rDynAttr) = fromMaybe (error "PANIC!: Constructor lookup failed @ GToWidget (f :+: C1 c f)") (M.lookup rConName evtMap)
    lDyn <- mapDyn (fmap L1) =<< do
      elDynAttr "div" lDynAttr $ do
        gToWidget (GToWidgetOpts (Just $ GToWidgetState (M.delete lConName evtMap)) Nothing)
    rDyn <- mapDyn (fmap R1) =<< do
      elDynAttr "div" rDynAttr $ do
        gToWidget (GToWidgetOpts Nothing Nothing)

    fmap joinDyn $ foldDyn (\a _ -> if a == lConName then lDyn else rDyn) lDyn (leftmost $ map (fst .snd) (M.toList evtMap))

instance (GToWidget a, GToWidget b) => GToWidget (a :*: b) where
  gToWidget gopts = do
    let gopts' = GToWidgetOpts Nothing Nothing
    adyn <- gToWidget gopts'
    bdyn <- gToWidget gopts'
    combineDyn (\a b -> (:*:) <$> a <*> b) adyn bdyn

instance (GToWidget f, Constructor c) => GToWidget (C1 c f) where
  gToWidget gopts = do
    let gopts' = GToWidgetOpts Nothing Nothing
    elClass "fieldset" "nested-field field" $ do
      el "legend" $ text $ conName (undefined :: C1 c f ())
      divClass "field" $ mapDyn (fmap M1) =<< (gToWidget gopts')
  
instance GToWidget U1 where
  gToWidget _ = return $ constDyn (Just U1)

instance GToWidget V1 where
  gToWidget _ = return $ constDyn (error "PANIC!: Unreachable code")

instance (GToWidget f, Selector s) => GToWidget (S1 s f) where
  gToWidget gopts = do
    let gopts' = GToWidgetOpts Nothing Nothing
    elClass "div" "field" $ do
      elAttr "label" ("class" =: "label") $ text $ selName (undefined :: S1 s f ())
      inp <- gToWidget gopts'
      mapDyn (fmap M1) inp

instance (ToWidget f) => GToWidget (K1 c f) where
  gToWidget _ = mapDyn (fmap K1) =<< toWidget Proxy

class ToWidget a where
  toWidget :: MonadWidget t m => Proxy a -> m (Dynamic t (Maybe a))
  default toWidget :: (MonadWidget t m, Generic a, GToWidget (Rep a)) => Proxy a -> m (Dynamic t (Maybe a))
  toWidget _ = mapDyn (fmap to) =<< gToWidget (GToWidgetOpts Nothing Nothing)

instance ToWidget Text where
  toWidget _ = do
    txt <- textInput $ def
        & attributes .~ constDyn ("class" =: "text-box")
    mapDyn (decodeParam . ASCII.pack) $ _textInput_value txt
    
instance ToWidget Int where
  toWidget _ = do
    txt <- textInput $ def
        & textInputConfig_inputType .~ "number"
        & attributes .~ constDyn ("class" =: "text-box")
    mapDyn (decodeParam . ASCII.pack) $ _textInput_value txt

instance ToWidget Bool where
  toWidget _ = do
    chk <- checkbox False def
    mapDyn Just $ _checkbox_value chk

instance ToWidget () where
  toWidget _ = emptyParamWidget

instance ToWidget a => ToWidget (Maybe a) where
  toWidget _ = mapDyn Just =<< toWidget (Proxy :: Proxy a)

class ToPathParamWidget (par :: *) (isTup :: Bool) where
  topathParamWidget :: MonadWidget t m => PathPar t par isTup -> [PathSegment] -> m (Dynamic t (Maybe par))

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
    combineDyn (\a b -> (,) <$> a <*> b) _1Wid _2Wid

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
    combineDyn3 (\a b c -> (,,) <$> a <*> b <*> c)  _1Wid _2Wid _3Wid
                  
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
                  
emptyParamWidget ::  MonadWidget t m => m (Dynamic t (Maybe ()))
emptyParamWidget = return $ constDyn $ Just ()

staticPthWid :: MonadWidget t m => Bool -> PathSegment -> m ()
staticPthWid sep (StaticSegment spth) = text $ (T.unpack spth) ++ if sep then "/" else ""
staticPthWid _ Hole = error "Invariant Violated @staticPthWid! Found Dynamic Hole"

mkXhrReq :: Reflex t
         => Dynamic t Text
         -> Dynamic t String
         -> Dynamic t BS.ByteString
         -> PullM t XhrRequest
mkXhrReq methD urlD fpD = do
  meth <- sample . current $ methD
  url  <- sample . current $ urlD
  fp   <- sample . current $ fpD
  let headerUrlEnc = if BS.null fp then M.empty else "Content-type" =: "application/x-www-form-urlencoded"
      body = T.unpack . decodeUtf8 $ fp
  return $ XhrRequest (T.unpack meth) url
            $ def { _xhrRequestConfig_headers = headerUrlEnc
                  , _xhrRequestConfig_sendData = Just body
                  }
