{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, TypeOperators, TypeFamilies, GADTs, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, OverloadedLists, DefaultSignatures, StandaloneDeriving, StaticPointers #-}
{-# LANGUAGE RecursiveDo #-}
-- |

module WebApi.Console where

import Data.JSString ()
import qualified Data.Map as M
import Data.Maybe
import Data.Tree
import GHCJS.Types
import Reflex.Dom as RD
import Data.Either
import Data.Proxy
import Data.Aeson as A hiding (Success)
import Data.Typeable
import WebApi
import GHC.Exts
import Network.URI
import Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as ASCII
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Map.Lazy as LM
import           Network.HTTP.Media                    (mapContentMedia)
import Network.HTTP.Types
import Network.HTTP.Types.URI
import Network.URI
import Control.Monad
import Control.Monad.IO.Class
import GHC.Generics
import WebApi.Client
import WebApi.Internal (getContentType)
import WebApi.PageTemplate
--import WebApi.Assertion hiding (ToWidget)
import Data.Map (Map)
import Debug.Trace
import Reflex.Utils
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Functor.Contravariant
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Rank1Dynamic as R1D (Dynamic)
import Data.Rank1Dynamic hiding (Dynamic)
import Data.Rank1Typeable hiding (V1)
import Control.Exception

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
                       , Generic (ApiOut m r)
                       --, GAssert (Rep (ApiOut m r))
                       , SelectorName (ApiOut m r)
                       , Assert (ApiOut m r)
                       , Generic (ApiErr m r)
                       --, GAssert (Rep (ApiErr m r))
                       , Assert (ApiErr m r)
                       , HeaderOut m r ~ ()
                       , CookieOut m r ~ ()
                       , RequestBody m r ~ '[]
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
      getPathSegs :: forall t m rs.MonadWidget t m => SingRoute api rs -> [(Method, Text, (Event t () -> m (Event t ())))]
      getPathSegs (RouteCons pxyM pxyR rs)
        = let routeTpl = T.intercalate "/" $ mkRouteTplStr pathSegs (getDynTyRep $ getPathParPxy pxyM pxyR)
              pathSegs = mkPathFormatString pxyR
          in ( singMethod pxyM
             , routeTpl
             , (\x -> paramWidget api pxyM pxyR (baseURI config) pathSegs x)
             ) : (getPathSegs rs)
      getPathSegs RouteNil           = []
      routeSegs = getPathSegs routes
      getPathParPxy :: Typeable (PathParam m r) => Proxy m -> Proxy r -> Proxy (PathParam m r)
      getPathParPxy _ _ = Proxy
  mainWidgetWithHead pageTemplate $ do
    el "main" $ do
      el "header" $ do
        el "h1" $ text "WebApi Console"
        el "small" $ text "v0.1.0"
      divClass "apiconsole" $ do
        dropdownTabDisplay "route-tab" "active-route-tab" $ LM.fromList $ (flip map) routeSegs $
          \(m, r, paramWid) -> ((show (m, r)), ((show (m, r)), (void . paramWid)))
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
               , Decodings (ContentTypes meth r) (ApiOut meth r)
               , Decodings (ContentTypes meth r) (ApiErr meth r)
               , Generic (ApiOut meth r)
               , SelectorName (ApiOut meth r)
               , Assert (ApiOut meth r)
               , Generic (ApiErr meth r)
               , Assert (ApiErr meth r)
               , HeaderOut meth r ~ ()
               , CookieOut meth r ~ ()
               , RequestBody meth r ~ '[]
               , ConsoleCtx api meth r
               ) => Proxy api -> Proxy meth -> Proxy r -> URI -> [PathSegment] -> Event t () -> m (Event t ())
paramWidget api meth r baseUrl pathSegs onSubmit = divClass "box-wrapper" $ do
  onReq <- divClass "request-form" $ do
    let methDyn = constDyn $ decodeUtf8 $ singMethod (Proxy :: Proxy meth)
    rec
      divClass "request-url" $ dynText urlDyn
      (urlDyn, encodedFormParDyn) <- divClass "params" $ do
        queryWidget <- divClass "param-type-wrapper query" $ do
          --divClass "param-type query" $ return ()
          toWidget (Proxy :: Proxy (QueryParam meth r))
        formWidget <- divClass "param-type-wrapper form" $ do
          --divClass "param-type" $ text "Form Params"
          toWidget (Proxy :: Proxy (FormParam meth r))
        pthWidget <- divClass "param-type-wrapper path" $ do
          --divClass "param-type" $ text "Path Params"
          pathWidget meth r pathSegs
        headWidget <- divClass "param-type-wrapper headers" $ do
          --divClass "param-type" $ text "Headers"
          toWidget (Proxy :: Proxy (HeaderIn meth r))
        fileWidget <- divClass "param-type-wrapper files" $ do
          --divClass "param-type" $ text "Files"
          toWidget (Proxy :: Proxy (FileParam meth r))
        cookWidget <- divClass "param-type-wrapper cookies" $ do
          --divClass "param-type" $ text "Cookies"
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
    return $ tag (pull $ mkXhrReq methDyn urlDyn encodedFormParDyn) onSubmit
  resE <- performRequestAsyncWithError onReq
  let res = fmap (\r -> case r of
                     Left ex -> Left ex
                     Right r' -> case _xhrResponse_responseText r' of
                       Just txt -> Right (Status (fromIntegral $ _xhrResponse_status r') (encodeUtf8 $ _xhrResponse_statusText r'), txt)
                       Nothing  -> Left (error "EMPTY BODY")
                 ) resE
      mkResponse' (Right (st, respTxt)) = mkResponse st (encodeUtf8 respTxt)
      mkResponse' (Left ex)             = Failure $ Right (OtherError $ toException ex)
      response :: Event t (Response meth r)
      response = mkResponse' <$> res
  divClass "right-pane" $ do
    (respAttr, assrAttr) <- divClass "div-head" $ do
      let defAttr = ("class" =: "tab")
      rec (rsp, _) <- elDynAttr' "div" respAttr' $ text "Response"
          (asr, _) <- elDynAttr' "div" assrAttr $ text "Assertion"
          [respAttr, assrAttr] <- mkSwitchableAttrs [(_el_clicked rsp, defAttr), (_el_clicked asr, defAttr)] []
          respAttr' <- holdDyn ("class" =: "tab active") $ updated respAttr
      return (respAttr', assrAttr)
    elDynAttr "div" respAttr $ do
      divClass "response" $ text "response: "
    elDynAttr "div" assrAttr $ do
      divClass "assert" $ do
        statusAssert <- divClass "status-assert" $ do
          divClass "title-text" $ text "Response"
          divClass "div field" $ do
            dropdown "Status Code" (constDyn (("Status Code" =: "Status Code") :: Map String String)) $ def
              & attributes .~ constDyn ("class" =: "assert-field-select")
            divClass "field-assertion" $ do
              let defKey = "-- No Selection --" :: Text
                  disabledWidgetAttr = ("class" =: "disabled-assert assert-widget")
                  enabledWidgetAttr = ("class" =: "assert-widget")
              dd <- dropdown defKey (constDyn ("==" =: "==")) def
              assertWidgetAttr <- holdDyn disabledWidgetAttr $ fmap (\x -> if x == defKey
                                                                            then disabledWidgetAttr
                                                                            else enabledWidgetAttr) $ _dropdown_change dd
              elDynAttr "div" assertWidgetAttr $ do
                exp <- toWidget (Proxy :: Proxy Int)
                return ()
          divClass "div field" $ do
            dropdown "Status Message" (constDyn (("Status Message" =: "Status Message") :: Map String String)) $ def
              & attributes .~ constDyn ("class" =: "assert-field-select")
            divClass "field-assertion" $ do
              let defKey = "-- No Selection --" :: Text
                  disabledWidgetAttr = ("class" =: "disabled-assert assert-widget")
                  enabledWidgetAttr = ("class" =: "assert-widget")
              dd <- dropdown defKey (constDyn ("==" =: "==")) def
              assertWidgetAttr <- holdDyn disabledWidgetAttr $ fmap (\x -> if x == defKey
                                                                            then disabledWidgetAttr
                                                                            else enabledWidgetAttr) $ _dropdown_change dd
              elDynAttr "div" assertWidgetAttr $ do
                exp <- toWidget (Proxy :: Proxy Text)
                return ()
        (apiOutAssert :: Dynamic t (Predicate (ApiOut meth r))) <- divClass "api-out-assert" $ do
          divClass "title-text inline-block" $ text "Api Out"
          (gAddEvtEl, _) <- elAttr' "span" ("class" =: "plus-button") $ text "+"
          let gAddEvt = _el_clicked gAddEvtEl
              displayNone = ("style" =: "display:none")
          rec let onModalToggle = leftmost [fmap (const True) gAddEvt, fmap (const False) onModalClose, fmap (const False) onAddAssrEvt]
                  modalClass = ("class" =: "modal-wrapper")
              modalAttr <- foldDyn (\x _ -> if x then modalClass else displayNone) displayNone onModalToggle
              (onAddAssrEvt, onModalClose) <- elDynAttr "div" modalAttr $ do
                divClass "assertion-modal" $ do
                  (closeEl, _) <- divClass "modal-header" $ do
                    divClass "title-text inline-block" $ text "Add Assertion"
                    elAttr' "span" ("class" =: "close-modal") $ text "x"
                  divClass "modal-body" $ do
                    let selNameMap = fromList $ map (\x -> ("." <> x, T.unpack x)) $ concat $ map flatten $ selectorNames (Proxy :: Proxy (ApiOut meth r)) []
                    dd <- dropdown "" (constDyn selNameMap) $ def
                          & attributes .~ constDyn ("class" =: "modal-field-select")
                    onAddAssr <- button' "modal-add" "Add +"
                    renderForest $ selectorNames (Proxy :: Proxy (ApiOut meth r)) []
                    return $ (tag (pull (sample . current $ _dropdown_value dd)) onAddAssr, _el_clicked closeEl)
          --mapDyn (contramap from) =<< gAssert (Proxy :: Proxy (Rep (ApiOut meth r) ()))
          toAssert (Proxy :: Proxy (ApiOut meth r)) (GAssertState "" onAddAssrEvt)
        (apiErrAssert :: Dynamic t (Predicate (ApiErr meth r))) <- divClass "api-err-assert" $ do
          text "api-err"
          gAddEvt <- button "+"
          --mapDyn (contramap from) =<< gAssert (Proxy :: Proxy (Rep (ApiErr meth r) ()))
          toAssert (Proxy :: Proxy (ApiErr meth r)) (GAssertState "" (fmap (const "") gAddEvt))
        (headerOutAssert :: Dynamic t (Predicate (HeaderOut meth r))) <- divClass "header-out-assert" $ do
          text "hd-out"
          gAddEvt <- button "+"
          --mapDyn (contramap from) =<< gAssert (Proxy :: Proxy (Rep (HeaderOut meth r) ()))
          toAssert (Proxy :: Proxy (HeaderOut meth r)) (GAssertState "" (fmap (const "") gAddEvt))
        (cookieOutAssert :: Dynamic t (Predicate (CookieOut meth r))) <- divClass "cookie-out-assert" $ do
          text "cook-out"
          gAddEvt <- button "+"
          --mapDyn (contramap from) =<< gAssert (Proxy :: Proxy (Rep (CookieOut meth r) ()))
          toAssert (Proxy :: Proxy (CookieOut meth r)) (GAssertState "" (fmap (const "") gAddEvt))
        let combDyn (outPred, errPred, hdrPred, cookPred) (Success st out hdr cook) =
              let outRes   = (getPredicate outPred) out
                  hdrRes   = (getPredicate hdrPred) hdr
                  cookRes  = (getPredicate cookPred) cook
              in outRes -- && hdrRes && cookRes
            combDyn (outPred, errPred, hdrPred, cookPred) (Failure (Left (ApiError st err hdr cook))) =
              let errRes   = (getPredicate errPred) err
                  hdrRes   = maybe True (getPredicate hdrPred) hdr
                  cookRes  = maybe True (getPredicate cookPred) cook
              in errRes && hdrRes && cookRes
            combDyn _ (Failure (Right _ex)) = False
        preds <- combineDyn4 (,,,) apiOutAssert apiErrAssert headerOutAssert cookieOutAssert
        display =<< holdDyn True (attachDynWith combDyn preds response)
        return ()
  return $ tag (constant ()) onReq
  where
    mkReq pw qw fw fiw hw cw md = Request <$> pw
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
  Just ctype -> let decs = decodings (reproxy r) o
               in maybe (firstRight (map snd decs)) id (mapContentMedia decs ctype)
  Nothing    -> firstRight (map snd (decodings (reproxy r) o))
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
             ( MonadWidget t m
             , ToPathParamWidget
                        (PathParam meth r) (IsTuple (PathParam meth r))
             ) => Proxy meth -> Proxy r -> [PathSegment] -> m (Dynamic t (Maybe (PathParam meth r)))
pathWidget meth r pthSegs = do
  topathParamWidget (undefined :: (PathPar t (PathParam meth r) (IsTuple (PathParam meth r)))) pthSegs

class CtorInfo (f :: * -> *) where
  constructorNames :: proxy f -> [Text]
  constructorNames _ = []

instance CtorInfo f => CtorInfo (D1 c f) where
  constructorNames _ = constructorNames (Proxy :: Proxy f)

instance (CtorInfo x, CtorInfo y) => CtorInfo (x :+: y) where
  constructorNames _ = constructorNames (Proxy :: Proxy x) ++ constructorNames (Proxy :: Proxy y)

instance (Constructor c, CtorInfo f) => CtorInfo (C1 c f) where
  constructorNames _ = [T.pack $ conName (undefined :: t c f a)]

instance (Selector s, CtorInfo f) => CtorInfo (S1 s f) where

instance CtorInfo (K1 i c) where

instance CtorInfo (f :*: g) where

instance CtorInfo U1 where

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
    let ctorNames = constructorNames (Proxy :: Proxy f)
    gopts' <- case def of
      Just dynM1 -> ((GToWidgetOpts Nothing) . Just) <$> mapDyn (\(M1 a) -> a) dynM1
      _           -> return $ GToWidgetOpts Nothing Nothing
    case ctorNames of
      (_:_:_) -> do  -- SumType
        divClass "sum-wrapper" $ do
          sumTyInfo <- forM ctorNames (\cname -> do
            evt <- button (T.unpack cname)
            return (cname, fmap (const cname) evt)
            )
          attrList <- mkSwitchableAttrs (map (\x -> (snd x, ("class" =: "sum-ty"))) sumTyInfo) []
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

instance (GToWidget f, GToWidget g, Typeable g, CtorInfo f, Constructor c) => GToWidget (f :+: C1 c g) where
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

instance (GToWidget f, Typeable f, Constructor c) => GToWidget (C1 c f) where
  gToWidget gopts = do
    let gopts' = GToWidgetOpts Nothing Nothing
    case eqT :: Maybe (f :~: U1) of
      Just Refl -> do
        mapDyn (fmap M1) =<< (gToWidget gopts')
      _ -> do
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

instance ToWidget a => ToWidget [a] where
  toWidget _ = do
    divClass "list-wrapper" $ do
      rec dynValMap <- listWithKeyShallowDiff (M.empty :: M.Map Int ()) (leftmost evtList) createWidget
          let setNothingAt i = do
                valMap <- sample. current $ dynValMap
                return $ Just $ M.mapWithKey (\k a -> if k == i then Nothing else Just ()) valMap
              addElement = do
                valMap <- sample. current $ dynValMap
                lastKey <- sample . current $ lastKeyD
                return $ M.insert (lastKey + 1) (Just ()) $ M.map (const (Just ())) valMap
          dynListWithKeys <- mapDyn M.toList dynValMap
          dynValMap' <- mapDyn (M.map fst) dynValMap
          let getLastKey (x :: [Int]) = if null x then (-1) else maximum x
          lastKeyD <- mapDyn (getLastKey . map fst) dynListWithKeys
          (_, evtsD) <- splitDyn =<< mapDyn (unzip . map snd) dynListWithKeys
          let modelD = joinDynThroughMap dynValMap'
          evts <- mapDyn leftmost evtsD -- Remove events
          let evtList = (tag (pull addElement) addEvt) : [(push setNothingAt $ switchPromptlyDyn evts)]
          addEvt <- button "+"
      mapDyn (sequence . (map snd) . M.toList) modelD
    where
      fn :: MonadSample t m => [Dynamic t (Maybe a)] -> m (Maybe [a])
      fn = (\model -> do
        model' <- mapM (sample . current) model
        return $ sequence model'
        )
      createWidget k _ _ = initNew k
      initNew :: (MonadWidget t m , ToWidget a) => Int -> m (Dynamic t (Maybe a), Event t Int)
      initNew i = do
        mDyn   <- toWidget (Proxy :: Proxy a)
        onRemove <- button "x"
        mDyn' <- mapDyn (maybe [] (: [])) mDyn
        return (mDyn, tag (constant i) onRemove)

instance ToWidget a => ToWidget (Maybe a) where
  toWidget _ = do
    widget <- toWidget (Proxy :: Proxy a)
    (e, _) <- el' "button" $
      text "Toggle"
    isActive <- toggle True (_el_clicked e)
    combineDyn (\a b -> fmap (\x -> if a then Just x else Nothing) b) isActive widget

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

class GSelectorName (f :: * -> *) where
  getFieldNames :: Proxy f -> Forest Text -> Forest Text

instance GSelectorName f => GSelectorName (D1 c f) where
  getFieldNames _ acc = getFieldNames (Proxy :: Proxy f) acc

instance GSelectorName (f :+: g) where
  getFieldNames _ _ = [] -- TODO:

instance (GSelectorName a, GSelectorName b) => GSelectorName (a :*: b) where
  getFieldNames _ acc = acc <> getFieldNames (Proxy :: Proxy a) [] <> getFieldNames (Proxy :: Proxy b) acc

instance GSelectorName U1 where
  getFieldNames _ acc = acc

instance (GSelectorName f) => GSelectorName (C1 c f) where
  getFieldNames _ acc = getFieldNames (Proxy :: Proxy f) acc

instance (Selector s, SelectorName f) => GSelectorName (S1 s (K1 c f)) where
  getFieldNames _ _ = let sName = T.pack $ selName (undefined :: S1 s (K1 c f) ())
                          prefixSName (Node x xs) = Node (sName <> "." <> x) (map prefixSName xs)
                      in [Node sName (map prefixSName $ selectorNames (Proxy :: Proxy f) [])]

class SelectorName f where
  selectorNames :: Proxy f -> Forest Text -> Forest Text
  default selectorNames :: (GSelectorName (Rep f)) => Proxy f -> Forest Text -> Forest Text
  selectorNames _ acc = getFieldNames (Proxy :: Proxy (Rep f)) acc

instance SelectorName Text where
  selectorNames _ acc = acc

instance SelectorName Int where
  selectorNames _ acc = acc

instance SelectorName Bool where
  selectorNames _ acc = acc

instance SelectorName () where
  selectorNames _ acc = acc

instance (SelectorName a) => SelectorName [a] where
  selectorNames _ acc = selectorNames (Proxy :: Proxy a) acc

instance (SelectorName a) => SelectorName (Maybe a) where
  selectorNames _ acc = selectorNames (Proxy :: Proxy a) acc

data GAssertState t = GAssertState
  { fieldNameAcc :: Text
  , globalAddEvent :: Event t Text
  }

class GAssert f where
  gAssert :: (MonadWidget t m) => Proxy (f a) -> GAssertState t -> m (Dynamic t (Predicate (f a)))

instance GAssert f => GAssert (D1 c f) where
  gAssert _ gs = do
    mapDyn (contramap unM1) =<< gAssert Proxy gs

instance GAssert (f :+: g) where
  gAssert _ _ = do
    text "<<TODO: SUM TYPE>>"
    return $ constDyn $ Predicate (\_ -> True)

instance (GAssert a, GAssert b) => GAssert (a :*: b) where
  gAssert _ gs = do
    assertA <- gAssert Proxy gs
    assertB <- gAssert Proxy gs
    let fstPrd (f :*: s) = f
        sndPrd (f :*: s) = s
    aDyn <- mapDyn (contramap fstPrd) assertA
    bDyn <- mapDyn (contramap sndPrd) assertB
    combineDyn (\a b -> Predicate $ \prd -> ((getPredicate a) prd) && ((getPredicate b) prd)) aDyn bDyn
instance GAssert U1 where
  gAssert _ _ = return $ constDyn $ Predicate (\_ -> True)

instance (GAssert f, Typeable f, Constructor c) => GAssert (C1 c f) where
  gAssert _ gs = do
    case eqT :: Maybe (f :~: U1) of
      Just Refl -> do
        mapDyn (contramap unM1) =<< gAssert Proxy gs
      _ -> do
        elClass "fieldset" "nested-field field" $ do
          el "legend" $ text $ conName (undefined :: C1 c f ())
          divClass "field" $ mapDyn (contramap unM1) =<< gAssert Proxy gs

instance (GAssert f, Selector s, f ~ (K1 c f1), ToWidget f1, Typeable f1, Assert f1) => GAssert (S1 s f) where
  gAssert _ (GAssertState fname gAddEvt) = do
    let fldName = fname <> "." <> T.pack sName
        sName = selName (undefined :: S1 s f ())
    (onFieldShow, onFieldAdd) <- headTailE $ push (ifFieldIs fldName) gAddEvt
    putDebugLnE onFieldShow (\x -> T.unpack fldName)
    let onFieldPrefix = push (ifFieldHas fldName) gAddEvt
    p1 <- listWidget onFieldShow onFieldAdd createWidget
    p2 <- createAssertWidget onFieldPrefix
    combineDyn (\a b -> Predicate $ \prd -> ((getPredicate a) prd) && ((getPredicate b) prd)) p1 p2
    where
      showWidgetOn = foldDyn (\x -> const M.empty) ("style" =: "display:none")
      toggleWidgetOn showEvt hideEvt = foldDyn (\x _ -> if x then M.empty else ("style" =: "display:none")) ("style" =: "display:none") $ leftmost [fmap (const True) showEvt, fmap (const False) hideEvt]
      ifFieldHas x y = if T.isPrefixOf x y && x /= y then return (Just ()) else return Nothing
      ifFieldIs x y = if x == y then return (Just ()) else return Nothing
      listWidget wgtShowEvt onFieldAdd createWidget = do
        wgtShowAttr <- showWidgetOn wgtShowEvt
        elDynAttr "div" wgtShowAttr $ do
          (addEvtEl, _) <- elAttr' "span" ("class" =: "plus-button float-right") $ text "+"
          rec dynValMap <- listWithKeyShallowDiff ((0 =: ()) :: M.Map Int ()) (leftmost evtList) createWidget
              let setNothingAt i = do
                    valMap <- sample. current $ dynValMap
                    return $ Just $ M.mapWithKey (\k a -> if k == i then Nothing else Just ()) valMap
                  addElement = do
                    valMap <- sample. current $ dynValMap
                    lastKey <- sample . current $ lastKeyD
                    return $ M.insert (lastKey + 1) (Just ()) $ M.map (const (Just ())) valMap
              dynListWithKeys <- mapDyn M.toList dynValMap
              dynValMap' <- mapDyn (M.map fst) dynValMap
              let getLastKey (x :: [Int]) = if null x then (-1) else maximum x
              lastKeyD <- mapDyn (getLastKey . map fst) dynListWithKeys
              (_, evtsD) <- splitDyn =<< mapDyn (unzip . map snd) dynListWithKeys
              let modelD = joinDynThroughMap dynValMap'
              evts <- mapDyn leftmost evtsD -- Remove events
              let evtList = (tag (pull addElement) addEvt) : [(push setNothingAt $ switchPromptlyDyn evts)]
                  addEvt = leftmost [_el_clicked addEvtEl, onFieldAdd]
          mapDyn (combinePredicates . (map snd) . M.toList) modelD
      createWidget k _ _ = do
        el "div" $ do
          let sName = selName (undefined :: S1 s f ())
          dropdown sName (constDyn (sName =: sName)) $ def
            & attributes .~ constDyn ("class" =: "assert-field-select")
          (removeEl, _) <- elAttr' "span" ("class" =: "assert-remove") $ text "-"
          dyn <- divClass "field-assertion" $ do
            let defKey = "-- No Selection --" :: Text
                disabledWidgetAttr = ("class" =: "disabled-assert assert-widget")
                enabledWidgetAttr = ("class" =: "assert-widget")
                fldname = fname <> "." <> T.pack sName
            dd <- dropdown defKey (constDyn ("==" =: "==")) def
            assertWidgetAttr <- holdDyn disabledWidgetAttr $ fmap (\x -> if x == defKey
                                                                          then disabledWidgetAttr
                                                                          else enabledWidgetAttr) $ _dropdown_change dd
            let lookupPredFn fnKey = maybe (error "Unknown Fn1") prjFnDyn $ HM.lookup fnKey funTable
            let succeed _ _ = True
            fnDyn <- holdDyn (toDynamic (succeed :: ANY -> ANY -> Bool)) ((lookupPredFn) <$>_dropdown_change dd)
            elDynAttr "div" assertWidgetAttr $ do
              valDyn <- toWidget Proxy
              res <- combineDyn (\fn val -> mkPred fn val) fnDyn valDyn
              mapDyn (contramap unM1 . contramap unK1) res
          return (dyn, tag (constant k) (_el_clicked removeEl))
      createAssertWidget wgtShowEvt = do
        rec wgtShowAttr <- toggleWidgetOn wgtShowEvt rmEvt
            (predDyn, rmEvt) <- elDynAttr "div" wgtShowAttr $ do
              let sName = selName (undefined :: S1 s f ())
                  fldname = fname <> "." <> T.pack sName
              dropdown sName (constDyn (sName =: sName)) $ def
                & attributes .~ constDyn ("class" =: "assert-field-select")
              (removeEl, _) <- elAttr' "span" ("class" =: "assert-remove") $ text "-"
              --elAttr "label" ("class" =: "label") $ text $ selName (undefined :: S1 s f ())
              dyn <- divClass "field-assertion" $ do
                el "div" $ do
                  mapDyn (contramap unM1 . contramap unK1) =<< toAssert (Proxy :: Proxy f1) (GAssertState (fname <> "." <> T.pack sName) gAddEvt)
              return (dyn, _el_clicked removeEl)
        return predDyn
      mkPred :: R1D.Dynamic -> Maybe f1 -> Predicate f1
      mkPred fn Nothing  = Predicate $ const True
      mkPred fn (Just w) = Predicate $ \v -> unsafeRight $ do
        pred <- fn `dynApply` (toDynamic v)
        res  <- pred `dynApply` (toDynamic w)
        fromDynamic res
      unsafeRight (Right a) = a
      unsafeRight _         = error "Expecting only right"

instance (ToWidget f, Typeable f, Assert f) => GAssert (K1 c f) where
  {-gAssert _ gs@(GAssertState fname gAddEvt) = do

    mapDyn ((contramap unK1) . mkPred) =<< toWidget Proxy
      where mkPred Nothing  = Predicate $ const False
            mkPred (Just p) = p

instance (ToWidget a, Eq a) => ToWidget (Predicate a) where
  toWidget _ = do
    mapDyn mkPred =<< toWidget (Proxy :: Proxy a)
      where mkPred Nothing  = Nothing
            mkPred (Just w) = Just $ Predicate $ \v -> v == w
-}

class Assert a where
  toAssert :: (MonadWidget t m) => Proxy a -> GAssertState t -> m (Dynamic t (Predicate a))
  default toAssert :: (MonadWidget t m, Generic a, GAssert (Rep a)) => Proxy a -> GAssertState t -> m (Dynamic t (Predicate a))
  toAssert _ gs = mapDyn (contramap from) =<< gAssert Proxy gs

instance Assert Text where
  toAssert _ _ = return $ constDyn $ Predicate (\_ -> True)

instance Assert Int where
  toAssert _ _ = return $ constDyn $ Predicate (\_ -> True)

instance Assert Bool where
  toAssert _ _ = return $ constDyn $ Predicate (\_ -> True)

instance Assert () where
  toAssert _ _ = return $ constDyn $ Predicate (\_ -> True)

instance Assert [a] where
  toAssert _ gs = mapDyn (contramap from) =<< gAssert Proxy gs

instance Assert (Maybe a) where
  toAssert _ gs = mapDyn (contramap from) =<< gAssert Proxy gs

data WidgetBox = forall a.(ToWidget a) => WidgetBox (Proxy a)

instance Show WidgetBox where
  show wb = "<<WidgetBox>>"

data PrjFnInfo = PrjFnInfo
  { prjFnDyn :: R1D.Dynamic
  , prjResWidget :: WidgetBox
  , prjFnModName :: String
  , prjFnPkgKey  :: String
  } deriving (Show)

type PrjMap = HashMap Text PrjFnInfo

data PrjVal = forall v.(Typeable v, ToWidget v) => Val v
            | ProjectedVal (R1D.Dynamic, PrjFnInfo) PrjVal

funTable :: PrjMap
funTable = HM.fromList
  [ ("Data.List.length", PrjFnInfo (toDynamic (length :: [ANY] -> Int)) (WidgetBox (Proxy :: Proxy Int)) "Data.List" "base")
  , ("==", PrjFnInfo (toDynamic ((==) :: Int -> Int -> Bool)) (WidgetBox (Proxy :: Proxy Bool)) "GHC.Classes" "base")
  ]

apply :: Text -> PrjVal -> Either String PrjVal
apply fnKey val@(Val v) = do
  prjFnInf <- maybe (Left "Unknown Fn") Right $ HM.lookup fnKey funTable
  prjRes <- (prjFnDyn prjFnInf) `dynApply` (toDynamic v)
  return $ ProjectedVal (prjRes, prjFnInf) val
apply fnKey pVal@(ProjectedVal pRes _) = do
  prjFnInf <- maybe (Left "Unknown Fn") Right $ HM.lookup fnKey funTable
  prjRes <- (prjFnDyn prjFnInf) `dynApply` (fst pRes)
  return $ ProjectedVal (prjRes, prjFnInf) pVal

unapply :: PrjVal -> PrjVal
unapply v@(Val _) = v
unapply (ProjectedVal _ nextVal) = nextVal


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
      body = ASCII.unpack fp
  return $ XhrRequest (T.unpack meth) url
            $ def { _xhrRequestConfig_headers = headerUrlEnc
                  , _xhrRequestConfig_sendData = Just body
                  }

combinePredicates :: [Predicate a] -> Predicate a
combinePredicates xs = Predicate $ \x -> all id $ map (($ x) . getPredicate) xs
