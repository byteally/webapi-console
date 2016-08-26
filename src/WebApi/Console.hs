{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, TypeOperators, TypeFamilies, GADTs, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, OverloadedLists, DefaultSignatures, StandaloneDeriving, StaticPointers, DeriveGeneric, TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
-- |

module WebApi.Console
       (
         module WebApi.Console
--       , module WebApi.Console.TH
       ) where

import Data.JSString ()
import qualified Data.Map as M
import qualified Data.Map.Lazy as LM
import Data.Maybe
import Data.Either (either)
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
import           Data.CaseInsensitive  ( original )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as ASCII
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Map.Lazy as LM
import           Network.HTTP.Media                    (mapContentMedia)
import Network.HTTP.Types
import Network.HTTP.Types.URI
import Network.URI
import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.IO.Class
import GHC.Generics
import WebApi.Client
import WebApi.Internal (getContentType)
import WebApi.PageTemplate
--import WebApi.Console.TH
--import WebApi.Assertion hiding (ToWidget)
import Data.Map (Map)
import Debug.Trace
import Reflex.Utils
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Functor.Contravariant
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.Rank1Dynamic as R1D (Dynamic)
import Data.Rank1Dynamic hiding (Dynamic)
import Data.Rank1Typeable hiding (V1)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Exception
import Data.Either
import Data.Aeson.Encode.Pretty
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

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
                       , ToJSON (ApiOut m r)
                       , SelectorInfo (ApiOut m r)
                       , SelectorInfo (ApiErr m r)
                       , SelectorInfo (HeaderOut m r)
                       , SelectorInfo (CookieOut m r)
                       , AssertWidget (ApiOut m r)
                       , AssertWidget (ApiErr m r)
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
  { assertFunctions :: PrjMap
  } deriving (Show)

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
             , (\x -> paramWidget api pxyM pxyR (baseURI config) (functions config) pathSegs x)
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
          \(m, r, paramWid) -> ((show (m, r)), ((ASCII.unpack m <> ": " <> T.unpack r), (void . paramWid)))
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
mkRouteTplStr [] ["{()}"]                  = []
mkRouteTplStr [] []                        = []
mkRouteTplStr [] treps                     = [] --error $ "Panic: @mkRouteTplStr: more dynamic args found than the required" ++ (show treps)

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
               ( MonadWidget t m
               , ToWidget (QueryParam meth r)
               , ToWidget (FormParam meth r)
               , ToWidget (FileParam meth r)
               , ToWidget (HeaderIn meth r)
               , ToWidget (CookieIn meth r)
               , ToPathParamWidget (PathParam meth r) (IsTuple (PathParam meth r))
               , Decodings (ContentTypes meth r) (ApiOut meth r)
               , Decodings (ContentTypes meth r) (ApiErr meth r)
               , ToJSON (ApiOut meth r) -- TODO: Haack
               , SelectorInfo (ApiOut meth r)
               , SelectorInfo (ApiErr meth r)
               , SelectorInfo (HeaderOut meth r)
               , SelectorInfo (CookieOut meth r)
               , AssertWidget (ApiOut meth r)
               , AssertWidget (ApiErr meth r)
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
paramWidget api meth r baseUrl conFuns pathSegs onSubmit = divClass "box-wrapper" $ do
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
          hideIfUnit :: (Dynamic t (Map String String), Bool) -> Dynamic t (Map String String)
          hideIfUnit (x, False) = x
          hideIfUnit (x, True) =
            let cls = "class" =: "hide"
            in constDyn cls
          paramClass = "class" =: "tab"
      rec attrs@[queryAttr, formAttr, pathAttr, headerAttr, fileAttr, cookieAttr] <-
            mkSwitchableAttrs
              [ (onQuery, paramClass)
              , (onForm, paramClass)
              , (onPath, paramClass)
              , (onHeader, paramClass)
              , (onFile, paramClass)
              , (onCookie, paramClass)
              ] []
          [queryAttr', formAttr', pathAttr', headerAttr', fileAttr', cookieAttr'] <-
            modifyAttr $ zip attrs
              [ isUnitVal (Proxy :: Proxy (IsUnit (QueryParam meth r)))
              , isUnitVal (Proxy :: Proxy (IsUnit (FormParam meth r)))
              , isUnitVal (Proxy :: Proxy (IsUnit (PathParam meth r)))
              , isUnitVal (Proxy :: Proxy (IsUnit (HeaderIn meth r)))
              , isUnitVal (Proxy :: Proxy (IsUnit (FileParam meth r)))
              , isUnitVal (Proxy :: Proxy (IsUnit (CookieIn meth r)))
              ]
          (onQuery, onForm, onPath, onHeader, onFile, onCookie) <- divClass "div-head" $ do
            onQuery <- clickableSpan "Query Params" queryAttr'
            onForm <- clickableSpan "Form Params" formAttr'
            onPath <- clickableSpan "Path Params" pathAttr'
            onHeader <- clickableSpan "HeaderIn" headerAttr'
            onFile <- clickableSpan "File Params" fileAttr'
            onCookie <- clickableSpan "Cookie" cookieAttr'
            return (onQuery, onForm, onPath, onHeader, onFile, onCookie)
      rec
        (queryWidget, formWidget, pthWidget, headWidget, fileWidget, cookWidget) <- divClass "param-form" $ do
          divClass "request-url" $ dynText linkDyn
          queryWidget <- elDynAttr "div" queryAttr' $ do
            --divClass "param-type query" $ return ()
            toWidget (Proxy :: Proxy (QueryParam meth r)) Nothing
          formWidget <- elDynAttr "div" formAttr' $ do
            --divClass "param-type" $ text "Form Params"
            toWidget (Proxy :: Proxy (FormParam meth r)) Nothing
          pthWidget <- elDynAttr "div" pathAttr' $ do
            --divClass "param-type" $ text "Path Params"
            pathWidget meth r pathSegs
          headWidget <- elDynAttr "div" headerAttr' $ do
            --divClass "param-type" $ text "Headers"
            toWidget (Proxy :: Proxy (HeaderIn meth r)) Nothing
          fileWidget <- elDynAttr "div" fileAttr' $ do
            --divClass "param-type" $ text "Files"
            toWidget (Proxy :: Proxy (FileParam meth r)) Nothing
          cookWidget <- elDynAttr "div" cookieAttr' $ do
            --divClass "param-type" $ text "Cookies"
            toWidget (Proxy :: Proxy (CookieIn meth r)) Nothing
          return (queryWidget, formWidget, pthWidget, headWidget, fileWidget, cookWidget)
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
        let toHdrStrs hdrs = M.fromList $ fmap (\(k, v) -> (ASCII.unpack $ original k, ASCII.unpack v)) hdrs
        hdrIn <- mapDyn (fmap (toHdrStrs . toHeader . headerIn)) requestDyn
      return (linkDyn, encodedFormParDyn, hdrIn)
      -- display encodedFormParDyn
    return $ tag (pull $ mkXhrReq methDyn urlDyn encodedFormParDyn hdrIn) onSubmit
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
    rec (respAttr, assrAttr) <- divClass "div-head" $ do
          let defAttr = ("class" =: "tab")
          rec (rsp, _) <- elDynAttr' "div" respAttr' $ text "Response"
              (asr, _) <- elDynAttr' "div" assrAttr $ do
                imgAttr <- mapDyn (\x -> "class" =: (if x then "assert-pass" else "assert-fail")) assertResDyn
                elDynAttr "span" imgAttr $ return ()
                text "Assertion"
              [respAttr, assrAttr] <- mkSwitchableAttrs [(_el_clicked rsp, defAttr), (_el_clicked asr, defAttr)] []
              respAttr' <- holdDyn ("class" =: "tab active") $ updated respAttr
          return (respAttr', assrAttr)
        _ <- elDynAttr "div" respAttr $ do
          divClass "response" $ do
            divClass "title-text" $ text "response: "
            let prettyOut :: ToJSON (ApiOut meth r) => Response meth r -> String
                prettyOut (Success _ out _ _) = ASCII.unpack $ toStrict $ encodePretty out
                prettyOut _                   = "Error getting response"
            el "pre" $ dynText =<< (holdDyn ("{}") $ fmap prettyOut response)
            --dynText =<< (holdDyn ("Loading..") $ fmap (either (const "Error getting response") (T.unpack . snd)) res)
        assertResDyn <- elDynAttr "div" assrAttr $ do
          divClass "assert" $ do
            (statusAssert, statusMsgAssert) <- divClass "status-assert" $ do
              divClass "title-text" $ text "Response"
              statusAssert <- divClass "div field" $ do
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
                    toWidget (Proxy :: Proxy Int) Nothing
              statusMsgAssert <- divClass "div field" $ do
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
                    toWidget (Proxy :: Proxy Text) Nothing
              return (statusAssert, statusMsgAssert)
            let hideIfUnit defCls x = if isUnitVal x then "display-none" else defCls
                apiOutCls = hideIfUnit "api-out-assert" (Proxy :: Proxy (IsUnit (ApiOut meth r)))
                apiErrCls = hideIfUnit "api-err-assert" (Proxy :: Proxy (IsUnit (ApiErr meth r)))
                headerOutCls = hideIfUnit "header-out-assert" (Proxy :: Proxy (IsUnit (HeaderOut meth r)))
                cookieOutCls = hideIfUnit "cookie-out-assert" (Proxy :: Proxy (IsUnit (CookieOut meth r)))
            (apiOutAssert :: Dynamic t (Predicate (ApiOut meth r))) <- divClass apiOutCls $ do
              divClass "title-text inline-block" $ text "Api Out"
              onAddAssrEvt <- addFieldModal (Proxy :: Proxy (ApiOut meth r))
              --mapDyn (contramap from) =<< gAssert (Proxy :: Proxy (Rep (ApiOut meth r) ()))
              assertWidget (Proxy :: Proxy (ApiOut meth r)) (GAssertState "" Nothing onAddAssrEvt conFuns)
            (apiErrAssert :: Dynamic t (Predicate (ApiErr meth r))) <- divClass apiErrCls $ do
              divClass "title-text inline-block" $ text "Api Err"
              onAddAssrEvt <- addFieldModal (Proxy :: Proxy (ApiErr meth r))
              --mapDyn (contramap from) =<< gAssert (Proxy :: Proxy (Rep (ApiErr meth r) ()))
              assertWidget (Proxy :: Proxy (ApiErr meth r)) (GAssertState "" Nothing onAddAssrEvt conFuns)
            (headerOutAssert :: Dynamic t (Predicate (HeaderOut meth r))) <- divClass headerOutCls $ do
              divClass "title-text inline-block" $ text "Header Out"
              onAddAssrEvt <- addFieldModal (Proxy :: Proxy (HeaderOut meth r))
              --mapDyn (contramap from) =<< gAssert (Proxy :: Proxy (Rep (HeaderOut meth r) ()))
              assertWidget (Proxy :: Proxy (HeaderOut meth r)) (GAssertState "" Nothing onAddAssrEvt conFuns)
            (cookieOutAssert :: Dynamic t (Predicate (CookieOut meth r))) <- divClass cookieOutCls $ do
              divClass "title-text inline-block" $ text "Cookie Out"
              onAddAssrEvt <- addFieldModal (Proxy :: Proxy (CookieOut meth r))
              --mapDyn (contramap from) =<< gAssert (Proxy :: Proxy (Rep (CookieOut meth r) ()))
              assertWidget (Proxy :: Proxy (CookieOut meth r)) (GAssertState "" Nothing onAddAssrEvt conFuns)
            let combDyn (statusVal, outPred, errPred, hdrPred, cookPred) (Success st out hdr cook) =
                  let outRes   = (getPredicate outPred) out
                      hdrRes   = (getPredicate hdrPred) hdr
                      cookRes  = (getPredicate cookPred) cook
                      statusCodeRes = maybe True (== statusCode st) statusVal
                  in outRes && statusCodeRes -- && hdrRes && cookRes
                combDyn (statusVal, outPred, errPred, hdrPred, cookPred) (Failure (Left (ApiError st err hdr cook))) =
                  let errRes   = (getPredicate errPred) err
                      hdrRes   = maybe True (getPredicate hdrPred) hdr
                      cookRes  = maybe True (getPredicate cookPred) cook
                      statusCodeRes = maybe True (== statusCode st) statusVal
                  in errRes && hdrRes && cookRes && statusCodeRes
                combDyn _ (Failure (Right _ex)) = False
            preds <- combineDyn5 (,,,,) statusAssert apiOutAssert apiErrAssert headerOutAssert cookieOutAssert
            holdDyn True (attachDynWith combDyn preds response)
        display assertResDyn
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
    addFieldModal :: forall a. (SelectorInfo a) => Proxy a -> m (Event t AddEvtPayload)
    addFieldModal prxy = do
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
                let selNameMap = fromList $ map ((\x -> ("." <> x, T.unpack x)) . selectorName) $ filterSInfo . concat . map flatten $ selectorInfos prxy []
                    filterSInfo []                 = []
                    filterSInfo (SInfo sn st : xs) = SInfo sn st : filterSInfo xs
                    filterSInfo (_:xs)             = filterSInfo xs
                rec
                  onNodeSelect <- elClass "ul" "field-selection-tree" $
                    renderForest $ selectorInfos prxy []
                  dd <- dropdown "" (constDyn selNameMap) $ def
                          & attributes .~ constDyn ("class" =: "modal-field-select")
                          & setValue .~ (fmap (("." <>) . selectorName) onNodeSelect)
                  onAddAssr <- button' "modal-add" "Add +"
                  (addWgtDynList :: [(Text, (Dynamic t (Maybe R1D.Dynamic), Dynamic t GName))]) <- forM (filterSInfo . concat . map flatten $ selectorInfos prxy []) $ \si -> do
                    let displayNone = ("style" =: "display:none") :: Map String String
                    --isVisibleDyn <- holdDyn False $ fmap (== si) onNodeSelect
                    attr <- holdDyn displayNone $ fmap (\x -> if x == si then ("style" =: "") else displayNone) onNodeSelect
                    case selectorType si of
                      WidgetBox p -> do
                        elDynAttr "div" attr $ do
                          let funTable' = funTable
                              dropDownKV (n, fnInfo) = (n, displayName fnInfo)
                          funcDd <- dropdown defAssertFnKey (constDyn (LM.fromList $ fmap dropDownKV $ HM.toList funTable')) def
                          dyn <- mapDyn (fmap toDynamic) =<< renderAssertWidget p Nothing
                          return (selectorName si, (dyn, _dropdown_value funcDd))
                selectedField <- hold ("" :: Text) $ fmap selectorName onNodeSelect
                let addWgtDynMap = M.fromList addWgtDynList
                    mkPayload = do
                      fldN <- sample selectedField
                      let mAddWgtDyn = M.lookup fldN addWgtDynMap
                      case mAddWgtDyn of
                        Nothing -> return $ AddEvtPayload "" Nothing defAssertFnKey
                        Just (valDyn, ddValDyn) -> do
                          ddVal <- sample . current $ ddValDyn
                          val <- sample . current $ valDyn
                          return $ AddEvtPayload ("." <> fldN) val ddVal

                return $ (tag (pull mkPayload) onAddAssr, _el_clicked closeEl)
      return onAddAssrEvt
    renderForest :: MonadWidget t m => Forest SInfo -> m (Event t SInfo)
    renderForest xs = do
      evts <- mapM renderTree xs
      return $ leftmost evts
      where
        renderTree (Node t@(SInfo txt _) ts) = do
          (elem, _) <- el' "li" $ text . T.unpack $ txt
          el "ul" $ do
            evt <- el "li" $ renderForest ts
            return $ leftmost [fmap (const t) (domEvent Click elem), evt]
        renderTree (Node (SumConName txt) ts) = do
          el "li" $ text . T.unpack $ txt
          el "ul" $ do
            evt <- el "li" $ renderForest ts
            return evt


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

instance CtorInfo f => CtorInfo (D1 c f) where
  constructorNames _ = constructorNames (Proxy :: Proxy f)

instance (CtorInfo x, CtorInfo y) => CtorInfo (x :+: y) where
  constructorNames _ = constructorNames (Proxy :: Proxy x) ++ constructorNames (Proxy :: Proxy y)

instance (Constructor c) => CtorInfo (C1 c f) where
  constructorNames _ = [T.pack $ conName (undefined :: t c f a)]

type DynamicAttr t = Dynamic t (Map String String)

data GToWidgetState t = GToWidgetState
  { st_constructors :: (Event t Text, M.Map Text (DynamicAttr t))
  }

data GToWidgetOpts t f a = GToWidgetOpts
  { state        :: Maybe (GToWidgetState t)
  , widgetDefVal :: Maybe (f a)
  , arbitraryDef :: Maybe (Dynamic t (f a))
  }

class GToWidget f where
  gToWidget :: ( MonadWidget t m
              ) => GToWidgetOpts t f a -> m (Dynamic t (Maybe (f a)))

instance (GToWidget f, CtorInfo f) => GToWidget (D1 c f) where
  gToWidget gOpts@(GToWidgetOpts _ wDef aDef) = do
    {-wDef' <- case wDef of
      Just (D1 x) -> Just x
      _ -> Nothing-}
    let ctorNames = constructorNames (Proxy :: Proxy f)
    aDef' <- case aDef of
      Just dynM1 -> Just <$> mapDyn (\(M1 a) -> a) dynM1
      _           -> return Nothing
    let wDef' = fmap (\(M1 a) -> a) wDef
        gopts' = GToWidgetOpts Nothing wDef' aDef'
    case ctorNames of
      (firstCtor:_:_) -> do  -- SumType
        divClass "sum-wrapper" $ do
          let ctorNameMap = M.fromList $ map (\x -> (x, T.unpack x)) ctorNames
          dd <- dropdown firstCtor (constDyn ctorNameMap) $ def
          sumTyAttrMap <- (return . M.fromList) =<< mapM (\c -> do
            cDyn <- mapDyn (\ddVal -> if ddVal == c then ("class" =: "sum-ty active") else ("class" =: "sum-ty")) (_dropdown_value dd)
            return (c, cDyn)
            ) ctorNames
          mapDyn (fmap M1) =<< gToWidget gopts' { state = Just $ GToWidgetState (_dropdown_change dd, sumTyAttrMap) }
      _ -> mapDyn (fmap M1) =<< gToWidget gopts'

instance (GToWidget f, GToWidget g, CtorInfo f, GToWidget (g :+: h)) => GToWidget (f :+: g :+: h) where
  gToWidget gopts@(GToWidgetOpts gstate wDef aDef) = do
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

    lDyn <- mapDyn (fmap L1) =<< do
      elDynAttr "div" lDynAttr $ do
        gToWidget (GToWidgetOpts (Just $ GToWidgetState ({-M.delete lConName-} (evt, attrMap))) lwDef Nothing)
    rDyn <- mapDyn (fmap R1) =<< gToWidget (GToWidgetOpts (Just $ GToWidgetState ({-M.delete lConName-} (evt, attrMap))) rwDef Nothing)

    fmap joinDyn $ foldDyn (\a _ -> if a == lConName then lDyn else rDyn) lDyn evt

instance (GToWidget f, GToWidget g, Typeable g, CtorInfo f, Constructor c) => GToWidget (f :+: C1 c g) where
  gToWidget gopts@(GToWidgetOpts gstate wDef aDef) = do
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
    lDyn <- mapDyn (fmap L1) =<< do
      elDynAttr "div" lDynAttr $ do
        gToWidget (GToWidgetOpts (Just $ GToWidgetState ({-M.delete lConName-} evt, attrMap)) lwDef Nothing)
    rDyn <- mapDyn (fmap R1) =<< do
      elDynAttr "div" rDynAttr $ do
        gToWidget (GToWidgetOpts Nothing rwDef Nothing)

    fmap joinDyn $ foldDyn (\a _ -> if a == lConName then lDyn else rDyn) lDyn evt

instance (GToWidget a, GToWidget b) => GToWidget (a :*: b) where
  gToWidget gopts@(GToWidgetOpts _ wDef _)= do
    let (awDef, bwDef) = case wDef of
          Nothing -> (Nothing, Nothing)
          Just (ad :*: bd) -> (Just ad, Just bd)
        aGopts' = GToWidgetOpts Nothing awDef Nothing
        bGopts' = GToWidgetOpts Nothing bwDef Nothing
    adyn <- gToWidget aGopts'
    bdyn <- gToWidget bGopts'
    combineDyn (\a b -> (:*:) <$> a <*> b) adyn bdyn

instance (GToWidget f, Typeable f, Constructor c) => GToWidget (C1 c f) where
  gToWidget gopts@(GToWidgetOpts _ wDef _) = do
    let wDef' = fmap (\(M1 a) -> a) wDef
        gopts' = GToWidgetOpts Nothing wDef' Nothing
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
  gToWidget gopts@(GToWidgetOpts _ wDef _) = do
    let wDef' = fmap (\(M1 a) -> a) wDef
        gopts' = GToWidgetOpts Nothing wDef' Nothing
    elClass "div" "field" $ do
      elAttr "label" ("class" =: "label") $ text $ selName (undefined :: S1 s f ())
      inp <- gToWidget gopts'
      mapDyn (fmap M1) inp

instance (ToWidget f) => GToWidget (K1 c f) where
  gToWidget (GToWidgetOpts _ wDef _) =
    let wDef' = fmap (\(K1 a) -> a) wDef
    in mapDyn (fmap K1) =<< toWidget Proxy wDef'

class ToWidget a where
  toWidget :: MonadWidget t m => Proxy a -> Maybe a -> m (Dynamic t (Maybe a))
  default toWidget :: (MonadWidget t m, Generic a, GToWidget (Rep a)) => Proxy a -> Maybe a -> m (Dynamic t (Maybe a))
  toWidget _ wDef = mapDyn (fmap to) =<< gToWidget (GToWidgetOpts Nothing (fmap from wDef) Nothing)

instance ToWidget Text where
  toWidget _ wDef = do
    let def' = def
                & textInputConfig_inputType .~ "number"
                & attributes .~ constDyn ("class" =: "text-box")
        textDef = case wDef of
          Nothing -> def'
          Just a -> def' & textInputConfig_initialValue .~ T.unpack a
    txt <- textInput textDef
    mapDyn (decodeParam . ASCII.pack) $ _textInput_value txt

instance ToWidget Int where
  toWidget _ wDef = do
    let def' = def
                & textInputConfig_inputType .~ "number"
                & attributes .~ constDyn ("class" =: "text-box")
        intDef = case wDef of
          Nothing -> def'
          Just a -> def' & textInputConfig_initialValue .~ show a
    txt <- textInput intDef
    mapDyn (decodeParam . ASCII.pack) $ _textInput_value txt

instance ToWidget Double where
  toWidget _ wDef = do
    let def' = def
                & textInputConfig_inputType .~ "number"
                & attributes .~ constDyn ("class" =: "text-box")
        doubleDef = case wDef of
          Nothing -> def'
          Just a -> def' & textInputConfig_initialValue .~ show a
    txt <- textInput doubleDef
    mapDyn (decodeParam . ASCII.pack) $ _textInput_value txt

instance ToWidget Bool where
  toWidget _ wDef = do
    chk <- checkbox (fromMaybe False wDef) def
    mapDyn Just $ _checkbox_value chk

instance ToWidget () where
  toWidget _ _ = emptyParamWidget

instance ToWidget UTCTime where
  toWidget _ wDef = do
    let def' = def
                & attributes .~ constDyn ("class" =: "text-box")
        utcDef = maybe def' (\a -> def' & textInputConfig_initialValue .~ (ASCII.unpack . encodeParam $ a)) wDef
    txt <- textInput utcDef
    dynText $ _textInput_value txt
    mapDyn (decodeParam . ASCII.pack) $ _textInput_value txt

instance ToWidget LocalTime where
  toWidget _ wDef = do
    let def' = def
                & textInputConfig_inputType .~ "datetime-local"
                & attributes .~ (constDyn $ M.fromList [("class", "text-box"), ("step", "1")])
        timeDef = maybe def' (\a -> def' & textInputConfig_initialValue .~ (ASCII.unpack . encodeParam $ a)) wDef
    txt <- textInput timeDef
    dynText $ _textInput_value txt
    mapDyn (decodeParam . ASCII.pack) $ _textInput_value txt

instance ToWidget Day where
  toWidget _ wDef = do
    let def' = def
                & textInputConfig_inputType .~ "date"
                & attributes .~ constDyn ("class" =: "text-box")
        dayDef = maybe def' (\a -> def' & textInputConfig_initialValue .~ (ASCII.unpack . encodeParam $ a)) wDef
    txt <- textInput dayDef
    dynText $ _textInput_value txt
    mapDyn (decodeParam . ASCII.pack) $ _textInput_value txt

instance ToWidget TimeOfDay where
  toWidget _ wDef = do
    let def' = def
                & textInputConfig_inputType .~ "time"
                & attributes .~ (constDyn $ M.fromList [("class", "text-box"), ("step", "1")])
        todDef = maybe def' (\a -> def' & textInputConfig_initialValue .~ (ASCII.unpack . encodeParam $ a)) wDef
    txt <- textInput todDef
    dynText $ _textInput_value txt
    mapDyn (decodeParam . ASCII.pack) $ _textInput_value txt


instance ToWidget a => ToWidget [a] where
  toWidget _ _ = do
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
          (addEvtEl, _) <- elAttr' "span" ("class" =: "plus-button") $ text "+"
          let addEvt = _el_clicked addEvtEl
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
        mDyn   <- toWidget (Proxy :: Proxy a) Nothing
        (removeEl, _) <- elAttr' "span" ("class" =: "cross-button") $ text "+"
        let onRemove = _el_clicked removeEl
        mDyn' <- mapDyn (maybe [] (: [])) mDyn
        return (mDyn, tag (constant i) onRemove)

instance ToWidget a => ToWidget (Maybe a) where
  toWidget _ _ = do
    divClass "maybe-wrapper" $ do
      let checkboxDefVal = False
      chk <- checkbox checkboxDefVal def
      widget <- toWidget (Proxy :: Proxy a) Nothing
      let checkboxDyn = _checkbox_value chk
      isActive <- toggle checkboxDefVal (updated checkboxDyn)
      combineDyn (\a b -> fmap (\x -> if a then Just x else Nothing) b) isActive widget

class ToPathParamWidget (par :: *) (isTup :: Bool) where
  topathParamWidget :: MonadWidget t m => PathPar t par isTup -> [PathSegment] -> m (Dynamic t (Maybe par))

instance ToWidget par => ToPathParamWidget par 'False where
  topathParamWidget _ pthSegs = do
    let (spths1, _:pthSegs1) = break (==Hole) pthSegs
    forM spths1 $ \spth -> staticPthWid True spth
    _1Wid <- toWidget (Proxy :: Proxy par) Nothing
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
    _1Wid <- toWidget (Proxy :: Proxy t1) Nothing
    let (spths2, _:pthSegs2) = break (==Hole) pthSegs1
    forM spths2 $ \spth -> staticPthWid True spth
    _2Wid <- toWidget (Proxy :: Proxy t2) Nothing
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

data SelNameInst = GenSelName | CustomSelName

data SInfo = SInfo { selectorName :: Text , selectorType :: WidgetBox }
           | SumConName Text

type ConName = Text

instance Eq SInfo where
  s1 == s2 = s1 `compare` s2 == EQ

instance Ord SInfo where
  compare s1 s2 = compare (selectorName s1) (selectorName s2)

class GSelectorInfo (f :: * -> *) where
  gSelectorInfo :: Proxy f -> Forest SInfo -> Forest SInfo

instance GSelectorInfo f => GSelectorInfo (D1 c f) where
  gSelectorInfo _ acc = gSelectorInfo (Proxy :: Proxy f) acc

instance (GSelectorInfo f, GSelectorInfo (g :+: h), CtorInfo f) => GSelectorInfo (f :+: g :+: h) where
  gSelectorInfo _ acc = let cName = head $ constructorNames (Proxy :: Proxy f)
                            prefixCName (Node (SInfo x wb) xs) = Node (SInfo (cName <> "." <> x) wb) (map prefixCName xs)
                            prefixCName (Node x xs) = Node x (map prefixCName xs)
                        in [Node (SumConName cName) $ map prefixCName (gSelectorInfo (Proxy :: Proxy f) [])] <> gSelectorInfo (Proxy :: Proxy (g :+: h)) []

instance (GSelectorInfo f, GSelectorInfo g, CtorInfo f, Constructor c) => GSelectorInfo (f :+: C1 c g) where
  gSelectorInfo _ acc = let lcName = head $ constructorNames (Proxy :: Proxy f)
                            rcName = T.pack $ conName (undefined :: t c g a)
                            prefixCName cName (Node (SInfo x wb) xs) = Node (SInfo (cName <> "." <> x) wb) (map (prefixCName cName) xs)
                            prefixCName cName (Node x xs) = Node x (map (prefixCName cName) xs)
                        in [Node (SumConName lcName) $ map (prefixCName lcName) (gSelectorInfo (Proxy :: Proxy f) [])] <>
                           [Node (SumConName rcName) $ map (prefixCName rcName) (gSelectorInfo (Proxy :: Proxy g) [])]

instance (GSelectorInfo a, GSelectorInfo b) => GSelectorInfo (a :*: b) where
  gSelectorInfo _ acc = acc <> gSelectorInfo (Proxy :: Proxy a) [] <> gSelectorInfo (Proxy :: Proxy b) acc

instance GSelectorInfo U1 where
  gSelectorInfo _ acc = acc

instance (GSelectorInfo f) => GSelectorInfo (C1 c f) where
  gSelectorInfo _ acc = gSelectorInfo (Proxy :: Proxy f) acc

instance (Selector s, SelectorInfo f, AssertWidget f, ToWidget f, Typeable f) => GSelectorInfo (S1 s (K1 c f)) where
  gSelectorInfo _ _ = let sName = T.pack $ selName (undefined :: S1 s (K1 c f) ())
                          prefixSName (Node (SInfo x wb) xs) = Node (SInfo (sName <> "." <> x) wb) (map prefixSName xs)
                          prefixSName (Node x xs)            = Node x (map prefixSName xs)
                      in [Node (SInfo sName (WidgetBox (Proxy :: Proxy f))) (map prefixSName $ selectorInfos (Proxy :: Proxy f) [])]

class SelectorInfo f where
  selectorInfos :: Proxy f -> Forest SInfo -> Forest SInfo
  default selectorInfos :: (GSelectorInfo (Rep f)) => Proxy f -> Forest SInfo -> Forest SInfo
  selectorInfos _ acc = gSelectorInfo (Proxy :: Proxy (Rep f)) acc

instance SelectorInfo Text where
  selectorInfos _ acc = acc

instance SelectorInfo Int where
  selectorInfos _ acc = acc

instance SelectorInfo Bool where
  selectorInfos _ acc = acc

instance SelectorInfo () where
  selectorInfos _ acc = acc

instance SelectorInfo UTCTime where
  selectorInfos _ acc = acc

instance SelectorInfo LocalTime where
  selectorInfos _ acc = acc

instance SelectorInfo Day where
  selectorInfos _ acc = acc

instance SelectorInfo TimeOfDay where
  selectorInfos _ acc = acc

instance (SelectorInfo a) => SelectorInfo [a] where
  selectorInfos _ acc = selectorInfos (Proxy :: Proxy a) acc

instance (SelectorInfo a) => SelectorInfo (Maybe a) where
  selectorInfos _ acc = selectorInfos (Proxy :: Proxy a) acc

data AddEvtPayload = AddEvtPayload
  { fieldName :: Text
  , fieldValue :: Maybe R1D.Dynamic
  , selectedFunc :: GName
  }

data GAssertState t = GAssertState
  { fieldNameAcc :: Text
  , sumTypWgtState :: Maybe (Event t Text, M.Map Text (DynamicAttr t)) -- TODO: remove this
  , globalAddEvent :: Event t AddEvtPayload
  , as_conFuns :: ConsoleFunctions
  }

class GAssert f where
  gAssert :: (MonadWidget t m) => Proxy (f a) -> GAssertState t -> m (Dynamic t (Predicate (f a)))

instance (GAssert f, CtorInfo f) => GAssert (D1 c f) where
  gAssert _ gs = do
    let ctorNames = constructorNames (Proxy :: Proxy f)
    case ctorNames of
      (firstCtor:_:_) -> do
        divClass "sum-wrapper" $ do
          let ctorNameMap = M.fromList $ map (\x -> (x, T.unpack x)) ctorNames
          {-dd <- dropdown firstCtor (constDyn ctorNameMap) $ def
          sumTyAttrMap <- (return . M.fromList) =<< mapM (\c -> do
            cDyn <- mapDyn (\ddVal -> if ddVal == c then ("class" =: "sum-ty active") else ("class" =: "sum-ty")) (_dropdown_value dd)
            return (c, cDyn)
            ) ctorNames-}
          --mapDyn (contramap unM1) =<< gAssert Proxy gs { sumTypWgtState = Just (_dropdown_change dd, sumTyAttrMap) }
          mapDyn (contramap unM1) =<< gAssert Proxy gs { sumTypWgtState = Nothing }
      _ -> mapDyn (contramap unM1) =<< gAssert Proxy gs

instance (GAssert f, GAssert g, CtorInfo f, GAssert (g :+: h)) => GAssert (f :+: g :+: h) where
  gAssert _ gs@(GAssertState fname sState gAddEvt conFuns) = do
    let (evt, attrMap) =
          case sState of
            Just st -> st
            _       -> (never, M.empty)
        lConName = head $ constructorNames (Proxy :: Proxy f)
        lDynAttr = fromMaybe
                    (error $ "PANIC!: Constructor lookup failed @ GAssert (f :+: g)" ++ (T.unpack lConName) ++ (show $ M.keys attrMap))
                    (M.lookup lConName attrMap)
        fldName = fname <> "." <> lConName
        onFieldPrefix = push (ifFieldHas fldName) gAddEvt
    wgtShowAttr <- toggleWidgetOn onFieldPrefix never
    lDyn <- mapDyn (contramap (\(L1 x) -> x)) =<< do
      elDynAttr "div" wgtShowAttr $ do
        gAssert Proxy gs { fieldNameAcc = fldName }
    rDyn <- mapDyn (contramap (\(R1 x) -> x)) =<< gAssert Proxy gs
    fmap joinDyn $ foldDyn (\a _ -> if a == lConName then lDyn else rDyn) lDyn evt
    where
      ifFieldHas x y = let y' = fieldName y
        in if T.isPrefixOf x y' && x /= y' then return (Just y) else return Nothing
      toggleWidgetOn showEvt hideEvt = foldDyn (\x _ -> if x then M.empty else ("style" =: "display:none")) ("style" =: "display:none") $ leftmost [fmap (const True) showEvt, fmap (const False) hideEvt]

instance (GAssert f, GAssert g, Typeable g, CtorInfo f, Constructor c) => GAssert (f :+: C1 c g) where
  gAssert _ gs@(GAssertState fname sState gAddEvt conFuns) = do
    let (evt, attrMap) =
          case sState of
            Just st -> st
            _       -> (never, M.empty)
        lConName = head $ constructorNames (Proxy :: Proxy f)
        lDynAttr = fromMaybe
                    (error $ "PANIC!: Constructor lookup failed @ GAssert (f :+: g)" ++ (T.unpack lConName) ++ (show $ M.keys attrMap))
                    (M.lookup lConName attrMap)
        rConName = T.pack $ conName (undefined :: t c g a)
        rDynAttr = fromMaybe
                    (error $ "PANIC!: Constructor lookup failed @ GAssert (f :+: g)" ++ (T.unpack rConName) ++ (show $ M.keys attrMap))
                    (M.lookup rConName attrMap)
        lfldName = fname <> "." <> lConName
        rfldName = fname <> "." <> rConName
        onLFieldPrefix = push (ifFieldHas lfldName) gAddEvt
        onRFieldPrefix = push (ifFieldHas rfldName) gAddEvt
    lwgtShowAttr <- toggleWidgetOn onLFieldPrefix never
    rwgtShowAttr <- toggleWidgetOn onRFieldPrefix never
    lDyn <- mapDyn (contramap (\(L1 x) -> x)) =<< do
      elDynAttr "div" lwgtShowAttr $ do
        gAssert Proxy gs { fieldNameAcc = lfldName }
    rDyn <- mapDyn (contramap (\(R1 x) -> x)) =<< do
      elDynAttr "div" rwgtShowAttr $ do
        gAssert Proxy gs { fieldNameAcc = rfldName }
    fmap joinDyn $ foldDyn (\a _ -> if a == lConName then lDyn else rDyn) lDyn evt
    where
      ifFieldHas x y = let y' = fieldName y
        in if T.isPrefixOf x y' && x /= y' then return (Just y) else return Nothing
      toggleWidgetOn showEvt hideEvt = foldDyn (\x _ -> if x then M.empty else ("style" =: "display:none")) ("style" =: "display:none") $ leftmost [fmap (const True) showEvt, fmap (const False) hideEvt]

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

instance (GAssert f, Selector s, f ~ (K1 c f1), ToWidget f1, Typeable f1, AssertWidget f1) => GAssert (S1 s f) where
  gAssert _ (GAssertState fname _ gAddEvt conFuns) = do
    let fldName = fname <> "." <> T.pack sName
        sName = selName (undefined :: S1 s f ())
    (onFieldShow, onFieldAdd) <- headTailE $ push (ifFieldIs fldName) gAddEvt
    putDebugLnE onFieldShow (\x -> T.unpack fldName)
    let onFieldPrefix = push (ifFieldHas fldName) gAddEvt
    p1 <- listWidget onFieldShow onFieldAdd createWidget
    p2 <- createAssertWidget onFieldPrefix
    combineDyn (\a b -> Predicate $ \prd -> ((getPredicate a) prd) && ((getPredicate b) prd)) p1 p2
    where
      funTable' = assertFunctions conFuns
      showWidgetOn = foldDyn (\x -> const M.empty) ("style" =: "display:none")
      toggleWidgetOn showEvt hideEvt = foldDyn (\x _ -> if x then M.empty else ("style" =: "display:none")) ("style" =: "display:none") $ leftmost [fmap (const True) showEvt, fmap (const False) hideEvt]
      ifFieldHas x y = let y' = fieldName y
        in if T.isPrefixOf x y' && x /= y' then return (Just y) else return Nothing
      ifFieldIs x y = let y' = fieldName y
        in if x == y' then return (Just y) else return Nothing
      listWidget wgtShowEvt onFieldAdd createWidget = do
        wgtShowAttr <- showWidgetOn wgtShowEvt
        putDebugLnE onFieldAdd (const "called")
        elDynAttr "div" wgtShowAttr $ do
          (addEvtEl, _) <- elAttr' "span" ("class" =: "plus-button float-right") $ text "+"
          rec dynValMap <- listWithKeyShallowDiff
                --((0 =: (AddEvtPayload "" Nothing defAssertFnKey)) :: M.Map Int AddEvtPayload)
                (M.empty :: M.Map Int AddEvtPayload)
                (leftmost evtList)
                createWidget
              let setNothingAt i = do
                    valMap <- sample. current $ dynValMap
                    return $ Just $ M.mapWithKey (\k a -> if k == i then Nothing else Just defPayload) valMap
                  addElement aPayload = do
                    valMap <- sample. current $ dynValMap
                    lastKey <- sample . current $ lastKeyD
                    return $ Just $ M.insert (lastKey + 1) (Just aPayload) $ M.map (const (Just defPayload)) valMap
                  defPayload = AddEvtPayload "" Nothing defAssertFnKey
              dynListWithKeys <- mapDyn M.toList dynValMap
              dynValMap' <- mapDyn (M.map fst) dynValMap
              let getLastKey (x :: [Int]) = if null x then (-1) else maximum x
              lastKeyD <- mapDyn (getLastKey . map fst) dynListWithKeys
              (_, evtsD) <- splitDyn =<< mapDyn (unzip . map snd) dynListWithKeys
              let modelD = joinDynThroughMap dynValMap'
              evts <- mapDyn leftmost evtsD -- Remove events
              let evtList = (push addElement addEvt) : [(push setNothingAt $ switchPromptlyDyn evts)]
                  addEvt = leftmost [fmap (const $ AddEvtPayload "" Nothing defAssertFnKey) $ _el_clicked addEvtEl, onFieldAdd, wgtShowEvt]
          mapDyn (combinePredicates . (map snd) . M.toList) modelD
      createWidget k (AddEvtPayload _ mDyn sFunc) _ = do
        el "div" $ do
          let sName = selName (undefined :: S1 s f ())
          dropdown sName (constDyn (sName =: sName)) $ def
            & attributes .~ constDyn ("class" =: "assert-field-select")
          (removeEl, _) <- elAttr' "span" ("class" =: "assert-remove") $ text "-"
          dyn <- divClass "field-assertion" $ do
            let defKey = defAssertFnKey
                assertWidgetAttr = ("class" =: "assert-widget")
                fldname = fname <> "." <> T.pack sName
                dropDownKV (n, fnInfo) = (n, displayName fnInfo)
            dd <- dropdown sFunc (constDyn (LM.fromList $ fmap dropDownKV $ HM.toList funTable')) def
            let lookupPredFn fnKey = maybe (error "Unknown Fn1") prjFnDyn $ HM.lookup fnKey funTable'
            let succeed _ _ = True
            fnDyn <- holdDyn (toDynamic (succeed :: ANY -> ANY -> Bool)) ((lookupPredFn) <$>_dropdown_change dd)
            elAttr "div" assertWidgetAttr $ do
              valDyn <- renderAssertWidget Proxy (mDyn >>= ((either (const Nothing) Just) . fromDynamic))
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
                  mapDyn (contramap unM1 . contramap unK1) =<< assertWidget (Proxy :: Proxy f1) (GAssertState (fname <> "." <> T.pack sName) Nothing gAddEvt conFuns)
              return (dyn, _el_clicked removeEl)
        return predDyn
      mkPred :: R1D.Dynamic -> Maybe f1 -> Predicate f1
      mkPred fn Nothing  = Predicate $ const True
      mkPred fn (Just w) = Predicate $ \v -> unsafeRight $ do
        pred <- fn `dynApply` (toDynamic v)
        res  <- pred `dynApply` (toDynamic w)
        fromDynamic res
      unsafeRight (Right a) = a
      unsafeRight (Left e)  = error $ "Expecting only right, but got: " ++ (show e)

instance (ToWidget f, Typeable f, AssertWidget f) => GAssert (K1 c f) where
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

class AssertWidget a where
  assertWidget :: (MonadWidget t m) => Proxy a -> GAssertState t -> m (Dynamic t (Predicate a))
  default assertWidget :: (MonadWidget t m, Generic a, GAssert (Rep a)) => Proxy a -> GAssertState t -> m (Dynamic t (Predicate a))
  assertWidget _ gs = mapDyn (contramap from) =<< gAssert Proxy gs
  renderAssertWidget :: (ToWidget a, MonadWidget t m) => Proxy a -> Maybe a -> m (Dynamic t (Maybe a))
  renderAssertWidget = toWidget

instance AssertWidget Text where
  assertWidget _ _ = return $ constDyn $ Predicate (\_ -> True)

instance AssertWidget Int where
  assertWidget _ _ = return $ constDyn $ Predicate (\_ -> True)

instance AssertWidget Bool where
  assertWidget _ _ = return $ constDyn $ Predicate (\_ -> True)

instance AssertWidget () where
  assertWidget _ _ = return $ constDyn $ Predicate (\_ -> True)

instance AssertWidget UTCTime where
  assertWidget _ _ = return $ constDyn $ Predicate (\_ -> True)

instance AssertWidget LocalTime where
  assertWidget _ _ = return $ constDyn $ Predicate (\_ -> True)

instance AssertWidget Day where
  assertWidget _ _ = return $ constDyn $ Predicate (\_ -> True)

instance AssertWidget TimeOfDay where
  assertWidget _ _ = return $ constDyn $ Predicate (\_ -> True)

instance (ToWidget a, Typeable a, AssertWidget a, Generic a, GAssert (Rep a)) => AssertWidget [a] where
  renderAssertWidget _ wDef = do
    idx <- textInput def
    mapDyn (fmap (: [])) =<< toWidget (Proxy :: Proxy a) Nothing
    --idxDyn <- mapDyn (decodeParam . ASCII.pack) $ _textInput_value txt
  assertWidget _ gs = do
    txt <- textInput def
    let fn = head
    mapDyn ((\y -> Predicate y) . (\x -> x . fn) . getPredicate . contramap from) =<< gAssert (Proxy :: Proxy (Rep a ())) gs
    --return $ constDyn $ Predicate (\_ -> True)--mapDyn (contramap from) =<< gAssert Proxy gs

instance AssertWidget (Maybe a) where
  assertWidget _ gs = return $ constDyn $ Predicate (\_ -> True)--mapDyn (contramap from) =<< gAssert Proxy gs

data WidgetBox = forall a.(ToWidget a, AssertWidget a, Typeable a) => WidgetBox (Proxy a)

instance Show WidgetBox where
  show wb = "<<WidgetBox>>"

data PrjFnInfo = PrjFnInfo
  { prjFnDyn :: R1D.Dynamic
  , prjResWidget :: WidgetBox
  , prjFnModName :: String
  , prjFnPkgKey  :: String
  , displayName  :: String
  } deriving (Show)

type PrjMap = HashMap GName PrjFnInfo

data PrjVal = forall v.(Typeable v, ToWidget v) => Val v
            | ProjectedVal (R1D.Dynamic, PrjFnInfo) PrjVal

data GName = GName
  { pkgName  :: !String
  , modName  :: !String
  , fnName   :: !String
  } deriving (Show, Read, Eq, Ord, Generic)

instance Hashable GName

instance Lift GName where
  lift (GName p m f) = (conE 'GName) `appE` lift p `appE` lift m `appE` lift f

-- (, PrjFnInfo (toDynamic (length :: [ANY] -> Int)) (WidgetBox (Proxy :: Proxy Int)) "Data.List" "base" Nothing)

funTable :: PrjMap
funTable = HM.fromList
  [ (GName "GHC.Classes" "base" "==", PrjFnInfo (toDynamic ((==) :: Int -> Int -> Bool)) (WidgetBox (Proxy :: Proxy Bool)) "GHC.Classes" "base" "Equals")
  , (GName "GHC.Classes" "base" "/=", PrjFnInfo (toDynamic ((/=) :: Int -> Int -> Bool)) (WidgetBox (Proxy :: Proxy Bool)) "GHC.Classes" "base" "NotEquals")
  ]

defAssertFnKey :: GName
defAssertFnKey = GName "GHC.Classes" "base" "=="

apply :: GName -> PrjVal -> Either String PrjVal
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

emptyParamWidget ::  MonadWidget t m => m (Dynamic t (Maybe ()))
emptyParamWidget = return $ constDyn $ Just ()

staticPthWid :: MonadWidget t m => Bool -> PathSegment -> m ()
staticPthWid sep (StaticSegment spth) = el "div" $ text $ (T.unpack spth) ++ if sep then "/" else ""
staticPthWid _ Hole = error "Invariant Violated @staticPthWid! Found Dynamic Hole"

mkXhrReq :: Reflex t
         => Dynamic t Text
         -> Dynamic t String
         -> Dynamic t BS.ByteString
         -> Dynamic t (Maybe (Map String String))
         -> PullM t XhrRequest
mkXhrReq methD urlD fpD hdrInD = do
  meth <- sample . current $ methD
  url  <- sample . current $ urlD
  fp   <- sample . current $ fpD
  hdrIn <- (fromMaybe M.empty) <$> (sample . current $ hdrInD)
  let headerUrlEnc = if BS.null fp then hdrIn else M.insert "Content-type" "application/x-www-form-urlencoded" hdrIn
      body = ASCII.unpack fp
  return $ XhrRequest (T.unpack meth) url
            $ def { _xhrRequestConfig_headers = headerUrlEnc
                  , _xhrRequestConfig_sendData = Just body
                  }

combinePredicates :: [Predicate a] -> Predicate a
combinePredicates xs = Predicate $ \x -> all id $ map (($ x) . getPredicate) xs
