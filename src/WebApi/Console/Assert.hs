module WebApi.Console.Assert where


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
          el "legend" $ text $ T.pack $ conName (undefined :: C1 c f ())
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
          let sName = T.pack $ selName (undefined :: S1 s f ())
          dropdown sName (constDyn (sName =: sName)) $ def
            & attributes .~ constDyn ("class" =: "assert-field-select")
          (removeEl, _) <- elAttr' "span" ("class" =: "assert-remove") $ text "-"
          dyn <- divClass "field-assertion" $ do
            let defKey = defAssertFnKey
                assertWidgetAttr = ("class" =: "assert-widget")
                fldname = fname <> "." <> sName
                dropDownKV (n, fnInfo) = (n, T.pack $ displayName fnInfo)
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
              let sName = T.pack $ selName (undefined :: S1 s f ())
                  fldname = fname <> "." <> sName
              dropdown sName (constDyn (sName =: sName)) $ def
                & attributes .~ constDyn ("class" =: "assert-field-select")
              (removeEl, _) <- elAttr' "span" ("class" =: "assert-remove") $ text "-"
              --elAttr "label" ("class" =: "label") $ text $ selName (undefined :: S1 s f ())
              dyn <- divClass "field-assertion" $ do
                el "div" $ do
                  mapDyn (contramap unM1 . contramap unK1) =<< assertWidget (Proxy :: Proxy f1) (GAssertState (fname <> "." <> sName) Nothing gAddEvt conFuns)
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
staticPthWid sep (StaticSegment spth) = el "div" $ text $ spth <> if sep then "/" else ""
staticPthWid _ Hole = error "Invariant Violated @staticPthWid! Found Dynamic Hole"
