{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Utils
       ( module Reflex.Utils
       , module Reflex.Utils.Dynamic
       ) where

import Data.Maybe
import Reflex.Dom
import Data.Monoid ((<>))
import Data.Text as T (Text, unpack)
import Data.Tree
import Control.Monad.Fix
import Control.Monad
import Control.Lens (imapM)
import Data.Map as Map hiding (map, fst)
import Control.Monad.IO.Class (liftIO)
import Reflex.Utils.Dynamic
import Data.String

mkSwitchableAttrs :: MonadWidget t m
                  => [(Event t a, Map Text Text)]
                  -> [(Event t a, Map Text Text)] -- accumulator
                  -> m [Dynamic t (Map Text Text)]
mkSwitchableAttrs [] _ = return []
mkSwitchableAttrs (y:ys) acc =  do
  let acc' = y : acc
  dynAttrs <- mkSwitchableAttrs ys acc'
  dynAttr <- fn y (acc <> ys)
  return (dynAttr : dynAttrs)
  where
    fn (x, attr) xs = do
        isActive <- holdDyn False $ leftmost [tagTrue x , tagFalse (leftmost (map fst xs))]
        mapDyn (addActive attr) isActive
    tagFalse = tag (constant False)
    tagTrue = tag (constant True)
    addActive defAttr b = if b then Map.insertWith (\x y -> x <> " " <> y) "class" "active" defAttr else defAttr

dropdownTabDisplay :: forall t m k. (MonadFix m, MonadWidget t m, Show k, Ord k)
  => Text               -- ^ Class applied to <ul> element
  -> Text               -- ^ Class applied to currently active <li> element
  -> Map k (Text, (Event t () -> m ())) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
dropdownTabDisplay ulClass activeClass tabItems = do
  (dCurrentTab, dTabClicks, onSubmit) <- divClass "endpoint-selector" $ do
    (dCurrentTab, dTabClicks) <- divClass "dropdown" $ do
      elAttr "span" ("class" =: "dropdown-val") $ text $ getFirstItemName tabItems
      rec dCurrentTab <- holdDyn Nothing (updated dTabClicks)
          dTabClicks :: Dynamic t (Maybe k) <- elAttr "ul" (Map.singleton "class" ulClass) $ do
            tabClicksList :: [Event t k] <- (liftM Map.elems) $ imapM (\k (s,_) -> headerBarLink s k =<< mapDyn (== (Just k)) dCurrentTab) tabItems
            let eTabClicks :: Event t k = leftmost tabClicksList
            holdDyn Nothing $ fmap Just eTabClicks :: m (Dynamic t (Maybe k))
      return (dCurrentTab, dTabClicks)
    (sendEl , _) <- elAttr' "button" ("class" =: "send") $ text "Send"
    return (dCurrentTab, dTabClicks, _el_clicked sendEl)
  divClass "wrapper" $ do
    let dTabs :: Dynamic t (Map k (Text, (Event t () -> m ()))) = constDyn tabItems
    _ <- listWithKey dTabs (\k dTab -> do
      dAttrs <- mapDyn (\sel -> do
        let t1 = listToMaybe $ Map.keys tabItems
        if sel == Just k || (sel == Nothing && t1 == Just k) then Map.empty else Map.singleton "style" "display:none;") dCurrentTab
      isVisibleDyn <- mapDyn (\x -> if x == Map.empty then True else False) dAttrs
      let onSubmit' = attachDynWithMaybe (\a _ -> if a then Just () else Nothing) isVisibleDyn onSubmit
      elDynAttr "div" dAttrs $ dyn =<< mapDyn (($ onSubmit') . snd) dTab)
    return ()
  where
    getFirstItemName x = if Map.null x then "" else (fst . snd . head . toList) x
    headerBarLink :: (MonadWidget t m, Ord k) => Text -> k -> Dynamic t Bool -> m (Event t k)
    headerBarLink x k dBool = do
      dAttributes <- mapDyn (\b -> if b then Map.singleton "class" activeClass else Map.empty) dBool
      (elem, _) <- elDynAttr' "li" dAttributes $ text x
      return $ fmap (const k) (_el_clicked elem)

button' :: MonadWidget t m => Text -> Text -> m (Event t ())
button' cls txt = do
  (e, _) <- elAttr' "button" ("class" =: cls) $ text txt
  return (_el_clicked e)

clickableSpan :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m (Event t ())
clickableSpan txt attrs = do
  (e, _) <- elDynAttr' "span" attrs $ text txt
  return (_el_clicked e)

putDebugLnE :: MonadWidget t m => Event t a -> (a -> String) -> m ()
putDebugLnE e mkStr = do
  performEvent_ (liftIO . putStrLn . mkStr <$> e)

putDebugLn :: MonadWidget t m => String -> m ()
putDebugLn str = do
  pb <- getPostBuild
  putDebugLnE pb (const str)
