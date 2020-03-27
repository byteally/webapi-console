{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Utils
       ( module Reflex.Utils
--       , module Reflex.Utils.Dynamic
       ) where

import Data.Maybe
import Reflex.Dynamic
import Reflex.Dom.Core
import Data.Monoid ((<>))
import Data.Text as T (Text)
import Control.Monad.Fix
import Control.Monad
import Control.Lens (imapM)
import Data.Map as Map hiding (map)
import Control.Monad.IO.Class (liftIO, MonadIO)
--import Reflex.Utils.Dynamic

mkSwitchableAttrs :: (Reflex t, MonadHold t m)
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
        pure $ fmap (addActive attr) isActive
    tagFalse = tag (constant False)
    tagTrue = tag (constant True)
    addActive defAttr b = if b then Map.insertWith (\x y' -> x <> " " <> y') "class" "active" defAttr else defAttr

dropdownTabDisplay :: forall t m k. (MonadFix m, Reflex t, MonadHold t m, Show k, Ord k, DomBuilder t m, PostBuild t m)
  => Text               -- ^ Class applied to <ul> element
  -> Text               -- ^ Class applied to currently active <li> element
  -> Map k (Text, (Event t () -> m ())) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
dropdownTabDisplay ulClass activeClass tabItems = do
  (dCurrentTab, _dTabClicks, onSubmit) <- divClass "endpoint-selector" $ do
    (dCurrentTab, dTabClicks) <- divClass "dropdown" $ do
      elAttr "span" ("class" =: "dropdown-val") $ text $ getFirstItemName tabItems
      rec dCurrentTab <- holdDyn Nothing (updated dTabClicks)
          dTabClicks :: Dynamic t (Maybe k) <- elAttr "ul" (Map.singleton "class" ulClass) $ do
            tabClicksList :: [Event t k] <- (liftM Map.elems) $ imapM (\k (s,_) -> headerBarLink s k $ fmap (== (Just k)) dCurrentTab) tabItems
            let eTabClicks :: Event t k = leftmost tabClicksList
            holdDyn Nothing $ fmap Just eTabClicks :: m (Dynamic t (Maybe k))
      return (dCurrentTab, dTabClicks)
    (sendEl , _) <- elAttr' "button" ("class" =: "send") $ text "Send"
    return (dCurrentTab, dTabClicks, domEvent Click sendEl)
  divClass "wrapper" $ do
    let dTabs :: Dynamic t (Map k (Text, (Event t () -> m ()))) = constDyn tabItems
    _ <- listWithKey dTabs (\k dTab -> do
      dAttrs <- pure $ fmap (\sel -> do
        let t1 = listToMaybe $ Map.keys tabItems
        if sel == Just k || (sel == Nothing && t1 == Just k) then Map.empty else Map.singleton "style" "display:none;") dCurrentTab
      isVisibleDyn <- pure $ fmap (\x -> if x == Map.empty then True else False) dAttrs
      let onSubmit' = attachPromptlyDynWithMaybe (\a _ -> if a then Just () else Nothing) isVisibleDyn onSubmit
      elDynAttr "div" dAttrs $ dyn $ fmap (($ onSubmit') . snd) dTab)
    return ()
  where
    getFirstItemName x = if Map.null x then "" else (fst . snd . head . toList) x
    headerBarLink :: (Ord k, DomBuilder t m) => Text -> k -> Dynamic t Bool -> m (Event t k)
    headerBarLink x k dBool = do
      dAttributes <- pure $ fmap (\b -> if b then Map.singleton "class" activeClass else Map.empty) dBool
      (elemt, _) <- elDynAttr' "li" dAttributes $ text x
      return $ fmap (const k) (domEvent Click elemt)

button' :: DomBuilder t m => Text -> Text -> m (Event t ())
button' cls txt = do
  (e, _) <- elAttr' "button" ("class" =: cls) $ text txt
  return (domEvent Click e)

clickableSpan :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t (Map Text Text) -> m (Event t ())
clickableSpan txt attrs = do
  (e, _) <- elDynAttr' "span" attrs $ text txt
  return (domEvent Click e)

putDebugLnE :: (DomBuilder t m
               , PerformEvent t m
               , MonadIO (Performable m)
               ) => Event t a -> (a -> String) -> m ()
putDebugLnE e mkStr = do
  performEvent_ (liftIO . putStrLn . mkStr <$> e)

putDebugLn :: (DomBuilder t m, PerformEvent t m, PostBuild t m, MonadIO (Performable m)) => String -> m ()
putDebugLn str = do
  pb <- getPostBuild
  putDebugLnE pb (const str)
