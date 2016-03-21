{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Utils where

import Data.Maybe
import Reflex.Dom
import Data.Monoid ((<>))
import Control.Monad.Fix
import Control.Monad
import Control.Lens (imapM)
import Data.Map as Map
import Control.Monad.IO.Class (liftIO)

mkSwitchableAttrs :: MonadWidget t m
                  => [Event t a]
                  -> [Event t a] -- accumulator
                  -> m [Dynamic t (Map String String)]
mkSwitchableAttrs [] _ = return []
mkSwitchableAttrs (y:ys) acc =  do
  let acc' = y : acc
  dynAttrs <- mkSwitchableAttrs ys acc'
  dynAttr <- fn y (acc <> ys)
  return (dynAttr : dynAttrs)
  where
    fn x xs = do
        isActive <- holdDyn False $ leftmost [tagTrue x , tagFalse (leftmost xs)]
        mapDyn addActive isActive
    tagFalse = tag (constant False)
    tagTrue = tag (constant True)
    addActive b = if b then empty else ("style" =: "display: none")

dropdownTabDisplay :: forall t m k. (MonadFix m, MonadWidget t m, Show k, Ord k)
  => String               -- ^ Class applied to <ul> element
  -> String               -- ^ Class applied to currently active <li> element
  -> Map k (String, (Event t () -> m ())) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
dropdownTabDisplay ulClass activeClass tabItems = do
  (dCurrentTab, dTabClicks, onSubmit) <- divClass "endpoint-selector" $ do
    (dCurrentTab, dTabClicks) <- divClass "dropdown" $ do
      elAttr "span" ("class" =: "dropdown-val") $ text "-- Select --"
      rec dCurrentTab <- holdDyn Nothing (updated dTabClicks)
          dTabClicks :: Dynamic t (Maybe k) <- elAttr "ul" (Map.singleton "class" ulClass) $ do
            tabClicksList :: [Event t k] <- (liftM Map.elems) $ imapM (\k (s,_) -> headerBarLink s k =<< mapDyn (== (Just k)) dCurrentTab) tabItems
            let eTabClicks :: Event t k = leftmost tabClicksList
            holdDyn Nothing $ fmap Just eTabClicks :: m (Dynamic t (Maybe k))
      return (dCurrentTab, dTabClicks)
    onSubmit <- button "Submit"
    return (dCurrentTab, dTabClicks, onSubmit)
  divClass "wrapper" $ do
    let dTabs :: Dynamic t (Map k (String, (Event t () -> m ()))) = constDyn tabItems
    _ <- listWithKey dTabs (\k dTab -> do
      dAttrs <- mapDyn (\sel -> do
        let t1 = listToMaybe $ Map.keys tabItems
        if sel == Just k || (sel == Nothing && t1 == Just k) then Map.empty else Map.singleton "style" "display:none;") dCurrentTab
      isVisibleDyn <- mapDyn (\x -> if x == Map.empty then True else False) dAttrs
      let onSubmit' = attachDynWithMaybe (\a _ -> if a then Just () else Nothing) isVisibleDyn onSubmit
      elDynAttr "div" dAttrs $ dyn =<< mapDyn (($ onSubmit') . snd) dTab)
    return ()
  where
    headerBarLink :: (MonadWidget t m, Ord k) => String -> k -> Dynamic t Bool -> m (Event t k)
    headerBarLink x k dBool = do
      dAttributes <- mapDyn (\b -> if b then Map.singleton "class" activeClass else Map.empty) dBool
      (elem, _) <- elDynAttr' "li" dAttributes $ text x
      return $ fmap (const k) (_el_clicked elem)

putDebugLnE :: MonadWidget t m => Event t a -> (a -> String) -> m ()
putDebugLnE e mkStr = do
    performEvent_ (liftIO . putStrLn . mkStr <$> e)

putDebugLn :: MonadWidget t m => String -> m ()
putDebugLn str = do
    pb <- getPostBuild
    putDebugLnE pb (const str)
