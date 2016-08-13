-- | 

module Reflex.Utils.Dynamic where

import Reflex
import Reflex.Dynamic

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

combineDyn8 :: (Reflex t, MonadHold t m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9) -> Dynamic t a1 -> Dynamic t a2 -> Dynamic t a3 -> Dynamic t a4 -> Dynamic t a5 -> Dynamic t a6 -> Dynamic t a7 -> Dynamic t a8 -> m (Dynamic t a9)
combineDyn8 fn a1 a2 a3 a4 a5 a6 a7 a8 = (combineDyn (\a8' (a1', a2', a3', a4', a5', a6', a7') -> fn a1' a2' a3' a4' a5' a6' a7' a8') a8) =<< (combineDyn7 (,,,,,,) a1 a2 a3 a4 a5 a6 a7)

combineDyn9 :: (Reflex t, MonadHold t m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10) -> Dynamic t a1 -> Dynamic t a2 -> Dynamic t a3 -> Dynamic t a4 -> Dynamic t a5 -> Dynamic t a6 -> Dynamic t a7 -> Dynamic t a8 -> Dynamic t a9 -> m (Dynamic t a10)
combineDyn9 fn a1 a2 a3 a4 a5 a6 a7 a8 a9 = (combineDyn (\a9' (a1', a2', a3', a4', a5', a6', a7', a8') -> fn a1' a2' a3' a4' a5' a6' a7' a8' a9') a9) =<< (combineDyn8 (,,,,,,,) a1 a2 a3 a4 a5 a6 a7 a8)

combineDyn10 :: (Reflex t, MonadHold t m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11) -> Dynamic t a1 -> Dynamic t a2 -> Dynamic t a3 -> Dynamic t a4 -> Dynamic t a5 -> Dynamic t a6 -> Dynamic t a7 -> Dynamic t a8 -> Dynamic t a9 -> Dynamic t a10 -> m (Dynamic t a11)
combineDyn10 fn a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = (combineDyn (\a10' (a1', a2', a3', a4', a5', a6', a7', a8', a9') -> fn a1' a2' a3' a4' a5' a6' a7' a8' a9' a10') a10) =<< (combineDyn9 (,,,,,,,,) a1 a2 a3 a4 a5 a6 a7 a8 a9)

combineDyn11 :: (Reflex t, MonadHold t m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12) -> Dynamic t a1 -> Dynamic t a2 -> Dynamic t a3 -> Dynamic t a4 -> Dynamic t a5 -> Dynamic t a6 -> Dynamic t a7 -> Dynamic t a8 -> Dynamic t a9 -> Dynamic t a10 -> Dynamic t a11 -> m (Dynamic t a12)
combineDyn11 fn a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = (combineDyn (\a11' (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10') -> fn a1' a2' a3' a4' a5' a6' a7' a8' a9' a10' a11') a11) =<< (combineDyn10 (,,,,,,,,,) a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)

combineDyn12 :: (Reflex t, MonadHold t m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> a13) -> Dynamic t a1 -> Dynamic t a2 -> Dynamic t a3 -> Dynamic t a4 -> Dynamic t a5 -> Dynamic t a6 -> Dynamic t a7 -> Dynamic t a8 -> Dynamic t a9 -> Dynamic t a10 -> Dynamic t a11 -> Dynamic t a12 -> m (Dynamic t a13)
combineDyn12 fn a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 = (combineDyn (\a12' (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10', a11') -> fn a1' a2' a3' a4' a5' a6' a7' a8' a9' a10' a11' a12') a12) =<< (combineDyn11 (,,,,,,,,,,) a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)

combineDyn13 :: (Reflex t, MonadHold t m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> a13 -> a14) -> Dynamic t a1 -> Dynamic t a2 -> Dynamic t a3 -> Dynamic t a4 -> Dynamic t a5 -> Dynamic t a6 -> Dynamic t a7 -> Dynamic t a8 -> Dynamic t a9 -> Dynamic t a10 -> Dynamic t a11 -> Dynamic t a12 -> Dynamic t a13 -> m (Dynamic t a14)
combineDyn13 fn a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 = (combineDyn (\a13' (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10', a11', a12') -> fn a1' a2' a3' a4' a5' a6' a7' a8' a9' a10' a11' a12' a13') a13) =<< (combineDyn12 (,,,,,,,,,,,) a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)

combineDyn14 :: (Reflex t, MonadHold t m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> a13 -> a14 -> a15) -> Dynamic t a1 -> Dynamic t a2 -> Dynamic t a3 -> Dynamic t a4 -> Dynamic t a5 -> Dynamic t a6 -> Dynamic t a7 -> Dynamic t a8 -> Dynamic t a9 -> Dynamic t a10 -> Dynamic t a11 -> Dynamic t a12 -> Dynamic t a13 -> Dynamic t a14 -> m (Dynamic t a15)
combineDyn14 fn a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 = (combineDyn (\a14' (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10', a11', a12', a13') -> fn a1' a2' a3' a4' a5' a6' a7' a8' a9' a10' a11' a12' a13' a14') a14) =<< (combineDyn13 (,,,,,,,,,,,,) a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13)

combineDyn15 :: (Reflex t, MonadHold t m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> a13 -> a14 -> a15 -> a16) -> Dynamic t a1 -> Dynamic t a2 -> Dynamic t a3 -> Dynamic t a4 -> Dynamic t a5 -> Dynamic t a6 -> Dynamic t a7 -> Dynamic t a8 -> Dynamic t a9 -> Dynamic t a10 -> Dynamic t a11 -> Dynamic t a12 -> Dynamic t a13 -> Dynamic t a14 -> Dynamic t a15 -> m (Dynamic t a16)
combineDyn15 fn a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 = (combineDyn (\a15' (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10', a11', a12', a13', a14') -> fn a1' a2' a3' a4' a5' a6' a7' a8' a9' a10' a11' a12' a13' a14' a15') a15) =<< (combineDyn14 (,,,,,,,,,,,,,) a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)
