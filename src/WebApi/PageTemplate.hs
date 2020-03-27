{-# LANGUAGE OverloadedStrings #-}
module WebApi.PageTemplate where

import Reflex.Dom.Core hiding (link)
import Data.Monoid ((<>))
--import Control.Monad.IO.Class (MonadIO)

pageTemplate :: (DomBuilder t m) => m ()
pageTemplate = do
    let meta attrs = elAttr "meta" attrs $ return ()
        link attrs = elAttr "link" attrs $ return ()
        script src = elAttr "script" ("src" =: src) $ return ()
        mainCSS = "/css/main.css"
        scriptJS = "/js/script.js"
    meta ( "charset" =: "utf-8" )
    meta ( "name"    =: "viewport"
        <> "content" =: "width=device-width, initial-scale=1")
    link ( "href" =: "img/favicon.png"
        <> "rel"  =: "icon")
    link ( "href" =: "http://fonts.googleapis.com/css?family=Nunito:300"
        <> "rel"  =: "stylesheet")
    el "title" $ text "Api Console"
    link ( "rel" =: "stylesheet"
        <> "href" =: mainCSS)
    script scriptJS
