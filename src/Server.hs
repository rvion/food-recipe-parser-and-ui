{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Server where

import Imports
import           Data.Text           (Text)
import           Yesod
import           Yesod.Form.Jquery
import Types

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/person PersonR POST
|]

instance Yesod App
instance RenderMessage App FormMessage where renderMessage _ _ = defaultFormMessage
instance YesodJquery App

personForm :: Maybe Recipe -> Html -> MForm Handler (FormResult Recipe, Widget)
personForm mbr = renderDivs' mbr $ Recipe
    <$> areq textField "Name" Nothing
    <*> areq (check vv $ selectFieldList  [("msg"::Text, [])]) "Ingredients" Nothing
    <*> areq (check vv $ selectFieldList  [("msg"::Text, [])]) "Ingredients" Nothing
    where 
        vv = const $ Left ("nope" :: Text)
        renderDivs' = const renderDivs

getHomeR :: Handler Html
getHomeR = do
    (widget, enctype) <- generateFormPost (personForm Nothing)
    defaultLayout
        [whamlet|
            <form method=post action=@{PersonR} enctype=#{enctype}>
                ^{widget}
                <button>Submit
        |]

postPersonR :: Handler Html
postPersonR = do
    ((result, widget), enctype) <- runFormPost (personForm Nothing)
    case result of
        FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{PersonR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]

run :: Int -> IO ()
run port = warp port App