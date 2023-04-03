module Handlers.Languages where

import Data.Text.Lazy qualified as L

import Web.Scotty.Trans

import Database
import Handlers.Utils
import State

handlers :: ScottyT L.Text HiobeM ()
handlers = do
  get "/languages/list" $ do
    trackPath
    langs <- withDB $ \conn ->
      listLangsStream conn
    json langs

  get "/languages/count/have/:lang" $ do
    trackPath
    lang <- param "lang"
    trackLang lang
    count <- withDB $ \conn ->
      countLangStream conn LangHave lang
    text . L.pack $ show count

  get "/languages/count/want/:lang" $ do
    trackPath
    lang <- param "lang"
    trackLang lang
    count <- withDB $ \conn ->
      countLangStream conn LangWant lang
    text . L.pack $ show count

  get "/languages/hist/have" $ do
    trackPath
    langCounts <- withDB $ \conn ->
      buildHistStream conn LangHave
    json langCounts

  get "/languages/hist/want" $ do
    trackPath
    langCounts <- withDB $ \conn ->
      buildHistStream conn LangWant
    json langCounts
