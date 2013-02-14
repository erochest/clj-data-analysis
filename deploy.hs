{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import ClassyPrelude
import Shelly
import Data.Time
import Data.Text.Lazy as LT hiding (isPrefixOf, filter)

default (LT.Text)

isHidden :: FilePath -> Bool
isHidden = isPrefixOf "." . toTextIgnore . filename

git_ :: LT.Text -> [LT.Text] -> Sh ()
git_ = command1_ "git" []

main :: IO ()
main = shelly $ verbosely $ do
    run_ "./dist/build/site/site" ["rebuild"]
    chdir "_deploy" $ git_ "checkout" ["gh-pages"]
    mapM_ rm_rf =<< filter (not . isHidden) <$> ls "_deploy"
    mapM_ (`cp_r` "_deploy") =<< ls "_site"
    chdir "_deploy" $ do
        now <- liftIO getCurrentTime
        git_ "add"    ["-A"]
        git_ "commit" ["-m", "deployed site on " ++ show now]
        git_ "push"   []

