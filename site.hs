{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable
import Data.List (stripPrefix)
import System.FilePath ((</>))
import Hakyll

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route idRoute
        compile copyFileCompiler

    match "node_modules/@fortawesome/fontawesome-free/webfonts/*.*" $ do
        route $ customRoute (faFontRoute . toFilePath)
        compile copyFileCompiler

    match (fromList ["pages/download.md", "pages/faq.md", "pages/history.md"]) $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    create ["blog/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let
                archiveCtx =
                    fold
                        [ listField "posts" postCtx (return posts)
                        , constField "title" "Archives"
                        , defaultContext
                        ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let
                indexCtx =
                    fold
                        [ listField "posts" postCtx (return posts)
                        , defaultContext
                        ]

            pandocCompiler
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match "proposal/*.html" $ do
        route idRoute
        compile $ getResourceBody >>= relativizeUrls

    match "proposal-1.0/*.html" $ do
        route idRoute
        compile $ getResourceBody >>= relativizeUrls

    match "proposal-1.1/*.html" $ do
        route idRoute
        compile $ getResourceBody >>= relativizeUrls

postCtx :: Context String
postCtx =
    fold
        [ dateField "date" "%B %e, %Y"
        , defaultContext
        ]

faFontRoute :: FilePath -> FilePath
faFontRoute x
    | Just y <- stripPrefix "node_modules/@fortawesome/fontawesome-free/webfonts/" x
    = "css" </> "fonts" </> y
    | otherwise = error $ "Unexpected fontawesome font of " ++ x
