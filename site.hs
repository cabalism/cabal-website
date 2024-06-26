{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable
import Data.List (stripPrefix)
import Hakyll
import System.FilePath ((</>))

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

    match
        ( fromList
            ["pages/download.md", "pages/faq.md", "pages/history.md"]
        )
        $ do
            route $ setExtension "html"
            compile $
                pandocCompiler
                    >>= loadAndApplyTemplate "templates/post.html" dateCtx
                    >>= loadAndApplyTemplate "templates/default.html" defaultContext
                    >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" dateCtx
                >>= loadAndApplyTemplate "templates/default.html" dateCtx
                >>= relativizeUrls

    match "release-notes/*" $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" dateCtx
                >>= loadAndApplyTemplate "templates/default.html" dateCtx
                >>= relativizeUrls

    create ["releases/index.html"] $ do
        route idRoute
        compile $ do
            versions <- loadAll "release-notes/*"
            let
                ctx =
                    fold
                        [ listField "versions" defaultContext (return versions)
                        , defaultContext
                        ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/releases.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    create ["blog/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let
                ctx =
                    fold
                        [ listField "posts" dateCtx (return posts)
                        , defaultContext
                        ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let
                indexCtx =
                    fold
                        [ listField "posts" dateCtx (return posts)
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

dateCtx :: Context String
dateCtx =
    fold
        [ dateField "date" "%Y-%m-%d"
        , defaultContext
        ]

faFontRoute :: FilePath -> FilePath
faFontRoute x
    | Just y <- stripPrefix "node_modules/@fortawesome/fontawesome-free/webfonts/" x =
        "css" </> "fonts" </> y
    | otherwise = error $ "Unexpected fontawesome font of " ++ x
