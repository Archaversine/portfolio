--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import Text.Pandoc.Highlighting
import Text.Pandoc.Options

--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration { destinationDirectory = "docs" }

pandocCodeStyle :: Style
pandocCodeStyle = kate

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWith
                     defaultHakyllReaderOptions 
                     defaultHakyllWriterOptions { writerHighlightMethod = Skylighting pandocCodeStyle
                                                , writerHTMLMathMethod = MathJax ""
                                                }

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do 
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx `mappend` defaultContext)
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx `mappend` defaultContext)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    mathCtx                                  `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (mathCtx `mappend` indexCtx)
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["css/syntax.css"] $ do 
        route idRoute 
        compile $ do 
            makeItem $ styleToCss pandocCodeStyle


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do 
    metadata <- getMetadata $ itemIdentifier item
    return $ case lookupString "mathjax" metadata of 
        Nothing -> ""
        Just _  -> "<script type=\"text/javascript\" src=\"//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
        -- Just _  -> "<script type=\"text/javascript\" src=\"/js/mathjax.js\"></script>"
