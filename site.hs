--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified Hakyll.Core.Metadata as Meta
import Text.Pandoc



--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "publications/*.pdf" $ do
      route $ setExtension "html"
      compile $ do
        makeItem "" >>= loadAndApplyTemplate "templates/pub.html" pubCtx
        >>= relativizeUrls

    match "publications/*.pdf" $ version "pdf" $ do
      route $ idRoute
      compile $ copyFileCompiler

    create ["publications.html"] $ do
      route idRoute
      compile $ do
        publications <- recentFirst =<< loadAll ("publications/*" .&&. hasNoVersion)
        let publicationsCtx =
              listField "publications" pubCtx (return publications) `mappend`
              constField "title" "Publications" `mappend`
              defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/publications.html" publicationsCtx
          >>= loadAndApplyTemplate "templates/default.html" publicationsCtx
          >>= relativizeUrls


    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            publications <- recentFirst =<< loadAll ("publications/*" .&&. hasNoVersion)

            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    listField "publications" pubCtx (return publications) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

pubCtx :: Context String
pubCtx =
  dateField "date" "%Y" `mappend`
  defaultContext

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }
