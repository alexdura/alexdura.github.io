--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified Hakyll.Core.Metadata as Meta
import Text.Pandoc



--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration {
  destinationDirectory = "docs"
  }


main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "files/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" copyrightCtx
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
              copyrightCtx
        makeItem ""
          >>= loadAndApplyTemplate "templates/publications.html" publicationsCtx
          >>= loadAndApplyTemplate "templates/default.html" publicationsCtx
          >>= relativizeUrls

    match "projects/*" $ do
      route idRoute
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/proj.html" postCtx
        >>= relativizeUrls

    create ["projects.html"] $ do
      route idRoute
      compile $ do
        projects <- recentFirst =<< loadAll "projects/*"
        let projectsCtx =
              listField "projects" pubCtx (return projects) `mappend`
              constField "title" "Projects" `mappend`
              copyrightCtx
        makeItem ""
          >>= loadAndApplyTemplate "templates/projects.html" projectsCtx
          >>= loadAndApplyTemplate "templates/default.html" projectsCtx
          >>= relativizeUrls


    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    copyrightCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            publications <- recentFirst =<< loadAll ("publications/*" .&&. hasNoVersion)
            projects <- recentFirst =<< loadAll "projects/*"

            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    listField "publications" pubCtx (return publications) `mappend`
                    listField "projects" pubCtx (return projects) `mappend`
                    constField "title" "Alexandru Dura" `mappend`
                    copyrightCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
copyrightCtx :: Context String
copyrightCtx =
  constField "copyrightdate" "2025" `mappend`
  defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    copyrightCtx

pubCtx :: Context String
pubCtx =
  dateField "date" "%Y" `mappend`
  copyrightCtx

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }
