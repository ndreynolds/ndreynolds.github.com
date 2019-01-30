{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid                    ( mappend )
import           Control.Monad                  ( forM_ )
import           Hakyll
import           Text.Pandoc

main :: IO ()
main = hakyll $ do
  forM_ ["js/*", "images/*", "fonts/*"] $ \path -> match path $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (fromList ["about.rst", "contact.markdown"]) $ do
    route $ setExtension "html"
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/layout.html" defaultContext
      >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompilerWith defaultHakyllReaderOptions withToc
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html"        postCtx
      >>= loadAndApplyTemplate "templates/post-layout.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      let archiveCtx =
            listField "posts" teaserCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html"     archiveCtx
        >>= loadAndApplyTemplate "templates/post-layout.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      let indexCtx =
            listField "posts" teaserCtx (return posts)
              `mappend` constField "title" "Home"
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/layout.html" indexCtx
        >>= relativizeUrls
 where
  withToc = defaultHakyllWriterOptions
      { writerTableOfContents = True
      , writerTemplate        = Just "<div class='toc'><h3>Contents</h3>$toc$</div>\n$body$"
      }


postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" `mappend` postCtx
