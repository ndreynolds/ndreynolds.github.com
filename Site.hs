{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid                    ( mappend )
import           Control.Monad                  ( forM_ )
import           Hakyll
import           Text.Pandoc
import           Text.Pandoc.SideNote           ( usingSideNotes )

main :: IO ()
main = hakyll $ do
  forM_ ["js/*", "images/*", "fonts/**/*"] $ \path -> match path $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/*" $ compile templateCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile
      $   pandocWithTocAndSidenotes
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

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots "posts/*" "content"
        renderAtom feedConfiguration feedCtx posts

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots "posts/*" "content"
        renderRss feedConfiguration feedCtx posts

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

pandocWithTocAndSidenotes :: Compiler (Item String)
pandocWithTocAndSidenotes = pandocCompilerWithTransform readerOpts
                                                        writerOpts
                                                        usingSideNotes
 where
  readerOpts = defaultHakyllReaderOptions
  writerOpts = defaultHakyllWriterOptions { writerTableOfContents = True }

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" `mappend` postCtx

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "ndreynolds.com"
    , feedDescription = "Writings and musings about software development"
    , feedAuthorName  = "Nick Reynolds"
    , feedAuthorEmail = "ndreynolds@posteo.de"
    , feedRoot        = "https://ndreynolds.com"
    }
