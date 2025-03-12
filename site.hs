--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc
import Text.Pandoc (Extension (Ext_tex_math_single_backslash))
import Text.Pandoc.Highlighting

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  let mathExtensions =
        [ Ext_tex_math_single_backslash
        , Ext_tex_math_double_backslash
        , Ext_latex_macros
        ]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = extensionsFromList $ extensionsToList defaultExtensions ++ mathExtensions
      writerOptions =
        defaultHakyllWriterOptions
          { writerExtensions = newExtensions
          , writerHTMLMathMethod = MathJax ""
          }
   in pandocCompilerWith defaultHakyllReaderOptions writerOptions

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
  match ("images/*" .||. "fonts/*") $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler'
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration{destinationDirectory = "docs", previewPort = 8081}

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> teaserField "teaser" "content"
    <> defaultContext
