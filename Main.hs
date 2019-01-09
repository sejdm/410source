{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Functor ((<$>))
import Data.List (isPrefixOf)
import Data.Monoid (mappend)
import Data.Text (pack, unpack, replace, empty)
import Control.Monad
import Data.Ord

import Hakyll
import           Text.Pandoc.Options
import qualified Data.HashMap.Strict as M
import Data.List
import Data.Char
import qualified Text.Regex as RE 
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Data.Maybe
import Hakyll.Core.Util.File
import Hakyll.Core.Configuration

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler = pandocCompilerWithTransformM defaultHakyllReaderOptions pandocOptions internalTransform

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }


writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }



-- | Internal link transformation taken from,
-- | https://groups.google.com/forum/#!msg/hakyll/XbB20ak441Q/1bzn-d_2WugJ
internalTransform :: Pandoc -> Compiler Pandoc 
internalTransform = walkM internalTransform' 

internalTransform' :: Inline -> Compiler Inline 
internalTransform' orig@(Link ref a (url,title)) = 
    case RE.matchRegexAll (RE.mkRegex "(.+)\\.md") url of 
        Nothing -> return orig 
        Just (_,e,_,_) -> do 
            rep <- getRoute $ fromFilePath e 
            return $ Link ref a ((fromMaybe url rep),title) 
internalTransform' x = return x 



myConfig = defaultConfiguration {
  deployCommand = "rsync -r _site/* $HOME/mth410/ && cd $HOME/mth410 && git add * && git commit && git push"
  }


main :: IO ()
main = fullSite

fullSite = hakyllWith myConfig $ do
    -- Build tags
    tags <- AllTags <$> (buildTags "posts/*" (fromCapture "tags/*.html")) <*> (buildTagsWith (getList ";" "author") "poems/*" (fromCapture "poets/*.html")) <*> (buildTagsWith (getList "," "genre") "poems/*" (fromCapture "poets/*.html"))

    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Copy Files
    match "files/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "**pdfs/**" $ do
        route   idRoute
        compile copyFileCompiler

    -- To prevent cycle
    match "*.md" $ version "titleLine" $ do
        route   $ setExtension ".html"
        compile $ do
          pandocMathCompiler


    -- Render pages
    match "*.md" $ do
        route   $ setExtension ".html"
        compile $ do
          l <- topPostsPages
          pandocMathCompiler
            >>= loadAndApplyTemplate "templates/default.html" (newdefaultCtx tags l)
            >>= relativizeUrls

    -- Render non main pages
    match "pages/**.md" $ do
        route   $ setExtension ".html"
        compile $ do
          l <- topPostsPages
          pandocMathCompiler
            >>= loadAndApplyTemplate "templates/default.html" (newdefaultCtx tags l)
            >>= relativizeUrls

  
    -- newTags poemTags "Poems by " "templates/poetspoems.html" "templates/poemitemnopoet.html" (sortusing "title") tags
    -- newTags poemGenre "Poem Genre " "templates/poetspoems.html" "templates/poemitem.html" (sortusing "title") tags
    newTags usualTags "Posts tagged " "templates/posts.html" "templates/postitem.html" recentFirst tags


    --createSimple "Poets" "templates/onlypoets.html" "onlypoets.html" poetsCtx newdefaultCtx tags
    --createSimple "Genre" "templates/onlygenre.html" "genre.html" poetsCtx newdefaultCtx tags

    -- poemsFldr tags
    -- poemsFldrPoet tags
    assignmentsFldr tags
    postsFldr tags




    match "index.markdown" $ do
        route   $ setExtension ".html"
        compile $ do
            l <- topPostsPages
            itemTpl <- loadBody "templates/postitem.html"
            _ <- applyTemplateList itemTpl postCtx (sorted l)
            pandocMathCompiler
                >>= loadAndApplyTemplate "templates/default.html" (newdefaultCtx tags l)
                >>= relativizeUrls

 {- 
    -- Index
    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll ("posts/*"  .&&. hasVersion "justlist")
            pages <- loadAll ("*.md" .&&. hasNoVersion)
            sorted <- take 10 <$> recentFirst posts
            itemTpl <- loadBody "templates/postitem.html"
            list <- applyTemplateList itemTpl postCtx sorted
            makeItem list
                >>= loadAndApplyTemplate "templates/index.html" (homeCtx tags list)
                >>= loadAndApplyTemplate "templates/default.html" (onlyTagsCtx tags `mappend` pagesCtx pages `mappend` homeCtx tags list `mappend` postsCtx sorted)
                >>= relativizeUrls
-}

    -- Post tags

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
            sorted <- take 10 <$> recentFirst posts
            renderRss feedConfiguration feedCtx (take 10 sorted)

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
            sorted <- take 10 <$> recentFirst posts
            renderAtom feedConfiguration feedCtx sorted

    -- Read templates
    match "templates/*" $ compile templateCompiler


newTags f ttl tmplt itmtmpl g tags = do
    tagsRules (f tags) $ \tag pattern -> do
        let title = ttl ++ tag
        route idRoute
        compile $ do
            list <- postList itmtmpl tags pattern g
            l <- topPostsPages
            makeItem ""
                >>= loadAndApplyTemplate tmplt
                        (constField "title" title `mappend`
                            constField "body" list `mappend`
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html"
                        (newdefaultCtx tags l)
                >>= relativizeUrls




postCtx :: Context String
postCtx =
    pdfCtx `mappend`
    strippedfileCtx `mappend`
    dateField "date" "%e %B, %Y" `mappend`
    defaultContext


assignmentCtx :: Context String
assignmentCtx =
    pdfCtx `mappend`
    strippedfileCtx `mappend`
    defaultContext

allPostsCtx :: Context String
allPostsCtx =
    constField "title" "All posts" `mappend`
    postCtx


allAssignmentsCtx :: Context String
allAssignmentsCtx =
    constField "title" "All assignments" `mappend`
    assignmentCtx

homeCtx :: AllTags -> String -> Context String
homeCtx tags list =
    (case list of [] -> mempty; _ -> constField "posts" list) `mappend`
    constField "title" "Index" `mappend`
    field "taglist" (\_ -> renderTagCloud 1 100 (usualTags tags)) `mappend`
    defaultContext


onlyTagsCtx :: AllTags -> Context String
onlyTagsCtx tags = field "taglist" (\_ -> renderTagCloud 90 200 (usualTags tags)) `mappend`
    defaultContext

poetsCtx :: AllTags -> Context String
poetsCtx tags = field "poetslist" (\_ -> renderPoetList (poemTags tags))
  `mappend`
  field "genrelist" (\_ -> renderPoetList (poemGenre tags))
  `mappend`
    defaultContext

feedCtx :: Context String
feedCtx =
    bodyField "description" `mappend`
    postCtx

tagsCtx :: AllTags -> Context String
tagsCtx tags =
    (if tagsMap (usualTags tags) == [] then tagsField "prettytags" (usualTags tags) else mempty) `mappend`
    postCtx

pagesCtx p = listField "pages" defaultContext (return p) `mappend` pdfCtx
postsCtx p = case p of [] -> mempty; _ -> listField "sortedposts" (postCtx `mappend` defaultContext) (return p)
assignmentsCtx p = case p of [] -> mempty; _ -> listField "sortedassignments" (assignmentCtx `mappend` strippedfileCtx `mappend` defaultContext) (return p)

allCtx (p, q, r) = pagesCtx p `mappend` postsCtx q `mappend` assignmentsCtx r

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Shane - RSS feed"
    , feedDescription = "Some thoughts"
    , feedAuthorName  = "Shane"
    , feedAuthorEmail = "shane.dmello@gmail.com"
    , feedRoot        = "http://blog.shane"
    }

externalizeUrls :: String -> Item String -> Compiler (Item String)
externalizeUrls root item = return $ fmap (externalizeUrlsWith root) item

externalizeUrlsWith :: String -- ^ Path to the site root
                    -> String -- ^ HTML to externalize
                    -> String -- ^ Resulting HTML
externalizeUrlsWith root = withUrls ext
  where
    ext x = if isExternal x then x else root ++ x

-- TODO: clean me
unExternalizeUrls :: String -> Item String -> Compiler (Item String)
unExternalizeUrls root item = return $ fmap (unExternalizeUrlsWith root) item

unExternalizeUrlsWith :: String -- ^ Path to the site root
                      -> String -- ^ HTML to unExternalize
                      -> String -- ^ Resulting HTML
unExternalizeUrlsWith root = withUrls unExt
  where
    unExt x = if root `isPrefixOf` x then unpack $ replace (pack root) empty (pack x) else x

postList :: Identifier -> AllTags
         -> Pattern
         -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tmpl tags pattern preprocess' = do
    postItemTpl <- loadBody tmpl
    posts <- loadAll pattern
    processed <- preprocess' posts
    applyTemplateList postItemTpl (tagsCtx tags) processed


mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ if "mathjax" `M.member` metadata
                  then "<script type=\"text/javascript\" src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
                  else ""

strippedfileCtx = strippedfileCtx' `mappend` fileDigitCtx

strippedfileCtx' ::  Context a
strippedfileCtx' = field "strippedfile" $ return . toStrippedFile . toFilePath . itemIdentifier

fileDigitCtx ::  Context a
fileDigitCtx = field "filedigit" $ return . show . extractDigit . toStrippedFile . toFilePath . itemIdentifier

toPDFFile = (++".pdf") . toStrippedFile
toStrippedFile = reverse . takeWhile (/='/') . tail . dropWhile (/='.') . reverse


checkPdfCtx = field "haspdf" $ \item -> do
   let x = addPdfFolder $ toFilePath $ itemIdentifier item
   r <- getRoute $ fromFilePath x
   return $ case  r of
              --Just s -> "<a href=\"/" ++ x ++ "\"><img src=\"/files/pdf.png\" alt=\"PDF\" height=\"15\"></a>"
              Just s -> "<a href=\"/" ++ x ++ "\">Download PDF</a>"
              Nothing -> ""
  

addPdfFolder s = reverse $ h ++ "/sfdp" ++ (dropWhile (/='/') f)
  where f = ("fdp" ++ ) $ dropWhile (/='.') $ reverse s
        h = takeWhile (/='/') f

pdfCtx = checkPdfCtx -- "" -- pdfCtx' "."
--pdfUpCtx = checkPdfCtx ".." -- pdfCtx' ".."

pdfCtx' :: String -> Context a 
pdfCtx' p = checkPdfCtx `mappend` (field "pdfversion" $ chkCtx p)

chkCtx p item = do
    metadata <- getMetadataField (itemIdentifier item) "linkpdf"
    return $ case metadata of
               Just "true" ->  p ++ "/pdfs/"++ toPDFFile (toFilePath $ itemIdentifier item)
               Just s -> p ++ "/pdfs/"++ s
               Nothing -> ""



chkCtx' p item = do
    metadata <- getMetadataField (itemIdentifier item) "linkpdf"
    return $ case metadata of
               Just "true" ->  p ++ "/pdfs/"++ toPDFFile (toFilePath $ itemIdentifier item)
               Just s -> p ++ "./pdfs/"++ s
               Nothing -> ""

sortAssignments = sortBy (\x y -> compare (extractDigit $ toFilePath $ itemIdentifier x) (extractDigit $ toFilePath $ itemIdentifier y))

sortusing n = sortByM (\x -> (do y <- getMetadataField (itemIdentifier x) n; return (case y of; Just k -> k; Nothing -> toStrippedFile (toFilePath (itemIdentifier x)))))


extractDigit :: String -> Int
extractDigit = read . takeWhile isDigit .  dropWhile (not . isDigit)

pnewdefaultCtx t l = newdefaultCtx t l `mappend` foldersCtx "sortedpoems" (poems l)

poemsFldr = orderedFolder' "All Poems" "poems/*" "templates/poem.html" "templates/poemitem.html" "templates/poems.html" "poems.html" (sortusing "title") pnewdefaultCtx pnewdefaultCtx folderCtx (const mempty)

poemsFldrPoet = createAllPage "All Poems (by author)" "poems/*" "templates/poem.html" "templates/poemitem.html" "templates/poemsauthor.html" "poemsauthor.html" (sortusing "author") pnewdefaultCtx pnewdefaultCtx folderCtx poetsCtx

assignmentsFldr = orderedFolder "All Exercise sheets" "assignments/*" "templates/assignment.html" "templates/assignmentitem.html" "templates/assignments.html" "assignments.html" (return . sortAssignments) newdefaultCtx newdefaultCtx assignmentCtx (const mempty)

postsFldr = orderedFolder "All posts" "posts/*" "templates/post.html" "templates/postitem.html" "templates/posts.html" "posts.html" recentFirst newdefaultCtx newdefaultCtx  postCtx (const mempty)

orderedFolder ttl folder tmplt itemtmplt tmplts newfile sortFun eachctx allctx itemctx pgCtx tags = do
    match folder $ version "justlist" $ do
        route   $ setExtension ".html"
        compile $ do
          pandocMathCompiler

    orderedFolder' ttl folder tmplt itemtmplt tmplts newfile sortFun eachctx allctx itemctx pgCtx tags

  -- Will not have a persistant list available on every page
orderedFolder' ttl folder tmplt itemtmplt tmplts newfile sortFun eachctx allctx itemctx pgCtx tags = do


    match folder $ do
        route   $ setExtension ".html"
        compile $ do
          l <- topPostsPages
          (pandocMathCompiler
            >>= loadAndApplyTemplate tmplt (tagsCtx tags)
            >>= (externalizeUrls $ feedRoot feedConfiguration)
            >>= saveSnapshot "content"
            >>= (unExternalizeUrls $ feedRoot feedConfiguration)
            >>= loadAndApplyTemplate "templates/default.html" (eachctx tags l)
            >>= relativizeUrls)

    createAllPage ttl folder tmplt itemtmplt tmplts newfile sortFun eachctx allctx itemctx pgCtx tags

createAllPage ttl folder tmplt itemtmplt tmplts newfile sortFun eachctx allctx itemctx pgCtx tags = do
    create [newfile] $ do
        route idRoute
        compile $ do
            l <- topPostsPages
            posts <- sortFun =<< loadAll ((folder )  .&&. hasNoVersion)
            --sorted' <- recentFirst posts
            itemTpl <- loadBody itemtmplt
            list <- applyTemplateList itemTpl itemctx posts
            makeItem list
                >>= loadAndApplyTemplate tmplts (allFolderCtx ttl `mappend` pgCtx tags)
                >>= loadAndApplyTemplate "templates/default.html" (constField "title" ttl `mappend` allctx tags l)
                >>= relativizeUrls



createSimple ttl tmplt newfile pgCtx allctx tags = do
    create [newfile] $ do
        route idRoute
        compile $ do
            --sorted' <- recentFirst posts
            l <- topPostsPages
            makeItem ""
                >>= loadAndApplyTemplate tmplt (allFolderCtx ttl `mappend` pgCtx tags)
                >>= loadAndApplyTemplate "templates/default.html" (constField "title" ttl `mappend` allctx tags l)
                >>= relativizeUrls

sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

folderCtx :: Context String
folderCtx =
    pdfCtx `mappend`
    strippedfileCtx' `mappend`
    defaultContext

allFolderCtx  :: String -> Context String
allFolderCtx t =
    constField "title" t `mappend`
    folderCtx

foldersCtx s p = case p of [] -> mempty; _ -> listField s (folderCtx `mappend` defaultContext) (return p)




newdefaultCtx tags l = assignmentsCtx (assgns l) `mappend` postsCtx (sorted l) `mappend` mathCtx `mappend` onlyTagsCtx tags `mappend` pagesCtx (pages l) `mappend` foldersCtx "sortedpoems" (poems l) `mappend` poetsCtx tags

data AllLists = AllLists {pages :: [Item String],  sorted :: [Item String], assgns :: [Item String], poems :: [Item String]}

topPostsPages :: Compiler AllLists
topPostsPages = do
          ps <- loadAll ("*.md" .&&. hasVersion "titleLine")
          posts <- loadAll ("posts/*"  .&&. hasVersion "justlist")
          as <- loadAll ("assignments/*"  .&&. hasVersion "justlist")
          pms <- loadAll ("poems/*"  .&&. hasVersion "justlist")
          sd <- take 5 <$> recentFirst posts
          return (AllLists ps sd (take 1 (reverse $ sortAssignments as)) pms)

getList :: MonadMetadata m => String -> String -> Identifier -> m [String]
getList delim s identifier = do
    metadata <- getMetadata identifier
    return $ fromMaybe [] $
        (map trim . splitAll delim <$> lookupString s metadata)

getAuthors :: MonadMetadata m => Identifier -> m [String]
getAuthors identifier = do
    metadata <- getMetadata identifier
    return $ fromMaybe [] $
        (map trim . splitAll ";" <$> lookupString "author" metadata)

data AllTags = AllTags {usualTags :: Tags, poemTags :: Tags, poemGenre :: Tags}

renderPoetList = renderTags makeLink (intercalate "\n ")
  where
    makeLink tag url count _ _ = renderHtml $
        H.li (H.a ! A.href (toValue url) $ toHtml (tag ++ " (" ++ show count ++ ")"))
