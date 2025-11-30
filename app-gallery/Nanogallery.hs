{-# LANGUAGE OverloadedStrings #-}

module Nanogallery (writeNanogalleries, computeThumbnails, computeOneThumbnail) where

import Data.List (intercalate)
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import qualified Data.Set as Set
import Model (PhotoMeta (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath
  ( makeRelative
  , takeDirectory
  , takeFileName
  , (</>)
  )
import System.Process (callProcess)

-- | Write a minimal nanogallery-style index.html into the gallery root.
--   Expects summaries of imported images (metadata path, PhotoMeta, md5 hash).
writeNanogalleries :: FilePath -> [(FilePath, PhotoMeta, String)] -> IO ()
writeNanogalleries root summaries = do
  createDirectoryIfMissing True root
  summariesWithThumbnails <- computeThumbnails root summaries
  writeFile (root </> "index.html") (renderPage root summariesWithThumbnails)

-- | Enrich summaries with optional thumbnail paths under $root/.thumbnail/<md5>.jpg.
computeThumbnails :: FilePath -> [(FilePath, PhotoMeta, String)] -> IO [(FilePath, PhotoMeta, Maybe FilePath, String)]
computeThumbnails root summaries = mapM addThumb summaries
  where
    addThumb (metaPath, meta, md5sum) = do
      thumb <- computeOneThumbnail root metaPath md5sum
      pure (metaPath, meta, thumb, md5sum)

renderPage :: FilePath -> [(FilePath, PhotoMeta, Maybe FilePath, String)] -> String
renderPage root summaries =
  unlines
    [ "<!DOCTYPE html>"
    , "<html lang=\"en\">"
    , "<head>"
    , "  <meta charset=\"UTF-8\" />"
    , "  <meta name=\"viewport\" content=\"user-scalable=no, width=device-width, initial-scale=1, maximum-scale=1\">"
    , "  <title>myphoto gallery</title>"
    , "  <script src=\"https://cdn.jsdelivr.net/npm/jquery@3.3.1/dist/jquery.min.js\" type=\"text/javascript\"></script>"
    , "  <link href=\"https://cdn.jsdelivr.net/npm/nanogallery2@3/dist/css/nanogallery2.min.css\" rel=\"stylesheet\" type=\"text/css\">"
    , "  <script type=\"text/javascript\" src=\"https://cdn.jsdelivr.net/npm/nanogallery2@3/dist/jquery.nanogallery2.min.js\"></script>"
    , "  <style>body { margin: 0; background: #0b0b0b; color: #eee; font-family: sans-serif; }</style>"
    , "</head>"
    , "<body>"
    , "  <div id=\"nanogallery\"></div>"
    , "  <script>"
    , "    const items = " <> renderItems root summaries <> ";"
    , "    jQuery('#nanogallery').nanogallery2({"
    , "      items: items.map(it => (Object.assign({"
    , "        src: it.src,"
    , "        title: it.caption,"
    , "        tags: (it.tags || []).join(' ')"
    , "      }, it.srct ? { srct: it.srct } : {}))),"
    , "      galleryFilterTags: true,"
    , "      galleryFilterTagsMode: 'multiple',"
    , "      thumbnailHeight: 250, thumbnailWidth: 250,"
    , "      thumbnailAlignment: 'fillWidth',"
    , "      galleryDisplayMode: 'fullContent',"
    , "      gallerySorting: 'random',"
    , "      thumbnailGutterWidth: 10, thumbnailGutterHeight: 10,"
    , "      thumbnailBorderHorizontal: 2, thumbnailBorderVertical: 2,"
    , "      galleryDisplayTransitionDuration: 1000,"
    , "      thumbnailDisplayTransition: 'slideRight',"
    , "      thumbnailDisplayTransitionDuration: 300,"
    , "      thumbnailDisplayInterval: 150,"
    , "      thumbnailDisplayOrder: 'colFromRight',"
    , "      thumbnailLabel: { display: true, position:'onBottomOverImage', hideIcons: true, titleFontSize: '1em', align: 'left', titleMultiLine:true, displayDescription: false},"
    , "      thumbnailToolbarImage: null,"
    , "      thumbnailToolbarAlbum: null,"
    , "      thumbnailHoverEffect2: 'label_font-size_1em_1.5em|title_backgroundColor_rgba(255,255,255,0.34)_rgba(((35,203,153,0.8)|title_color_#000_#fff|image_scale_1.00_1.10_5000|image_rotateZ_0deg_4deg_5000',"
    , "      touchAnimation: true,"
    , "      touchAutoOpenDelay: 800,"
    , "      galleryTheme : {"
    , "        thumbnail: { titleShadow : 'none', titleColor: '#fff', borderColor: '#fff' },"
    , "        navigationBreadcrumb: { background : '#3C4B5B' },"
    , "        navigationFilter: { background : '#003C3F', backgroundSelected: '#2E7C7F', color: '#fff' }"
    , "      },"
    , "      locationHash: false"
    , "    });"
    , "  </script>"
    , "</body>"
    , "</html>"
    ]

renderItems :: FilePath -> [(FilePath, PhotoMeta, Maybe FilePath, String)] -> String
renderItems root =
  (\xs -> "[" <> intercalate "," xs <> "]")
    . map (renderItem root)

renderItem :: FilePath -> (FilePath, PhotoMeta, Maybe FilePath, String) -> String
renderItem root (imgPath, meta, mThumbPath, _) =
  let relImg = makeRelative root imgPath
      relThumb = fmap (makeRelative root) mThumbPath
      relAbouts = map (makeRelative root) (about meta)
      tagsList = Set.toList (tags meta)
   in "{"
        <> field "src" relImg <> ","
        <> maybe "" (\t -> field "srct" t <> ",") relThumb
        <> field "caption" "" <> ","
        <> fieldArray "tags" tagsList <> ","
        <> fieldArray "about" relAbouts <> ","
        <> "}"

field :: String -> String -> String
field k v = show k <> ":" <> show v

fieldArray :: String -> [String] -> String
fieldArray k vs = show k <> ":[" <> intercalate "," (map show vs) <> "]"

-- | Ensure a thumbnail exists under root/.thumbnail/<md5>.jpg and return its path if successful.
computeOneThumbnail :: FilePath -> FilePath -> String -> IO (Maybe FilePath)
computeOneThumbnail root imgPath md5sum = do
  let thumbDir = root </> ".thumbnail"
      thumbPath = thumbDir </> md5sum <> ".jpg"
  exists <- doesFileExist thumbPath
  if exists
    then pure (Just thumbPath)
    else do
      createDirectoryIfMissing True thumbDir
      res <- try (callProcess "magick" [imgPath, "-resize", "250x250^", "-gravity", "center", "-extent", "250x250", thumbPath]) :: IO (Either SomeException ())
      case res of
        Left _ -> pure Nothing
        Right _ -> pure (Just thumbPath)
