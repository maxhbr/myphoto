{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nanogallery (writeNanogalleries, computeThumbnails, computeOneThumbnail) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedFile)
import qualified Data.Set as Set
import Model (PhotoMeta (..))
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequestThrow)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath
  ( makeRelative,
    takeDirectory,
    takeFileName,
    (</>),
  )
import System.Process (callProcess)

-- | Write a minimal nanogallery-style index.html into the gallery root.
--   Expects summaries of imported images (metadata path, PhotoMeta, md5 hash).
writeNanogalleries :: FilePath -> [(FilePath, PhotoMeta, String)] -> IO ()
writeNanogalleries root summaries = do
  createDirectoryIfMissing True root
  ensureStaticAssets root
  summariesWithThumbnails <- computeThumbnails root summaries
  BL.writeFile (root </> "index.json") (renderItemsJson root summariesWithThumbnails)
  BL.writeFile (root </> "index.html") renderPage
  writeFile (root </> "debug.html") (renderDebugPage root summariesWithThumbnails)

-- | Enrich summaries with optional thumbnail paths under $root/.thumbnail/<md5>.jpg.
computeThumbnails :: FilePath -> [(FilePath, PhotoMeta, String)] -> IO [(FilePath, PhotoMeta, Maybe FilePath, String)]
computeThumbnails root summaries = mapConcurrently addThumb summaries
  where
    addThumb (metaPath, meta, md5sum) = do
      thumb <- computeOneThumbnail root metaPath md5sum
      pure (metaPath, meta, thumb, md5sum)

renderPage :: BL.ByteString
renderPage = BL.fromStrict $(embedFile "app-gallery/assets/index.html")

renderItemsJson :: FilePath -> [(FilePath, PhotoMeta, Maybe FilePath, String)] -> BL.ByteString
renderItemsJson root = A.encode . map (renderItem root)

renderItem :: FilePath -> (FilePath, PhotoMeta, Maybe FilePath, String) -> A.Value
renderItem root (imgPath, meta, mThumbPath, _) =
  let relImg = makeRelative root imgPath
      relThumb = fmap (makeRelative root) mThumbPath
      relAbouts = map (makeRelative root) (about meta)
      tagsList = Set.toList (tags meta)
      baseFields =
        [ "src" .= relImg,
          "caption" .= ("" :: String),
          "tags" .= tagsList,
          "about" .= relAbouts
        ]
      thumbField = maybe [] (\t -> ["srct" .= t]) relThumb
   in A.object (baseFields <> thumbField)

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

renderDebugPage :: FilePath -> [(FilePath, PhotoMeta, Maybe FilePath, String)] -> String
renderDebugPage root summaries =
  unlines
    [ "<!DOCTYPE html>",
      "<html lang=\"en\">",
      "<head>",
      "  <meta charset=\"UTF-8\" />",
      "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
      "  <title>myphoto gallery - debug view</title>",
      "  <style>",
      "    body { margin: 20px; background: #f5f5f5; color: #333; font-family: sans-serif; }",
      "    h1 { color: #2c3e50; }",
      "    table { width: 100%; border-collapse: collapse; background: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }",
      "    th, td { padding: 12px; text-align: left; border: 1px solid #ddd; vertical-align: top; }",
      "    th { background: #34495e; color: white; font-weight: bold; position: sticky; top: 0; }",
      "    tr:nth-child(even) { background: #f9f9f9; }",
      "    tr:hover { background: #e8f4f8; }",
      "    img { max-width: 150px; max-height: 150px; display: block; }",
      "    a { color: #3498db; text-decoration: none; }",
      "    a:hover { text-decoration: underline; }",
      "    .tag { display: inline-block; background: #3498db; color: white; padding: 2px 8px; margin: 2px; border-radius: 3px; font-size: 0.9em; }",
      "    .about-item { display: block; margin: 2px 0; }",
      "    .no-thumb { width: 150px; height: 150px; background: #ecf0f1; display: flex; align-items: center; justify-content: center; color: #7f8c8d; }",
      "  </style>",
      "</head>",
      "<body>",
      "  <h1>myphoto gallery - debug view</h1>",
      "  <table>",
      "    <thead>",
      "      <tr>",
      "        <th>Thumbnail</th>",
      "        <th>Tags</th>",
      "        <th>Path</th>",
      "        <th>About</th>",
      "      </tr>",
      "    </thead>",
      "    <tbody>"
    ]
    <> concatMap (renderDebugRow root) summaries
    <> unlines
      [ "    </tbody>",
        "  </table>",
        "</body>",
        "</html>"
      ]

renderDebugRow :: FilePath -> (FilePath, PhotoMeta, Maybe FilePath, String) -> String
renderDebugRow root (imgPath, meta, mThumbPath, md5sum) =
  let relImg = makeRelative root imgPath
      relThumb = fmap (makeRelative root) mThumbPath
      relAbouts = map (makeRelative root) (about meta)
      tagsList = Set.toList (tags meta)
      thumbCell = case relThumb of
        Just t -> "<img src=\"" <> escapeHtml t <> "\" alt=\"thumbnail\" />"
        Nothing -> "<div class=\"no-thumb\">No thumbnail</div>"
      imgCell = "<a href=\"" <> escapeHtml relImg <> "\" target=\"_blank\">" <> escapeHtml (takeFileName relImg) <> "</a>"
      tagsCell = concatMap (\t -> "<span class=\"tag\">" <> escapeHtml t <> "</span>") tagsList
      actualPathCell = escapeHtml (takeDirectory relImg)
      metaPathCell = maybe "" escapeHtml (path meta)
      aboutCell = concatMap (\a -> "<span class=\"about-item\"><a href=\"" <> escapeHtml a <> "\" target=\"_blank\">" <> escapeHtml a <> "</a></span>") relAbouts
   in "      <tr>\n"
        <> "        <td>"
        <> thumbCell
        <> "<br/>"
        <> imgCell
        <> "</td>\n"
        <> "        <td>"
        <> tagsCell
        <> "</td>\n"
        <> "        <td> acutal: "
        <> actualPathCell
        <> "<br/> meta: "
        <> metaPathCell
        <> "</td>\n"
        <> "        <td>"
        <> aboutCell
        <> "</td>\n"
        <> "      </tr>\n"

escapeHtml :: String -> String
escapeHtml = concatMap escape
  where
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '&' = "&amp;"
    escape '"' = "&quot;"
    escape '\'' = "&#39;"
    escape c = [c]

-- | Download external JS/CSS assets locally so the gallery can run offline.
ensureStaticAssets :: FilePath -> IO ()
ensureStaticAssets root = do
  let assetsDir = root </> "assets"
  downloadAsset assetsDir "jquery.min.js" "https://cdn.jsdelivr.net/npm/jquery@3.3.1/dist/jquery.min.js"
  downloadAsset assetsDir "nanogallery2.min.css" "https://cdn.jsdelivr.net/npm/nanogallery2@3/dist/css/nanogallery2.min.css"
  downloadAsset assetsDir "jquery.nanogallery2.min.js" "https://cdn.jsdelivr.net/npm/nanogallery2@3/dist/jquery.nanogallery2.min.js"
  downloadAsset assetsDir "css/nanogallery2.min.css" "https://cdn.jsdelivr.net/npm/nanogallery2@3/dist/css/nanogallery2.min.css"
  downloadAsset assetsDir "css/nanogallery2.woff.min.css" "https://cdn.jsdelivr.net/npm/nanogallery2@3/dist/css/nanogallery2.woff.min.css"
  downloadAsset assetsDir "font/ngy2_icon_font.woff" "https://cdn.jsdelivr.net/npm/nanogallery2@3/dist/css/font/ngy2_icon_font.woff"

downloadAsset :: FilePath -> FilePath -> String -> IO ()
downloadAsset dir filename url = do
  let target = dir </> filename
  createDirectoryIfMissing True (takeDirectory target)
  request <- parseRequestThrow url
  response <- httpLBS request
  BL.writeFile target (getResponseBody response)
