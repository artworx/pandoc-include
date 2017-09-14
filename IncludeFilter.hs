#!/usr/bin/env runhaskell

{-
The MIT License (MIT)

Copyright (c) 2015 Dániel Stein <daniel@stein.hu>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{-|
A Pandoc filter that replaces include labeled Code Blocks with the contents of
the referenced files. Even nested, recursive includes.

Based on the scripting tutorial for Pandoc:
http://pandoc.org/scripting.html#include-files

The Code Blocks like the following will include every file in a new line. The
reference paths should be either absolute or relative to the folder where the
pandoc command will be executed.

> ```include
> /absolute/file/path.md
> relative/to/the/command/root.md
> #do/not/include/this.md
> ```

If the file does not exist, it will be skipped completely. No warnings, no
residue, nothing. Putting an # as the first character in the line will make the
filter skip that file.

For now the nested includes only work for two levels, after that the source
will be inserted and not parsed.

Note: the metadata from the included source files are discarded.

Alternatively, use one of the following to increase all the header levels in the
included file. The first option is a shortcut for incrementing the level by 1.
The second demonstrates an increase of 2.

> ```include-indented

> ```{ .include header-change=2 }

If the header level is increased, the title from the included file is inserted at the
beginning of the included file as a header, at the level of the header level change. For
example, if the header is incremented by 1, the title is inserted as a level 1 heading.

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Monad
import           Data.List
import qualified Data.Char as C
import qualified Data.Map as Map
import           Control.Error (readMay, fromMaybe, note)
import           System.Directory
import           System.FilePath
import           System.IO
import qualified Control.Exception as E
import qualified Data.Set as Set

import           Lens.Micro
import           Lens.Micro.Mtl

import           Text.Pandoc
import           Text.Pandoc.Readers (getReader)
import           Text.Pandoc.App (convertWithOpts', defaultOpts, options,
                                  parseOptions, Opt(..), APIOpt(..), defaultAPIOpts)
import           Text.Pandoc.Error (PandocError, handleError)
import           Text.Pandoc.Shared (uniqueIdent, stringify)
import           Text.Pandoc.Class
import           Text.Pandoc.Error
import           Text.Pandoc.JSON
import           Text.Pandoc.Walk
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.UTF8 as UTF8

import           Data.Text (Text)
import           Data.Text.Encoding
import qualified Data.Text as T
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

main :: IO ()
main = (\(e :: PandocError) -> handleError (Left e)) `E.handle`  do
    opts <- parseOptions options defaultOpts
    api <- apiOpts opts
    convertWithOpts' api opts

apiOpts :: Opt -> IO APIOpt
apiOpts opts = do
    readerExts <- error "Can't parse options" `either` (pure . snd) $ getReader @PandocPure =<< readerName
    writerName <- pure . nonPdfWriterName $ optWriter opts
    abbrevs <- fmap (error "Can't read abbreviations" `either` id) . runIO $ do
        abs <- case optAbbreviations opts of
                Nothing -> UTF8.toString <$> readDataFile "abbreviations"
                Just f  -> UTF8.toString <$> readFileStrict f
        pure . Set.fromList . filter (not . null) . lines $ abs
    pure $ defaultAPIOpts &~ do
        _apiFilterFunction .= (walk' . doInclude $
            readerOpts writerName abbrevs readerExts
            )
    where
        walk' f = bottomUpM $ fmap concat . traverse f

        outputFile = fromMaybe "-" (optOutputFile opts)

        format = takeWhile (`notElem` ['+','-']) . takeFileName

        nonPdfWriterName = defaultWriterName outputFile `maybe` map C.toLower

        readerOpts writerName abbrevs readerExts = def
          { readerStandalone = optStandalone opts ||
                  (not . isTextFormat . format $ writerName)
          , readerColumns = optColumns opts
          , readerTabStop = optTabStop opts
          , readerIndentedCodeClasses = optIndentedCodeClasses opts
          , readerDefaultImageExtension =
             optDefaultImageExtension opts
          , readerTrackChanges = optTrackChanges opts
          , readerAbbreviations = abbrevs
          , readerExtensions = readerExts
          }

        readerName = note "No reader" . fmap (map C.toLower) $ optReader opts

        isTextFormat :: String -> Bool
        isTextFormat s = s `notElem` ["odt","docx","epub","epub3"]

        defaultWriterName :: FilePath -> String
        defaultWriterName "-" = "html" -- no output file
        defaultWriterName x =
          case takeExtension (map C.toLower x) of
            ""          -> "markdown" -- empty extension
            ".tex"      -> "latex"
            ".latex"    -> "latex"
            ".ltx"      -> "latex"
            ".context"  -> "context"
            ".ctx"      -> "context"
            ".rtf"      -> "rtf"
            ".rst"      -> "rst"
            ".s5"       -> "s5"
            ".native"   -> "native"
            ".json"     -> "json"
            ".txt"      -> "markdown"
            ".text"     -> "markdown"
            ".md"       -> "markdown"
            ".markdown" -> "markdown"
            ".textile"  -> "textile"
            ".lhs"      -> "markdown+lhs"
            ".texi"     -> "texinfo"
            ".texinfo"  -> "texinfo"
            ".db"       -> "docbook"
            ".odt"      -> "odt"
            ".docx"     -> "docx"
            ".epub"     -> "epub"
            ".org"      -> "org"
            ".asciidoc" -> "asciidoc"
            ".adoc"     -> "asciidoc"
            ".fb2"      -> "fb2"
            ".opml"     -> "opml"
            ".icml"     -> "icml"
            ".tei.xml"  -> "tei"
            ".tei"      -> "tei"
            ".ms"       -> "ms"
            ".roff"     -> "ms"
            ['.',y]     | y `elem` ['1'..'9'] -> "man"
            _           -> "html"

_apiFilterFunction :: APIOpt `Lens'` (Pandoc -> IO Pandoc)
_apiFilterFunction f APIOpt{..} = (\apiFilterFunction -> APIOpt{apiFilterFunction, ..}) <$> f apiFilterFunction
{-# INLINE _apiFilterFunction #-}

stripPandoc _ (Left _) = [Null]
stripPandoc changeInHeaderLevel (Right (Pandoc meta blocks)) = maybe id (:) title modBlocks
    where
         getTitle (Meta (Map.lookup "title" -> Just (MetaInlines inls))) = Just inls
         getTitle _ = Nothing
         theTitle = getTitle meta
         modBlocks = modifyHeaderLevelBlockWith changeInHeaderLevel (titleRef <$> theTitle) <$> blocks
         title = do
             inls <- theTitle
             guard $ changeInHeaderLevel > 0
             Just $ Header changeInHeaderLevel (titleRef inls,["section-title"],[]) inls
         -- WARNING titleRef doesn't check that titles are unique; for that try uniqueIdent.
         titleRef = stringify . fmap (lowerCase . dashFromSpace)
         dashFromSpace Space = Str "-"
         dashFromSpace x = x
         lowerCase (Str x) = Str (fmap C.toLower x)
         lowerCase x = x

amendIdentifier :: String -> Attr -> Attr
amendIdentifier title (ident, cls, kvs) = (concat [title, "-", ident], cls, kvs)

modifyHeaderLevelBlockWith :: Int -> Maybe String -> Block -> Block
modifyHeaderLevelBlockWith n mtitle (Header int att inls) =
        Header (int + n) (maybe id amendIdentifier mtitle att) inls
modifyHeaderLevelBlockWith _ _ x = x

modifyHeaderLevelWith :: Int -> Pandoc -> Pandoc
modifyHeaderLevelWith n = walk (modifyHeaderLevelBlockWith n mempty)

fileContentAsText :: FilePath -> IO Text
fileContentAsText file = withFile file ReadMode $ \handle -> do
  hSetEncoding handle utf8
  contents <- BS.hGetContents handle
  pure . decodeUtf8 $ contents

fileContentAsBlocks :: ReaderOptions -> Int -> FilePath -> IO [Block]
fileContentAsBlocks ropts changeInHeaderLevel file = do
  let contents = fileContentAsText file
  let p = runIO . readMarkdown ropts =<< contents
  stripPandoc changeInHeaderLevel <$> p

getProcessableFileList :: String -> [String]
getProcessableFileList list = do
  let f = lines list
  filter (\x -> not $ "#" `isPrefixOf` x) f

simpleInclude :: ReaderOptions -> Int -> String -> [String] -> IO [Block]
simpleInclude ropts changeInHeaderLevel list classes = do
  let toProcess = getProcessableFileList list
  fmap concat (fileContentAsBlocks ropts changeInHeaderLevel `mapM` toProcess)

includeCodeBlock :: Block -> IO [Block]
includeCodeBlock (CodeBlock (_, classes, _) list) = do
  let filePath = head $ lines list
  let content = fileContentAsText filePath
  let newclasses = filter (\x -> "include" `isPrefixOf` x || "code" `isPrefixOf` x) classes
  let blocks = fmap (B.codeBlockWith ("", newclasses, []) . T.unpack) content
  fmap B.toList blocks

cropContent :: [Text] -> (String, String) -> [Text]
cropContent lines (skip, count) =
  if not $ null skip then
    if not $ null count then
      take (read count) (drop (read skip) lines)
    else
      drop (read skip) lines
  else
    lines

includeCropped :: ReaderOptions -> Block -> IO [Block]
includeCropped ropts (CodeBlock (_, classes, _) list) = do
  let [filePath, skip, count] = lines list
  let content = fileContentAsText filePath
  let croppedContent = T.unlines <$> ((cropContent . T.lines <$> content) <*> pure (skip, count))
  fmap (stripPandoc 0) . runIO . readMarkdown ropts =<< croppedContent

doInclude :: ReaderOptions -> Block -> IO [Block]
doInclude ropts cb@(CodeBlock (_, classes, options) list)
  | "include" `elem` classes = do
    let changeInHeaderLevel = fromMaybe 0 $ readMay =<< "header-change" `lookup` options
    simpleInclude ropts changeInHeaderLevel list classes
  | "include-indented" `elem` classes = do
    let newClasses = ("include" :) . delete "include-indented" $ classes
    let newOptions = ("header-change","1") : options
    doInclude ropts $ CodeBlock ("", newClasses, newOptions) list
  | "code" `elem` classes = includeCodeBlock cb
  | "cropped" `elem` classes = includeCropped ropts cb
doInclude _ x = return [x]
