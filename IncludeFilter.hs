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


import           Control.Monad
import           Data.List
import qualified Data.Char as C
import qualified Data.Map as Map
import           Control.Error (readMay, fromMaybe)
import           System.Directory
import           System.IO

import           Text.Pandoc
import           Text.Pandoc.Shared (uniqueIdent, stringify)
import           Text.Pandoc.Error
import           Text.Pandoc.JSON
import           Text.Pandoc.Walk
import qualified Text.Pandoc.Builder as B

stripPandoc :: Int -> Either PandocError Pandoc -> [Block]
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

fileContentAsString :: String -> IO String
fileContentAsString file = withFile file ReadMode $ \handle -> do
  hSetEncoding handle utf8
  hGetContents handle

fileContentAsBlocks :: Int -> String -> IO [Block]
fileContentAsBlocks changeInHeaderLevel file = do
  let contents = fileContentAsString file
  let p = fmap (readMarkdown def) contents
  stripPandoc changeInHeaderLevel <$> p

getProcessableFileList :: String -> [String]
getProcessableFileList list = do
  let f = lines list
  filter (\x -> not $ "#" `isPrefixOf` x) f

simpleInclude :: Int -> String -> [String] -> IO [Block]
simpleInclude changeInHeaderLevel list classes = do
  let toProcess = getProcessableFileList list
  fmap concat (fileContentAsBlocks changeInHeaderLevel `mapM` toProcess)

includeCodeBlock :: Block -> IO [Block]
includeCodeBlock (CodeBlock (_, classes, _) list) = do
  let filePath = head $ lines list
  let content = fileContentAsString filePath
  let newclasses = filter (\x -> "include" `isPrefixOf` x || "code" `isPrefixOf` x) classes
  let blocks = fmap (B.codeBlockWith ("", newclasses, [])) content
  fmap B.toList blocks

cropContent :: [String] -> (String, String) -> [String]
cropContent lines (skip, count) =
  if not $ null skip then
    if not $ null count then
      take (read count) (drop (read skip) lines)
    else
      drop (read skip) lines
  else
    lines

includeCropped :: Block -> IO [Block]
includeCropped (CodeBlock (_, classes, _) list) = do
  let [filePath, skip, count] = lines list
  let content = fileContentAsString filePath
  let croppedContent = unlines <$> ((cropContent . lines <$> content) <*> pure (skip, count))
  fmap (stripPandoc 0 . readMarkdown def) croppedContent

doInclude :: Block -> IO [Block]
doInclude cb@(CodeBlock (_, classes, options) list)
  | "include" `elem` classes = do
    let changeInHeaderLevel = fromMaybe 0 $ readMay =<< "header-change" `lookup` options
    simpleInclude changeInHeaderLevel list classes
  | "include-indented" `elem` classes = do
    let newClasses = ("include" :) . delete "include-indented" $ classes
    let newOptions = ("header-change","1") : options
    doInclude $ CodeBlock ("", newClasses, newOptions) list
  | "code" `elem` classes = includeCodeBlock cb
  | "cropped" `elem` classes = includeCropped cb
doInclude x = return [x]

main :: IO ()
main = toJSONFilter doInclude
