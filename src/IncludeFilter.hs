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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module IncludeFilter
  ( pandoc
  , runFilter
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Monoid
import           Data.List
import qualified Data.Char as C
import qualified Data.Map as Map
import           Control.Error (readMay, fromMaybe)
import           System.FilePath
import           System.IO
import           System.IO.Temp
import qualified Control.Exception as E

import           Lens.Micro
import           Lens.Micro.Mtl

import           Text.Pandoc
import           Text.Pandoc.App (convertWithOpts', defaultOpts, options,
                                  parseOptions, Opt(..), APIOpt(..), defaultAPIOpts,
                                  toAST)
import           Text.Pandoc.Error (PandocError, handleError)
import           Text.Pandoc.Shared (uniqueIdent, stringify)
import           Text.Pandoc.JSON
import           Text.Pandoc.Walk
import qualified Text.Pandoc.Builder as B

import           Data.Text (Text)
import           Data.Text.Encoding
import qualified Data.Text as T
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

pandoc :: IO ()
pandoc = (\(e :: PandocError) -> handleError (Left e)) `E.handle`  do
    opts <- parseOptions options defaultOpts
    api <- apiOpts $ opts &~ do
        _optFilters .= []
    convertWithOpts' api $ opts

runFilter :: IO ()
runFilter = toJSONFilter $ transformDoc defaultOpts

apiOpts :: Opt -> IO APIOpt
apiOpts opts = do
    pure $ defaultAPIOpts &~ do
        _apiFilterFunction .= transformDoc opts

_apiFilterFunction :: APIOpt `Lens'` (Pandoc -> IO Pandoc)
_apiFilterFunction f APIOpt{..} = (\apiFilterFunction -> APIOpt{apiFilterFunction, ..}) <$> f apiFilterFunction
{-# INLINE _apiFilterFunction #-}

_optInputFiles :: Opt `Lens'` [FilePath]
_optInputFiles f Opt{..} = (\optInputFiles -> Opt{optInputFiles, ..}) <$> f optInputFiles
{-# INLINE _optInputFiles #-}

_optFilters :: Opt `Lens'` [FilePath]
_optFilters f Opt{..} = (\optFilters -> Opt{optFilters, ..}) <$> f optFilters
{-# INLINE _optFilters #-}

type BlocksWithMeta = ([Block], Meta)

stripPandoc :: Int -> Either err Pandoc -> BlocksWithMeta
stripPandoc _ (Left _) = ([Null], mempty)
stripPandoc changeInHeaderLevel (Right (Pandoc meta blocks)) =
        ( maybe id (:) title modBlocks
        , references . bibliography $ mempty
        )
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
         references :: Meta -> Meta
         references (lookupMeta "references" -> Just ml@(MetaList reflist)) = B.setMeta "references" ml mempty
         references x = mempty
         bibliography :: Meta -> Meta
         bibliography (lookupMeta "bibliography" -> Just ref@(MetaInlines _)) = B.setMeta "bibliography" (B.toMetaValue [ref]) mempty
         bibliography (lookupMeta "bibliography" -> Just refs@(MetaList _)) = B.setMeta "bibliography" refs mempty
         bibliography x = x

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

fileContentAsBlocks :: Int -> Opt -> IO BlocksWithMeta
fileContentAsBlocks changeInHeaderLevel opts = do
  let p = toAST defaultAPIOpts opts
  stripPandoc changeInHeaderLevel . pure <$> p

getProcessableFileList :: String -> [String]
getProcessableFileList list = do
  let f = lines list
  filter (\x -> not $ "#" `isPrefixOf` x) f

simpleInclude :: Opt -> Int -> String -> [String] -> IO BlocksWithMeta
simpleInclude opts changeInHeaderLevel list classes = do
  let o f = opts &~ do
          _optInputFiles .= [f]
  let toProcess = o <$> getProcessableFileList list
  (concat `bimap` mconcat) . unzip <$> fileContentAsBlocks changeInHeaderLevel `mapM` toProcess

includeCodeBlock :: Block -> IO BlocksWithMeta
includeCodeBlock (CodeBlock (_, classes, _) list) = do
  let filePath = head $ lines list
  let content = fileContentAsText filePath
  let newclasses = filter (\x -> "include" `isPrefixOf` x || "code" `isPrefixOf` x) classes
  let blocks = fmap (B.codeBlockWith ("", newclasses, []) . T.unpack) content
  (,mempty) . B.toList <$> blocks

cropContent :: [Text] -> (String, String) -> [Text]
cropContent lines (skip, count) =
  if not $ null skip then
    if not $ null count then
      take (read count) (drop (read skip) lines)
    else
      drop (read skip) lines
  else
    lines

includeCropped :: Opt -> Block -> IO BlocksWithMeta
includeCropped opts (CodeBlock (_, classes, _) list) = do
  let [filePath, skip, count] = lines list
  let content = fileContentAsText filePath
  let croppedContent = T.unlines <$> ((cropContent . T.lines <$> content) <*> pure (skip, count))
  takeFileName filePath `withSystemTempFile` \temp htemp -> do
    let o = opts &~ do
            _optInputFiles .= [temp]
    BS.hPut htemp . encodeUtf8 =<< croppedContent
    stripPandoc 0 . pure <$> toAST defaultAPIOpts o

doInclude :: Opt -> Block -> IO BlocksWithMeta
doInclude opts cb@(CodeBlock (_, classes, options) list)
  | "include" `elem` classes = do
    let changeInHeaderLevel = fromMaybe 0 $ readMay =<< "header-change" `lookup` options
    simpleInclude opts changeInHeaderLevel list classes
  | "include-indented" `elem` classes = do
    let newClasses = ("include" :) . delete "include-indented" $ classes
    let newOptions = ("header-change","1") : options
    doInclude opts $ CodeBlock ("", newClasses, newOptions) list
  | "code" `elem` classes = includeCodeBlock cb
  | "cropped" `elem` classes = includeCropped opts cb
doInclude _ x = return ([x], mempty)

transformDoc :: Opt -> Pandoc -> IO Pandoc
transformDoc opts (Pandoc meta bls) = do
        (blocks, m) <- unzip <$> doInclude opts `mapM` bls
        pure $ Pandoc (meta <> mconcat m) (concat blocks)
