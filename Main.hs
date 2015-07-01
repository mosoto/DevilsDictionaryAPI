{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T (unpack, pack, strip, drop, length) 
import qualified Data.Map as M (Map, empty, insert, keys, lookup)
import Text.Regex
import Data.Char (isAlpha)
import Data.List (dropWhileEnd)
import Data.Maybe (Maybe, isJust, fromJust)
import Debug.Trace (trace)
import Web.Spock
import qualified Network.HTTP.Types.Status as HttpStatus



main :: IO ()
main = do
    dictionaryContent <- readFile "dictionary.txt"
    let dictionaryMap = parseDictionary dictionaryContent
    runSpock 5000 $ spockT id $ do
        get root $ do
          setStatus HttpStatus.ok200
          json dictionaryMap
        get "keys" $ do
          setStatus HttpStatus.ok200
          json . M.keys $ dictionaryMap
        get (var) $ \key -> do
          setStatus HttpStatus.ok200
          json . M.lookup key $ dictionaryMap


parseDictionary :: String -> M.Map Text Text
parseDictionary c = toMap . groupLines $ dictionaryLines
    where   dictionaryLines :: [String]
            dictionaryLines = dropWhile (== "") .
                              filter (\l -> not (length l == 1 && (isAlpha . Prelude.head $ l))) .
                              takeWhile (/= "End of Project Gutenberg's The Devil's Dictionary, by Ambrose Bierce") .
                              dropWhile (/= "A") . 
                              lines $ c

            groupLines :: [String] -> [[String]]
            groupLines [] = []
            groupLines (x:xs) = (x : fst parts) : (groupLines . snd $ parts)
              where parts = break (isJust . matchEntryStart) xs

            toMap :: [[String]] -> M.Map Text Text
            toMap = foldl (\m v -> M.insert (key v) (value v) m) M.empty
              where key (x:xs) = T.pack . fromJust . matchEntryStart $ x
                    value x = T.strip . T.drop ((+1) . T.length . key $ x) . T.pack . unlines $ x


matchEntryStart :: String -> Maybe String
matchEntryStart l = case matchRegex entryStartRegex l of
                      Just (x:xs) -> Just x
                      Nothing -> Nothing
    where entryStartRegex = mkRegex "(^[A-Z]+),"

