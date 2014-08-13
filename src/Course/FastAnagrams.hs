{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams str file =
  (ncString <$>) <$>
    flip (filter . flip S.member)
         (map NoCaseString $ permutations str)
         <$> S.fromList <$> hlist . map NoCaseString . lines <$> readFile file

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Ord NoCaseString where
  compare = compare `on` map toLower . ncString

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
