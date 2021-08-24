module Mangle.TagName
    ( TagName
    , TagTagName
    , StrTagName

    , str_tag_name

    , tn'dsidx
    , tn'vidx
    , tn'name
    , tn'funidx
    , tn'idx
    ) where

import qualified Data.Map as Map

data TagName k
    = TN'DSIdx
    | TN'VIdx
    | TN'Name
    | TN'Root
    | TN'Next
    | TN'FunIdx
    | TN'Idx
    deriving (Eq, Ord, Enum, Bounded)

data TagTagName
data StrTagName

tn'dsidx, tn'vidx, tn'funidx :: TagName TagTagName
tn'dsidx = TN'DSIdx
tn'vidx = TN'VIdx
tn'funidx = TN'FunIdx

tn'name, tn'idx :: TagName StrTagName
tn'name = TN'Name
tn'idx = TN'Idx

str_tag_name :: TagName k -> String
str_tag_name = (shortened Map.!) . full_str_name
    where
        full_str_name TN'DSIdx = "dsidx"
        full_str_name TN'VIdx = "vidx"
        full_str_name TN'Name = "name"
        full_str_name TN'Root = "root"
        full_str_name TN'Next = "next"
        full_str_name TN'FunIdx = "funidx"
        full_str_name TN'Idx = "idx"

        -- longest common prefix
        lcp a b = takeWhile (uncurry (==)) (zip a b)

        all_names = enumFromTo minBound maxBound
        all_fulls = map full_str_name all_names
        shorten s = take (longest_lcp_length + 1) s
            where
                longest_lcp_length = maximum $ map (length . lcp s) (filter (s/=) all_fulls)
        shortened = Map.fromList $ map (\ n -> (n, shorten n)) all_fulls
