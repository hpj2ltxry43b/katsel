module Mangle.Tag
    ( Tag(..)
    , str_tag

    , tests
    ) where

import Test

import Mangle.TagName

import Numeric (showHex)
import Data.Char (isAscii, isAlpha, isDigit, ord)

data Tag = Tag (TagName TagTagName) [Tag] | StrTag (TagName StrTagName) String

str_tag :: Tag -> String
str_tag = ("_B"++) . str_tag'

str_tag' :: Tag -> String
str_tag' (Tag name children) = str_tag_name name ++ show (length children) ++ concatMap str_tag' children
str_tag' (StrTag name val) =
    let val' = encode val
    in str_tag_name name ++ show (length val') ++ val'

encode :: String -> String
encode "" = ""
encode (c:cs) = encode_ch True c ++ concatMap (encode_ch False) cs

encode_ch :: Bool -> Char -> String
encode_ch first ch
    | isAlpha ch && isAscii ch = [ch]
    | isDigit ch && isAscii ch && not first = [ch]
    | ch == '_' = "__"
    | otherwise = "_u" ++ showHex (ord ch) "_"

-- tests {{{1
tests :: Test
tests =
    let example_tag = Tag tn'dsidx [example_child]
        example_child = StrTag tn'name "asdf"
    in DescribeModule "Mangle.Tag"
        [ DescribeFunction "str_tag"
            [ ItCan "prepend '_B' to the tag" $
                let strd_no_prefix = str_tag' example_tag
                    strd = str_tag example_tag

                    (prefix, after) = splitAt 2 strd
                in pass_if $ prefix == "_B" && after == strd_no_prefix
            ]

        , DescribeFunction "str_tag'"
            [ ItCan "stringify a tag" $
                let strd = str_tag' example_tag
                    tn'dsidx_strd = str_tag_name tn'dsidx
                    child_strd = str_tag' example_child

                    should_be = tn'dsidx_strd ++ "1" ++ child_strd

                in pass_if $ strd == should_be

            , ItCan "stringify a string tag" $
                let tag = StrTag tn'name "asdf"
                in pass_if $ str_tag' tag == str_tag_name tn'name ++ "4asdf"
            ]

        , DescribeFunction "encode"
            [ ItCan "encode an empty string" $
                pass_if $ encode "" == ""
            ]

        , DescribeFunction "encode_ch"
            [ When "the character being encoded is the first character"
                [ ItCan "encode an ascii alphabetical character" $
                    pass_if $ encode_ch True 'a' == "a"

                , ItCan "encode an ascii number" $
                    pass_if $ encode_ch True '0' == "_u30_"

                , ItCan "encode an underscsore" $
                    pass_if $ encode_ch True '_' == "__"

                , ItCan "encode a non-ascii character" $
                    pass_if $ encode_ch True 'á' == "_ue1_"
                ]
            ,  When "the character being encoded is not the first character"
                [ ItCan "encode an ascii alphabetical character" $
                    pass_if $ encode_ch False 'a' == "a"

                , ItCan "encode an ascii number" $
                    pass_if $ encode_ch False '0' == "0"

                , ItCan "encode an underscsore" $
                    pass_if $ encode_ch False '_' == "__"

                , ItCan "encode a non-ascii character" $
                    pass_if $ encode_ch False 'á' == "_ue1_"
                ]
            ]
        ]
