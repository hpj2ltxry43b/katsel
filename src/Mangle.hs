module Mangle
    ( MangledName
    , mangled_str

    , mangle_dsidx
    , mangle_vidx
    , mangle_fun

    , Mangle.tests
    ) where

import Test

import qualified IR

import Interner

import Mangle.TagName
import Mangle.Tag

data MangledName = MangledName { mangled_str :: String }

to_mn :: Tag -> MangledName
to_mn = MangledName . str_tag

mangle_dsidx :: IR.DSIdx d -> MangledName
mangle_dsidx dsidx = to_mn $ Tag tn'dsidx [StrTag tn'idx (show $ IR.upcast_dsidx dsidx)]

mangle_vidx :: IR.VIdx v -> MangledName
mangle_vidx vidx = to_mn $ Tag tn'vidx [StrTag tn'idx (show $ IR.upcast_vidx vidx)]

mangle_fun :: InternerIdx IR.Function -> MangledName
mangle_fun fidx = to_mn $ Tag tn'funidx [StrTag tn'idx (show fidx)]

-- tests {{{1
tests :: Test
tests = DescribeModule "Mangle"
    [ Mangle.Tag.tests
    ]
