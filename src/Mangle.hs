module Mangle
    ( MangledName
    , mangled_str

    , mangle_dsidx
    , mangle_ds_interner_idx
    , mangle_vidx
    , mangle_v_interner_idx
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

mangle_ds_interner_idx :: InternerIdx IR.DeclSymbol' -> MangledName
mangle_ds_interner_idx idx = to_mn $ Tag tn'dsidx [StrTag tn'idx (show idx)] -- TODO: do this better

mangle_dsidx :: IR.DSIdx d -> MangledName
mangle_dsidx = mangle_ds_interner_idx . IR.upcast_dsidx

mangle_v_interner_idx :: InternerIdx IR.Value' -> MangledName
mangle_v_interner_idx idx = to_mn $ Tag tn'vidx [StrTag tn'idx (show idx)] -- TODO: do this better also

mangle_vidx :: IR.VIdx v -> MangledName
mangle_vidx = mangle_v_interner_idx . IR.upcast_vidx

mangle_fun :: InternerIdx IR.Function -> MangledName
mangle_fun fidx = to_mn $ Tag tn'funidx [StrTag tn'idx (show fidx)]

-- tests {{{1
tests :: Test
tests = DescribeModule "Mangle"
    [ Mangle.Tag.tests
    ]
