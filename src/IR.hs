module IR
    ( IRCtx

    , ds_interner
    , v_interner

    , DSIdx
    , VIdx
    , upcast_dsidx
    , upcast_vidx
    , resolve_dsidx
    , resolve_vidx
    , apply_to_ds
    , apply_to_v

    , DeclSymbol
    , Module
    , Type(..)

    , Signedness(..)

    , Value
    , ConstFunctionPointer

    , type_of

    , Function
    ) where

import IR.ChildList

import IR.DeclSpan
import IR.Typed

import IR.DeclSymbol
import IR.Module
import IR.Type

import IR.Value
import IR.ConstFunctionPointer

import IR.Function

import IR.ApplyTo

import IR.IRCtx
