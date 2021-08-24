module IR
    ( IRCtx

    , DSIdx
    , VIdx
    , upcast_dsidx
    , upcast_vidx

    , DeclSymbol
    , Module
    , Type(..)

    , Value
    , ConstFunctionPointer

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

import IR.IRCtx
