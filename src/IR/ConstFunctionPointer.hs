{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.ConstFunctionPointer
    ( ConstFunctionPointer
    , new_function_pointer
    , get_fptr_pointee
    , get_function_idx
    ) where

import StateAndLens

import IR.IRCtx
import IR.Type
import IR.Function

import IR.DeclSpan
import IR.Typed
import IR.Value

import Pool

import Data.Function (on)

import qualified Control.Monad.Reader as Reader (Reader)
import qualified Control.Monad.State as State (State)

data ConstFunctionPointer = ConstFunctionPointer { get_ty :: DSIdx Type, get_function_idx :: PoolIdx Function }

new_function_pointer :: PoolIdx Function -> State.State IRCtx ConstFunctionPointer
new_function_pointer fun_idx =
    get_from_pool fun_idx <$> view_s function_pool >>= \ fun ->
    search_ds (FunctionPointerType (get_ret_type fun) (get_param_types fun)) >>= \ fptr_ty ->
    return (ConstFunctionPointer fptr_ty fun_idx)

get_fptr_pointee :: ConstFunctionPointer -> Reader.Reader IRCtx Function
get_fptr_pointee (ConstFunctionPointer _ fidx) = get_from_pool fidx <$> view_r function_pool

instance DeclSpan IRCtx ConstFunctionPointer where
    decl_span fptr = get_fptr_pointee fptr >>= decl_span
instance Typed IRCtx (DSIdx Type) ConstFunctionPointer where
    type_of = return . get_ty

instance Eq ConstFunctionPointer where
    (==) = (==) `on` get_function_idx

instance IdentifyV ConstFunctionPointer where
    identify_v _ = VTConstFunctionPointer

instance IsValue IRCtx (DSIdx Type) ConstFunctionPointer
