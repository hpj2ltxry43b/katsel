{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module IR.IRCtx
    ( IRCtx

    , IsDeclSymbol'
    , DeclSymbol'
    , IsValue'
    , Value'

    , ds_pool
    , v_pool
    , ds_child_list
    , v_child_list
    , function_pool

    , DSIdx
    , upcast_dsidx
    , downcast_dsidx
    , add_ds
    , search_ds
    , resolve_dsidx

    , VIdx
    , upcast_vidx
    , downcast_vidx
    , add_v
    , search_v
    , resolve_vidx
    ) where

import SimpleLens
import StateAndLens

import Pool

import IR.ChildList

import IR.DeclSymbol
import IR.Value

import {-# SOURCE #-} IR.Type
import {-# SOURCE #-} IR.Function

import qualified Control.Monad.State as State (State, state)
import qualified Control.Monad.Reader as Reader (Reader, reader)

type IsDeclSymbol' = IsDeclSymbol IRCtx
type DeclSymbol' = DeclSymbol IRCtx
type IsValue' = IsValue IRCtx (DSIdx Type)
type Value' = Value IRCtx (DSIdx Type)

data IRCtx
    = IRCtx
      { _ds_pool :: Pool DeclSymbol'
      , _v_pool :: Pool Value'
      , _ds_child_list :: ChildList (PoolIdx DeclSymbol') (PoolIdx DeclSymbol') String
      , _v_child_list :: ChildList (PoolIdx DeclSymbol') (PoolIdx Value') String
      , _function_pool :: Pool Function
      }
ds_pool :: Lens IRCtx (Pool DeclSymbol')
ds_pool = Lens _ds_pool (\ a b -> a { _ds_pool = b })

v_pool :: Lens IRCtx (Pool Value')
v_pool = Lens _v_pool (\ a b -> a { _v_pool = b })

ds_child_list :: Lens IRCtx (ChildList (PoolIdx DeclSymbol') (PoolIdx DeclSymbol') String)
ds_child_list = Lens _ds_child_list (\ a b -> a { _ds_child_list = b })

v_child_list :: Lens IRCtx (ChildList (PoolIdx DeclSymbol') (PoolIdx Value') String)
v_child_list = Lens _v_child_list (\ a b -> a { _v_child_list = b })

function_pool :: Lens IRCtx (Pool Function)
function_pool = Lens _function_pool (\ a b -> a { _function_pool = b })

newtype DSIdx d = DSIdx { upcast_dsidx :: PoolIdx DeclSymbol' } deriving (Eq, Ord)
instance Show (DSIdx d) where
    show = show . upcast_dsidx

downcast_dsidx :: IsDeclSymbol' d => PoolIdx DeclSymbol' -> Reader.Reader IRCtx (Maybe (DSIdx d))
downcast_dsidx idx = Reader.reader $ \ irctx ->
    let ds = get_from_pool idx (view ds_pool irctx)

        into_dsidx :: Maybe d -> Maybe (DSIdx d)
        into_dsidx d = DSIdx idx <$ d

    in into_dsidx (ds_cast ds)

add_ds :: IsDeclSymbol' d => d -> State.State IRCtx (DSIdx d)
add_ds d = State.state $ \ irctx ->
    let (iidx, irctx') = modify ds_pool (add_to_pool (DeclSymbol d)) irctx
    in (DSIdx iidx, irctx')

search_ds :: IsDeclSymbol' d => d -> State.State IRCtx (DSIdx d)
search_ds d =
    search_in_pool (DeclSymbol d) <$> (view_s ds_pool) >>= \case
        Just dsidx -> return (DSIdx dsidx)
        Nothing -> add_ds d

resolve_dsidx :: IsDeclSymbol' d => DSIdx d -> Reader.Reader IRCtx d
resolve_dsidx (DSIdx iidx) = Reader.reader $ \ irctx ->
    case ds_cast $ get_from_pool iidx $ view ds_pool irctx of
        Just d -> d
        Nothing -> error "DSIdx does not have correct type"


newtype VIdx v = VIdx { upcast_vidx :: PoolIdx Value' } deriving (Eq, Ord)
instance Show (VIdx d) where
    show = show . upcast_vidx

downcast_vidx :: IsValue' v => PoolIdx Value' -> Reader.Reader IRCtx (Maybe (VIdx v))
downcast_vidx idx = Reader.reader $ \ irctx ->
    let v = get_from_pool idx (view v_pool irctx)
        
        into_vidx :: Maybe v -> Maybe (VIdx v)
        into_vidx v' = VIdx idx <$ v'

    in into_vidx (v_cast v)

add_v :: IsValue' v => v -> State.State IRCtx (VIdx v)
add_v v = State.state $ \ irctx ->
    let (iidx, irctx') = modify v_pool (add_to_pool (Value v)) irctx
    in (VIdx iidx, irctx')

search_v :: IsValue' v => v -> State.State IRCtx (VIdx v)
search_v v =
    search_in_pool (Value v) <$> (view_s v_pool) >>= \case
        Just vidx -> return (VIdx vidx)
        Nothing -> add_v v

resolve_vidx :: IsValue' v => VIdx v -> Reader.Reader IRCtx v
resolve_vidx (VIdx iidx) = Reader.reader $ \ irctx ->
    case v_cast $ get_from_pool iidx $ view v_pool irctx of
        Just v -> v
        Nothing -> error "VIdx does not have correct type"
