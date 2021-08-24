module IR.ApplyTo
    ( apply_to_ds
    , apply_to_v
    ) where

import IR.IRCtx

import IR.DeclSymbol
import IR.Module
import IR.Type

import IR.Value
import IR.ConstFunctionPointer

import Data.Typeable (Typeable, cast)

apply_to_ds :: (Module -> r) -> (Type -> r) -> DeclSymbol' -> r
apply_to_ds mod_f ty_f (DeclSymbol ds) =
    case identify_ds ds of
        DSTModule -> mod_f $ force_cast' ds
        DSTType -> ty_f $ force_cast' ds
    where
        force_cast' :: (Typeable a, Typeable b) => a -> b
        force_cast' = force_cast "DeclSymbol does not return the correct value of DeclSymbolType"

apply_to_v :: (ConstFunctionPointer -> r) -> Value' -> r
apply_to_v cfptr_f (Value v) =
    case identify_v v of
        VTConstFunctionPointer -> cfptr_f $ force_cast' v
    where
        force_cast' :: (Typeable a, Typeable b) => a -> b
        force_cast' = force_cast "Value does not return the correct value of ValueType"

force_cast :: (Typeable a, Typeable b) => String -> a -> b
force_cast msg a =
    case cast a of
        Just b -> b
        Nothing -> error msg
