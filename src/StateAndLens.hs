module StateAndLens
    ( view_s
    , over_s
    , modify_s
    , modify_s'
    ) where

import SimpleLens

import qualified Control.Monad.State as State (State, state, runState)

view_s :: Lens a b -> State.State a b
view_s lens = State.state $ \ a -> (view lens a, a)

over_s :: Lens a b -> (b -> b) -> State.State a ()
over_s lens f = State.state $ \ a -> ((), over lens f a)

modify_s :: Lens a b -> (b -> (c, b)) -> State.State a c
modify_s lens f = State.state $ modify lens f

modify_s' :: Lens a b -> State.State b c -> State.State a c
modify_s' lens s = State.state $ modify lens (State.runState s)
