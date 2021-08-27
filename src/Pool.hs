module Pool
    ( Pool
    , PoolIdx
    , new_pool
    , new_pool_with
    , pool_items
    , pool_idxs
    , add_to_pool
    , get_from_pool
    , search_in_pool
    , replace_in_pool

    , tests
    ) where

import Test

import Data.List (elemIndex)

newtype Pool a = Pool [a]
newtype PoolIdx a = PoolIdx Int deriving (Eq, Ord)

instance Show (PoolIdx a) where
    show (PoolIdx i) = show i

new_pool :: Pool a
new_pool = Pool []

new_pool_with :: [a] -> Pool a
new_pool_with = Pool

pool_items :: Pool a -> [a]
pool_items (Pool items) = items

pool_idxs :: Pool a -> [PoolIdx a]
pool_idxs (Pool items) = take (length items) (PoolIdx <$> [0..])

add_to_pool :: a -> Pool a -> (PoolIdx a, Pool a)
add_to_pool item (Pool items) = (PoolIdx $ length items, Pool $ items ++ [item])

get_from_pool :: PoolIdx a -> Pool a -> a
get_from_pool (PoolIdx idx) (Pool items) = items !! idx

search_in_pool :: Eq a => a -> Pool a -> Maybe (PoolIdx a)
search_in_pool item (Pool items) = PoolIdx <$> elemIndex item items

replace_in_pool :: a -> PoolIdx a -> Pool a -> Pool a
replace_in_pool item (PoolIdx idx) (Pool items) =
    let (keep, _:keep2) = splitAt idx items
        items' = keep ++ item : keep2
    in Pool items'

tests :: Test
tests = DescribeModule "Pool"
    [ DescribeFunction "new_pool"
        [ ItCan "create an empty pool" $
            case new_pool of
                Pool [] -> pass_test
                _ -> fail_test
        ]

    , DescribeFunction "new_pool_with"
        [ ItCan "create a new pool with the given elements" $
            case new_pool_with [1 :: Int, 2, 3, 1] of
                Pool [1, 2, 3, 1] -> pass_test
                _ -> fail_test
        ]

    , DescribeFunction "pool_items"
        [ ItCan "get all the items in a pool" $
            case pool_items $ new_pool_with [1 :: Int, 2, 3, 1] of
                [1, 2, 3, 1] -> pass_test
                _ -> fail_test
        ]

    , DescribeFunction "pool_idxs"
        [ ItCan "get all the indexes in a pool" $
            case pool_idxs $ new_pool_with [1 :: Int, 2, 3, 1] of
                [PoolIdx 0, PoolIdx 1, PoolIdx 2, PoolIdx 3] -> pass_test
                _ -> fail_test
        ]

    , DescribeFunction "add_to_pool"
        [ ItCan "add an item to an empty pool" $
            let pool = new_pool
                res = add_to_pool (1 :: Int) pool
            in case res of
                (PoolIdx 0, Pool [1]) -> pass_test
                _ -> fail_test

        , ItCan "add an item to a non-empty pool" $
            let pool = new_pool_with [1, 2, 3]
                res = add_to_pool (5 :: Int) pool
            in case res of
                (PoolIdx 3, Pool [1, 2, 3, 5]) -> pass_test
                _ -> fail_test
        ]

    , DescribeFunction "get_from_pool" $
        [ ItCan "return the item at an index" $
            let pool = new_pool_with [1 :: Int, 2, 3]
                idx = PoolIdx 1
            in pass_if $ get_from_pool idx pool == 2
        ]

    , DescribeFunction "replace_in_pool"
        [ ItCan "replace an item at an index" $
            let pool = new_pool_with [1 :: Int, 2, 3]
                idx = PoolIdx 2
            in case replace_in_pool 2 idx pool of
                Pool [1, 2, 2] -> pass_test
                _ -> fail_test
        ]

    ]
