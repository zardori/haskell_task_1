module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems, cartesianProd
              ) where
import Prelude hiding(null)
import Data.List (sort)


data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _     = False

member :: Eq a => a -> Set a -> Bool
member _ Empty = False
member elem (Singleton x) = elem == x
member elem (Union s1 s2) = member elem s1 || member elem s2

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a
fromList = foldl (\set elem -> insert elem set) Empty

toListAcc :: Set a -> [a] -> [a]
toListAcc Empty acc = acc
toListAcc (Singleton x) acc = x : acc
toListAcc (Union s1 s2) acc = toListAcc s1 (toListAcc s2 acc)

toList :: Set a -> [a]
toList s = toListAcc s []

toAscList :: Ord a => Set a -> [a]
toAscList set = listWithNoRep (sort (toList set))

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union = Union

insert :: a -> Set a -> Set a
insert elem Empty         = Singleton elem
insert elem (Singleton x) = Union (Singleton x) (Singleton elem)
insert elem set = Union (Singleton elem) set

instance Ord a => Eq (Set a) where
    s1 == s2 = toAscList s1 == toAscList s2

instance Semigroup (Set a) where
    s1 <> s2 = union s1 s2

instance Monoid (Set a) where
    mempty = empty

instance Show a => Show (Set a) where
    show set = show (toList set) 

instance Functor Set where
    fmap func Empty         = Empty
    fmap func (Singleton x) = Singleton (func x)
    fmap func (Union s1 s2) = Union (fmap func s1) (fmap func s2)



-- Remove repetitions from sorted list
listWithNoRep (h:t) =
    reverse
        (fst 
            (foldl
                (\(result_list, last_elem) curr_elem -> 
                    if last_elem /= curr_elem
                    then (curr_elem : result_list, curr_elem)
                    else (result_list, curr_elem)
                )
                ([h], h)
                t
            )
        )
listWithNoRep _ = []


-- Helper function to use in graph module
cartesianProd s1 s2 = fromList [(x, y) | x <- toList s1, y <- toList s2]


-- listWithNoRep2 (h:t) =
--     fst 
--         (foldr
--             (\curr_elem (result_list, last_elem)-> 
--                 if last_elem /= curr_elem
--                 then (curr_elem : result_list, curr_elem)
--                 else (result_list, curr_elem)
--             )
--             ([], h)
--             t
--         )
-- listWithNoRep2 _ = []


-- mergeWithNoRep [] l = l
-- mergeWithNoRep l [] = l
-- mergeWithNoRep (h1:t1) (h2:t2) = []



-- sortWithNoRep input_list = input_list