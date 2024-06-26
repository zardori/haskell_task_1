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
member x (Singleton y) = x == y
member x (Union s1 s2) = member x s1 || member x s2

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a
fromList = foldl (\set x -> insert x set) Empty

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
insert x Empty         = Singleton x
insert x (Singleton y) = Union (Singleton y) (Singleton x)
insert x set = Union (Singleton x) set

instance Ord a => Eq (Set a) where
    s1 == s2 = toAscList s1 == toAscList s2

instance Semigroup (Set a) where
    s1 <> s2 = union s1 s2

instance Monoid (Set a) where
    mempty = empty

instance Show a => Show (Set a) where
    show set = show (toList set) 

instance Functor Set where
    fmap _ Empty            = Empty
    fmap func (Singleton x) = Singleton (func x)
    fmap func (Union s1 s2) = Union (fmap func s1) (fmap func s2)


-- Remove repetitions from sorted list
listWithNoRep :: Eq a => [a] -> [a]
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
cartesianProd :: Set a -> Set b -> Set (a, b)
cartesianProd s1 s2 = fromList [(x, y) | x <- toList s1, y <- toList s2]
