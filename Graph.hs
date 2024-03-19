module Graph where
import qualified Set as Set
import qualified Data.Either as Set
class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)

instance Graph Relation where
    empty = Relation Set.empty Set.empty
    vertex x = Relation (Set.singleton x) Set.empty
    union g1 g2 = Relation (Set.union (domain g1) (domain g2)) 
                           (Set.union (relation g1) (relation g2))
    connect g1 g2 = 
        Relation 
            (Set.union (domain g1) (domain g2)) 
            (union 
                (union (relation g1) (relation g2))
                (cartesianProd (domain g1) (domain g2))
            )

                
instance (Ord a, Num a) => Num (Relation a) where

instance Graph Basic where
    empty = Empty
    vertex = Vertex
    union = Union
    connect = Connect


instance Ord a => Eq (Basic a) where

instance (Ord a, Num a) => Num (Basic a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Semigroup (Basic a) where
  (<>) = union

instance Monoid (Basic a) where
  mempty = Empty

fromBasic :: Graph g => Basic a -> g a
fromBasic = undefined

instance (Ord a, Show a) => Show (Basic a) where

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot = undefined

instance Functor Basic where
    fmap func Empty = Empty
    fmap func (Vertex x) = Vertex (func x)
    fmap func (Union g1 g2) = Union (fmap func g1) (fmap func g2)
    fmap func (Connect g2 g2) = Connect (fmap func g1) (fmap func g2)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV = undefined

instance Applicative Basic where

instance Monad Basic where

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV = undefined

