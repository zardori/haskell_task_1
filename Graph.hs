module Graph where
import Set(Set)
import qualified Set as Set


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
            (Set.union 
                (Set.union (relation g1) (relation g2))
                (Set.cartesianProd (domain g1) (domain g2))
            )

                
instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+) = union
    (*) = connect
    signum = const empty
    abs = id
    negate = id

instance Graph Basic where
    empty = Empty
    vertex = Vertex
    union = Union
    connect = Connect


instance Ord a => Eq (Basic a) where
    g1 == g2 =
        let rel_form_1 = fromBasic g1
            rel_form_2 = fromBasic g2
        in
            domain rel_form_1 == domain rel_form_2 &&
            relation rel_form_1 == relation rel_form_2

            
            

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
fromBasic Empty = empty
fromBasic (Vertex x) = vertex x
fromBasic (Union g1 g2) = union (fromBasic g1) (fromBasic g2)
fromBasic (Connect g1 g2) = connect (fromBasic g1) (fromBasic g2)





instance (Ord a, Show a) => Show (Basic a) where
    show g = 
        let rel_form = fromBasic g in
        let edges = Set.toAscList (relation rel_form)
            vertices = Set.toAscList (domain rel_form)
        in 
        let vertices_in_edges = foldl (\acc (x, y) -> x : (y : acc)) [] edges in
        let lonely_vertices = filter (\x -> notElem x vertices_in_edges) vertices in
            "edges " ++ show edges ++ " + vertices " ++ show lonely_vertices
        


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
    fmap func (Connect g1 g2) = Connect (fmap func g1) (fmap func g2)

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

