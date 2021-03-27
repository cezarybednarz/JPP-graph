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
  empty = Relation { domain = Set.empty, relation = Set.empty }
  vertex v = Relation { domain = Set.singleton v, relation = Set.empty }
  union Relation { domain = fd, relation = fr } Relation { domain = sd, relation = sr} = 
    Relation { domain = Set.union fd sd, relation = Set.union fr sr }
  connect Relation { domain = fd, relation = fr } Relation { domain = sd, relation = sr} =
    Relation { 
      domain = Set.union fd sd, 
      relation = Set.union (Set.union fr sr) (Set.fromList ([(x, y) | x <- Set.toList fd, y <- Set.toList sd]))
    }
                
instance (Ord a, Num a) => Num (Relation a) where
  fromInteger = vertex . fromInteger
  (+)         = union
  (*)         = connect
  signum      = const empty
  abs         = id
  negate      = id

-- instance Graph Basic where
-- -- todo B

-- instance Ord a => Eq (Basic a) where
-- -- todo B

-- instance (Ord a, Num a) => Num (Basic a) where
--     fromInteger = vertex . fromInteger
--     (+)         = union
--     (*)         = connect
--     signum      = const empty
--     abs         = id
--     negate      = id

-- instance Semigroup (Basic a) where
--   (<>) = union

-- instance Monoid (Basic a) where
--   mempty = Empty

-- fromBasic :: Graph g => Basic a -> g a
-- -- todo B

-- instance (Ord a, Show a) => Show (Basic a) where
-- -- todo B

-- -- | Example graph
-- -- >>> example34
-- -- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

-- example34 :: Basic Int
-- example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

-- todot :: (Ord a, Show a) => Basic a -> String
-- todot = undefined -- todo C

-- instance Functor Basic where
-- --- todo B

-- -- | Merge vertices
-- -- >>> mergeV 3 4 34 example34
-- -- edges [(1,2),(2,34),(34,5)] + vertices [17]

-- mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
-- mergeV = undefined -- todo B

-- instance Applicative Basic where
-- -- todo D

-- instance Monad Basic where
-- -- todo D

-- -- | Split Vertex
-- -- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- -- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

-- splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
-- splitV = undefined -- todo D

