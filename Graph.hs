module Graph where
import Set(Set)
import qualified Set as Set
import qualified Data.List(nub, sort)
import Data.List( (\\) ) 

class Graph g where
  empty     :: g a
  vertex    :: a -> g a
  union     :: g a -> g a -> g a
  connect   :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)

instance Graph Relation where
  empty    = Relation { domain = Set.empty, relation = Set.empty }
  vertex v = Relation { domain = Set.singleton v, relation = Set.empty }
  union Relation { domain = fd, relation = fr } Relation { domain = sd, relation = sr} = 
    Relation { domain = Set.union fd sd, relation = Set.union fr sr }
  connect Relation { domain = fd, relation = fr } Relation { domain = sd, relation = sr} =
    Relation { 
      domain = Set.union fd sd, 
      relation = 
        Set.union 
          (Set.union fr sr) 
          (Set.fromList ([(x, y) | x <- Set.toList fd, y <- Set.toList sd]))
    }
                
instance (Ord a, Num a) => Num (Relation a) where
  fromInteger = vertex . fromInteger
  (+)         = union
  (*)         = connect
  signum      = const empty
  abs         = id
  negate      = id

instance Graph Basic where
  empty   = Empty
  vertex  = Vertex 
  union   = Union
  connect = Connect

toSetV :: Ord a => Basic a -> Set a
toSetV Empty         = Set.empty
toSetV (Vertex v)    = Set.singleton v
toSetV (Union x y)   = Set.union (toSetV x) (toSetV y)
toSetV (Connect x y) = Set.union (toSetV x) (toSetV y)

toSetE :: Ord a => Basic a -> Set (a, a)
toSetE Empty         = Set.empty
toSetE (Vertex v)    = Set.empty
toSetE (Union x y)   = Set.union (toSetE x) (toSetE y)
toSetE (Connect x y) = 
  Set.union 
    (Set.union (toSetE x) (toSetE y)) 
    (Set.fromList [(p, q) | p <- Set.toList (toSetV x), q <- Set.toList (toSetV y)])

instance Ord a => Eq (Basic a) where
  (==) x y = (toSetV x == toSetV y) && (toSetE x == toSetE y)

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
fromBasic Empty         = empty
fromBasic (Vertex v)    = vertex v
fromBasic (Union x y)   = union (fromBasic x) (fromBasic y)
fromBasic (Connect x y) = connect (fromBasic x) (fromBasic y)

basicToPrint :: (Ord a, Show a) => Basic a -> ([(a, a)], [a])
basicToPrint b = 
  let eList          = Set.toAscList (toSetE b)
      eListConnected = [p | (p, _) <- eList] ++ [q | (_, q) <- eList]
      vList          = Set.toAscList (toSetV b)
      vListShortened = Data.List.nub vList \\ Data.List.nub eListConnected
  in (eList, vListShortened)

instance (Ord a, Show a) => Show (Basic a) where
  show b = 
    let (eList, vList) = basicToPrint b
    in "edges " ++ show eList ++ " + vertices " ++ show vList

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot b = 
  let (eList, vList) = basicToPrint b
  in "digraph {\n" ++ 
    foldl (\x y -> x ++ show (fst y) ++ " -> " ++ show (snd y) ++ ";\n") "" eList ++
    foldl (\x y -> x ++ show y ++ ";\n") "" vList ++ "}\n"

instance Functor Basic where
  fmap f Empty         = empty
  fmap f (Vertex v)    = vertex (f v)
  fmap f (Union x y)   = union (fmap f x) (fmap f y)
  fmap f (Connect x y) = connect (fmap f x) (fmap f y)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV x y z = 
  fmap (\v -> case () of 
              _  | v == x -> z
                 | v == y -> z
                 | otherwise -> v)

instance Applicative Basic where
  pure = vertex
  (<*>) Empty t         = empty
  (<*>) (Vertex v) t    = fmap v t
  (<*>) (Union x y) t   = union (x <*> t) (y <*> t)
  (<*>) (Connect x y) t = connect (x <*> t) (y <*> t)

instance Monad Basic where
  (>>=) Empty f         = Empty
  (>>=) (Vertex v) f    = f v
  (>>=) (Union x y) f   = union (x >>= f) (y >>= f)
  (>>=) (Connect x y) f = connect (x >>= f) (y >>= f)

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV x y z b = 
  union (vertex (\v -> if v == x then y else v)) (vertex (\v -> if v == x then z else v)) <*> b

