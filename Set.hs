module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null, toList)
import qualified Data.List(sort) 

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _     = False

member :: Eq a => a -> Set a -> Bool
member x Empty = False
member x (Singleton y) = x == y
member x (Union left right) = member x left || member x right

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a
fromList (x:xs) = union (singleton x) (fromList xs)

toList :: Set a -> [a]
toList Empty = []
toList (Singleton x) = [x]
toList (Union left right) = toList left ++ toList right

toAscList :: Ord a => Set a -> [a]
toAscList s = Data.List.sort (toList s)

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union = Union

insert :: a -> Set a -> Set a
insert x Empty = singleton x
insert x (Singleton y) = union (singleton x) (singleton y)
insert x (Union left right) = union (insert x left) right

instance Ord a => Eq (Set a) where
    (==) x y = toAscList x == toAscList y

instance Semigroup (Set a) where
-- todo

instance Monoid (Set a) where
-- todo

instance Show a => Show (Set a) where
-- todo

instance Functor Set where
-- todo