module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null, toList)
import qualified Data.List(nub, sort, group) 

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _     = False

member :: Eq a => a -> Set a -> Bool
member x Empty              = False
member x (Singleton y)      = x == y
member x (Union left right) = member x left || member x right

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a
fromList []     = empty
fromList [x]    = singleton x 
fromList (x:xs) = union (singleton x) (fromList xs)


toListPrepend :: Set a -> [a] -> [a]
toListPrepend Empty xs              = xs
toListPrepend (Singleton x) xs      = x:xs
toListPrepend (Union left right) xs = toListPrepend left (toListPrepend right xs)

toList :: Set a -> [a]
toList x = toListPrepend x []

toAscList :: Ord a => Set a -> [a]
toAscList r = map head (Data.List.group (Data.List.sort (toList r) ) )

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union = Union

insert :: a -> Set a -> Set a
insert x Empty              = singleton x
insert x (Singleton y)      = union (singleton x) (singleton y)
insert x (Union left right) = union (insert x left) right

instance Ord a => Eq (Set a) where
    (==) x y = toAscList x == toAscList y

instance Semigroup (Set a) where
    (<>) = union

instance Monoid (Set a) where
    mempty = empty

instance Show a => Show (Set a) where
    show x = show (toList x)

instance Functor Set where
    fmap f x = fromList (fmap f (toList x))