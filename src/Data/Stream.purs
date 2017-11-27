module Data.Stream where

import Prelude
import Control.Lazy as Z
import Control.Monad.Rec.Class as MR
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Data.Distributive (class Distributive, distribute, collectDefault)
import Data.Foldable as F
import Data.Lazy (defer)
import Data.List.Lazy as L
import Data.List.Lazy.NonEmpty as NL
import Data.List.Lazy.Types (List(..), NonEmptyList(..), Step(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (class Unfoldable)

newtype Stream a = Stream (Unit -> Tuple a (Stream a))

instance showStream :: Show a => Show (Stream a) where
  show _ = "<Stream>" -- strings are strict in purescript :)

instance lazyStream :: Z.Lazy (Stream a) where
  defer f = Stream \_ -> step (f unit)

instance semigroupStream :: Semigroup (Stream a) where
  append xs _ = xs

instance functorStream :: Functor Stream where
  map f s = Stream \_ -> map' (step s)
    where map' (Tuple x xs') = Tuple (f x) (map f xs')

instance applyStream :: Apply Stream where
  apply = zipWith ($)

instance applicativeStream :: Applicative Stream where
  pure = repeat

instance extendStream :: Extend Stream where
  extend f w = Stream \_ -> Tuple (f w) (extend f (tail w))

instance comonadStream :: Comonad Stream where
  extract = head

instance distributeStream :: Distributive Stream where
  distribute w = (extract <$> w) :> distribute (tail <$> w)
  collect = collectDefault

instance bindStream :: Bind Stream where
  bind xs f = join (f <$> xs)
    where
      join :: forall a. Stream (Stream a) -> Stream a
      join xss = Stream go
        where go _ | Tuple xs' xss' <- step xss
                   = Tuple (head xs') (join (map tail xss'))

instance monadStream :: Monad Stream

-- Basic Functions

cons :: forall a. a -> Stream a -> Stream a
cons x xs = Stream \_ -> Tuple x xs

infixr 6 cons as :>

step :: forall a. Stream a -> Tuple a (Stream a)
step (Stream f) = f unit

head :: forall a. Stream a -> a
head = fst <<< step

tail :: forall a. Stream a -> Stream a
tail = snd <<< step

inits :: forall f a. Unfoldable f => Stream a -> Stream (f a)
inits s = L.toUnfoldable <$> inits' s
  where
    inits' :: Stream a -> Stream (List a)
    inits' s' = Stream go
      where
        go _ | Tuple x xs <- step s'
             = Tuple L.nil (map (L.cons x) (inits' xs))

tails :: forall a. Stream a -> Stream (Stream a)
tails = iterate tail

-- | `intersperse y xs` creates an alternating stream of
-- elements from `xs` and `y`.
intersperse :: forall a. a -> Stream a -> Stream a
intersperse y s = Stream go
  where go _ = (\xs -> y :> intersperse y xs) <$> step s

-- | [x1,x2,...] `interleave` [y1,y2,...] == [x1,y1,x2,y2,...]
interleave :: forall a. Stream a -> Stream a -> Stream a
interleave s1 s2 = Stream go
  where go _ | Tuple x xs <- step s1
             = Tuple x (interleave s2 s1)

-- | Create a stream by iterating a function.
iterate :: forall a. (a -> a) -> a -> Stream a
iterate f x = Stream \_ -> Tuple x (iterate f (f x))

-- | Add a Foldable as a prefix to a Stream.
prefix :: forall t a. F.Foldable t => t a -> Stream a -> Stream a
prefix xs ys = F.foldr cons ys xs

-- | Create a stream by repeating an element.
repeat :: forall a. a -> Stream a
repeat x = Stream \_ -> Tuple x (repeat x)

-- | `cycle xs` returns the infinite repetition of `xs`
cycle :: forall t. F.Foldable (NonEmpty t) => NonEmpty t ~> Stream
cycle xs = F.foldr cons (Z.defer \_ -> cycle xs) xs

-- | The unfold function is similar to the unfold for lists. Note
-- | there is no base case: all streams must be infinite.
unfold :: forall a c. (c -> Tuple a c) -> c -> Stream a
unfold f c | Tuple x d <- f c = Stream \_ -> Tuple x (unfold f d)

-- | Sadly, we cannot use Data.Foldable because it is strict
foldr :: forall a b. Z.Lazy b => (a -> b -> b) -> Stream a -> b
foldr f xs = Z.defer go
  where go _ | Tuple x xs' <- step xs = f x (foldr f xs')

traverse_ :: forall m a b. MR.MonadRec m => (a -> m b) -> Stream a -> m b
traverse_ f = MR.tailRecM go
  where go xs | Tuple x xs' <- step xs = f x *> pure (MR.Loop xs')

sequence_ :: forall m t. MR.MonadRec m => Stream (m t) -> m t
sequence_ = traverse_ id

-- | Apply a function to pairs of elements at the same positions in two streams,
-- | collecting the results in a new stream.
zipWith :: forall a b c. (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f xs ys
  | Tuple x xs' <- step xs
  , Tuple y ys' <- step ys
  = Stream \_ -> Tuple (f x y) (zipWith f xs' ys')

-- | Collect pairs of elements at the same positions in two streams.
zip :: forall a b. Stream a -> Stream b -> Stream (Tuple a b)
zip = zipWith Tuple

-- | The 'unzip' function is the inverse of the 'zip' function.
unzip :: forall a b. Stream (Tuple a b) -> Tuple (Stream a) (Stream b)
unzip xs = Tuple (fst <$> xs) (snd <$> xs)

toList :: Stream ~> NonEmptyList
toList s = NonEmptyList (defer mkNonEmptyList)
  where
    mkNonEmptyList _ | Tuple x s' <- step s = x :| mkList s'
    mkList s' = List (defer mkList')
      where mkList' _ | Tuple x' s'' <- step s' = Cons x' (mkList s'')

take :: forall f. Unfoldable f => Int -> Stream ~> NonEmpty f
take i xs | Tuple x xs' <- step xs = x :| rest
  where rest = L.toUnfoldable <<< L.take (i - 1) <<< NL.toList $ toList xs'

window :: forall f a. Unfoldable f => Int -> Stream a -> Stream (NonEmpty f a)
window sz s = take sz <$> tails s
