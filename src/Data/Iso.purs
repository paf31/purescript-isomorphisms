module Data.Iso 
  ( Iso(..)
  ) where

-- | An isomorphism between types `a` and `b` consists of a pair of functions 
-- | `f :: a -> b` and `g :: b -> a`, satisfying the following laws:
-- |
-- | - `f <<< g = id`
-- | - `g <<< f = id`
-- |
-- | The isomorphisms in this library satisfy these laws by construction.
data Iso a b = Iso (a -> b) (b -> a)

-- | Run an isomorphism "forwards".
forwards :: forall a b. Iso a b -> a -> b
forwards (Iso f _) = f

-- | Run an isomorphism "backwards".
backwards :: forall a b. Iso a b -> b -> a
backwards (Iso _ f) = f

-- | Isomorphism is symmetric
sym :: forall a b. Iso a b -> Iso b a
sym (Iso f g) = Iso g f

instance semigroupoidIso :: Semigroupoid Iso where
  (<<<) (Iso f1 g1) (Iso f2 g2) = Iso (f1 <<< f2) (g1 >>> g2)

instance categoryIso :: Category Iso where
  id = Iso id id
