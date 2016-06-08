module Data.Iso where

import Prelude

import Data.Bifunctor as B
import Data.Either (Either(..), either)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor as P
import Data.Tuple (Tuple(..), swap, fst, snd, curry, uncurry)

-- | An isomorphism between types `a` and `b` consists of a pair of functions
-- | `f :: a -> b` and `g :: b -> a`, satisfying the following laws:
-- |
-- | - `f <<< g = id`
-- | - `g <<< f = id`
-- |
-- | The isomorphisms in this library satisfy these laws by construction.
data Iso a b = Iso (a -> b) (b -> a)

infix 4 type Iso as ≅

-- | Run an isomorphism "forwards".
forwards :: forall a b. a ≅ b -> a -> b
forwards (Iso f _) = f

-- | Run an isomorphism "backwards".
backwards :: forall a b. a ≅ b -> b -> a
backwards (Iso _ f) = f

-- | Isomorphism is symmetric
sym :: forall a b. a ≅ b -> b ≅ a
sym (Iso f g) = Iso g f

-- | $1 \times a = a$
prodIdent :: forall a. Tuple Unit a ≅ a
prodIdent = Iso snd (Tuple unit)

-- | $a \times (b \times c) = (a \times b) \times c$
prodAssoc :: forall a b c. Tuple a (Tuple b c) ≅ Tuple (Tuple a b) c
prodAssoc = Iso to from
  where to   (Tuple a (Tuple b c)) = Tuple (Tuple a b) c
        from (Tuple (Tuple a b) c) = Tuple a (Tuple b c)

-- | $a \times b = b \times a$
prodComm :: forall a b. Tuple a b ≅ Tuple b a
prodComm = Iso swap swap

-- | $0 \times a = 0$
prodZeroZero :: forall a. Tuple Void a ≅ Void
prodZeroZero = Iso fst absurd

-- | $0 + a = a$
coprodIdent :: forall a. Either Void a ≅ a
coprodIdent = Iso (either absurd id) Right

-- | $a + (b + c) = (a + b) + c$
coprodAssoc :: forall a b c. Either a (Either b c) ≅ Either (Either a b) c
coprodAssoc = Iso to from
  where to   (Left a)          = Left (Left a)
        to   (Right (Left b))  = Left (Right b)
        to   (Right (Right c)) = Right c
        from (Left (Left a))   = Left a
        from (Left (Right b))  = Right (Left b)
        from (Right c)         = Right (Right c)

-- | $a + b = b + a$
coprodComm :: forall a b. Either a b ≅ Either b a
coprodComm = Iso (either Right Left) (either Right Left)

-- | $a \times (b + c) = (a \times b) + (a \times c)$
distribute :: forall a b c. Tuple a (Either b c) ≅ Either (Tuple a b) (Tuple a c)
distribute = Iso to from
  where to (Tuple a e) = B.bimap (Tuple a) (Tuple a) e
        from (Left  t) = map Left t
        from (Right t) = map Right t

-- | $1 + a = `Maybe` a$
onePlusMaybe :: forall a. Either Unit a ≅ Maybe a
onePlusMaybe = Iso (either (const Nothing) Just) (maybe (Left unit) Right)

-- | $1 + 1 = 2$
onePlusOneIsTwo :: Either Unit Unit ≅ Boolean
onePlusOneIsTwo = Iso to from
  where to = either (const false) (const true)
        from b = if b then Right unit else Left unit

-- | $a^b \times a^c = a^{b + c}$
expProdSum :: forall a b c. Tuple (b -> a) (c -> a) ≅ (Either b c -> a)
expProdSum = Iso to from
  where to = uncurry either
        from f = Tuple (f <<< Left) (f <<< Right)

-- | $c^{a \times b} = (c^{b})^{a}$
expExpProd :: forall a b c. (Tuple a b -> c) ≅ (a -> b -> c)
expExpProd = Iso curry uncurry

-- | $a^1 = a$
expOne :: forall a. (Unit -> a) ≅ a
expOne = Iso (_ $ unit) const

-- | $a^0 = 1$
expZero :: forall a. (Void -> a) ≅ Unit
expZero = Iso (const unit) (const absurd)

-- | $a = b \implies f(a) = f(b)$
functorCong :: forall a b f. Functor f => a ≅ b -> f a ≅ f b
functorCong (Iso f g) = Iso (map f) (map g)

-- | $a = a\prime \implies f(a, b) = f(a\prime, b)$
bifunctorCongLeft :: forall a a' b f. B.Bifunctor f => a ≅ a' -> f a b ≅ f a' b
bifunctorCongLeft (Iso f g) = Iso (B.lmap f) (B.lmap g)

-- | $a = b \implies f(a) = f(b)$
contraCong :: forall a b f. Contravariant f => a ≅ b -> f a ≅ f b
contraCong (Iso f g) = Iso (cmap g) (cmap f)

-- | $a = a\prime \implies f(a, b) = f(a\prime, b)$
profunctorCongLeft :: forall a a' b f. P.Profunctor f => a ≅ a' -> f a b ≅ f a' b
profunctorCongLeft (Iso f g) = Iso (P.lmap g) (P.lmap f)

-- | $b = c \wedge a = b \implies a = c$
instance semigroupoidIso :: Semigroupoid Iso where
  compose (Iso f1 g1) (Iso f2 g2) = Iso (f1 <<< f2) (g1 >>> g2)

-- | $a = a$
instance categoryIso :: Category Iso where
  id = Iso id id
