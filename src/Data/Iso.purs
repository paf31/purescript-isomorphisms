module Data.Iso where

import Prelude

import Data.Profunctor.Strong ((***), first, second)
import Data.Profunctor.Choice (left, right)
import qualified Data.Bifunctor as B
import qualified Data.Profunctor as P
import Data.Functor.Contravariant (Contravariant, cmap)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..), swap, fst, snd, curry, uncurry)
import Data.Void (Void(), absurd)

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

-- | $1 \times a = a$
prodIdent :: forall a. Iso (Tuple Unit a) a
prodIdent = Iso snd (Tuple unit)

-- | $a \times (b \times c) = (a \times b) \times c$ 
prodAssoc :: forall a b c. Iso (Tuple a (Tuple b c)) (Tuple (Tuple a b) c)
prodAssoc = Iso to from
  where to   (Tuple a (Tuple b c)) = Tuple (Tuple a b) c
        from (Tuple (Tuple a b) c) = Tuple a (Tuple b c)

-- | $a \times b = b \times a$
prodComm :: forall a b. Iso (Tuple a b) (Tuple b a)
prodComm = Iso swap swap

-- | $0 \times a = 0$
prodZeroZero :: forall a b. Iso (Tuple Void a) Void
prodZeroZero = Iso fst absurd

-- | $0 + a = a$
coprodIdent :: forall a. Iso (Either Void a) a
coprodIdent = Iso (either absurd id) Right

-- | $a + (b + c) = (a + b) + c$
coprodAssoc :: forall a b c. Iso (Either a (Either b c)) (Either (Either a b) c)
coprodAssoc = Iso to from
  where to   (Left a)          = Left (Left a)
        to   (Right (Left b))  = Left (Right b)
        to   (Right (Right c)) = Right c
        from (Left (Left a))   = Left a
        from (Left (Right b))  = Right (Left b)
        from (Right c)         = Right (Right c)

-- | $a + b = b + a$
coprodComm :: forall a b. Iso (Either a b) (Either b a)
coprodComm = Iso (either Right Left) (either Right Left)

-- | $a \times (b + c) = (a \times b) + (a \times c)$
distribute :: forall a b c. Iso (Tuple a (Either b c)) (Either (Tuple a b) (Tuple a c))
distribute = Iso to from
  where to (Tuple a e) = B.bimap (Tuple a) (Tuple a) e
        from (Left  t) = map Left t
        from (Right t) = map Right t

-- | $1 + a = `Maybe` a$
onePlusMaybe :: forall a. Iso (Either Unit a) (Maybe a)
onePlusMaybe = Iso (either (const Nothing) Just) (maybe (Left unit) Right)

-- | $1 + 1 = 2$
onePlusOneIsTwo :: forall a. Iso (Either Unit Unit) Boolean
onePlusOneIsTwo = Iso to from
  where to = either (const false) (const true)
        from b = if b then Right unit else Left unit

-- | $a^b \times a^c = a^{b + c}$
expProdSum :: forall a b c. Iso (Tuple (b -> a) (c -> a)) (Either b c -> a)
expProdSum = Iso to from
  where to = uncurry either
        from f = Tuple (f <<< Left) (f <<< Right)

-- | $c^{a \times b} = (c^{b})^{a}$
expExpProd :: forall a b c. Iso (Tuple a b -> c) (a -> b -> c)
expExpProd = Iso curry uncurry

-- | $a^1 = a$
expOne :: forall a. Iso (Unit -> a) a
expOne = Iso ($ unit) const

-- | $a^0 = 1$
expZero :: forall a. Iso (Void -> a) Unit
expZero = Iso (const unit) (const absurd)

-- | $a = b \implies f(a) = f(b)$
functorCong :: forall a b f. (Functor f) => Iso a b -> Iso (f a) (f b)
functorCong (Iso f g) = Iso (map f) (map g)

-- | $a = a\prime \implies f(a, b) = f(a\prime, b)$
bifunctorCongLeft :: forall a a' b f. (B.Bifunctor f) => Iso a a' -> Iso (f a b) (f a' b)
bifunctorCongLeft (Iso f g) = Iso (B.lmap f) (B.lmap g)

-- | $a = b \implies f(a) = f(b)$
contraCong :: forall a b c f. (Contravariant f) => Iso a b -> Iso (f a) (f b)
contraCong (Iso f g) = Iso (cmap g) (cmap f)

-- | $a = a\prime \implies f(a, b) = f(a\prime, b)$
profunctorCongLeft :: forall a a' b c f. (P.Profunctor f) => Iso a a' -> Iso (f a b) (f a' b)
profunctorCongLeft (Iso f g) = Iso (P.lmap g) (P.lmap f)

-- | $b = c \wedge a = b \implies a = c$
instance semigroupoidIso :: Semigroupoid Iso where
  compose (Iso f1 g1) (Iso f2 g2) = Iso (f1 <<< f2) (g1 >>> g2)

-- | $a = a$
instance categoryIso :: Category Iso where
  id = Iso id id
