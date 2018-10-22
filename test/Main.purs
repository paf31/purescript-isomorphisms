module Test.Main where

import Test.QuickCheck

import Data.Either (Either)
import Data.Iso (Iso, backwards, forwards, onePlusOneIsTwo, onePlusMaybe, distribute, coprodComm, coprodAssoc, prodComm, prodAssoc, prodIdent)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class.Console (log)
import Prelude (class Eq, class Show, Unit, discard, identity, (<<<))

type A = Int
type B = String
type C = Boolean

-- Most of the isomorphisms should have fairly strong parametric guarantees
main :: Effect Unit
main = do
  testIso (identity :: Iso A A)
  {- compose
   - sym
   -}
  log "prodIdent"
  testIso (prodIdent :: Iso (Tuple Unit A) A)
  log "prodAssoc"
  testIso (prodAssoc :: Iso (Tuple A (Tuple B C)) (Tuple (Tuple A B) C))
  log "prodComm"
  testIso (prodComm :: Iso (Tuple A B) (Tuple B A))
  {- prodZeroZero
   - coprodIdent
   -}
  log "coprodAssoc"
  testIso (coprodAssoc :: Iso (Either A (Either B C)) (Either (Either A B) C))
  log "coprodComm"
  testIso (coprodComm :: Iso (Either A B) (Either B A))
  log "distribute"
  testIso (distribute :: Iso (Tuple A (Either B C)) (Either (Tuple A B) (Tuple A C)))
  log "onePlusMaybe"
  testIso (onePlusMaybe :: Iso (Either Unit A) (Maybe A))
  log "onePlusOneIsTwo"
  testIso (onePlusOneIsTwo :: Iso (Either Unit Unit) Boolean)
  {- expProdSum
   - expOne
   - expZero
   - functorCong
   - bifunctorCongLeft
   - contraCong
   - profunctorCongLeft
   -}


testIso :: forall a b
         . Arbitrary a
        => Arbitrary b
        => Eq a
        => Eq b
        => Show a
        => Show b
        => Iso a b
        -> Effect Unit
testIso iso = do
  log "has `backward <<< forward` as the identity"
  isId (backwards iso <<< forwards iso)
  log "has `forward <<< backward` as the identity"
  isId (forwards iso <<< backwards iso)

isId :: forall a
      . Arbitrary a
     => Eq a
     => Show a
     => (a -> a)
     -> Effect Unit
isId f = quickCheck \x -> f x === x
