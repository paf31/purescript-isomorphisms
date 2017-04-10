module Test.Main where

import Prelude (class Show, class Eq, Unit, bind, discard, id, (<<<))

import Control.Monad.Eff.Console (log)
import Data.Either (Either)
import Data.Iso (Iso, backwards, forwards, onePlusOneIsTwo, onePlusMaybe, distribute, coprodComm, coprodAssoc, prodComm, prodAssoc, prodIdent)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Test.QuickCheck

type A = Int
type B = String
type C = Boolean

-- Most of the isomorphisms should have fairly strong parametric guarantees
main :: QC () Unit
main = do
  testIso (id :: Iso A A)
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
        -> QC () Unit
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
     -> QC () Unit
isId f = quickCheck \x -> f x === x
