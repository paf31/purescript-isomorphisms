module Test.Main where

import Prelude

import Control.Monad.Aff          (Aff())
import Control.Monad.Eff.Random   (RANDOM())
import Data.Either                (Either())
import Data.Maybe                 (Maybe())
import Data.Tuple                 (Tuple())
import Test.Spec                  (Spec(), describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner           (run)
import Test.Spec.QuickCheck       (quickCheck)
import Test.QuickCheck            (Result(), (===))
import Test.QuickCheck.Arbitrary  (Arbitrary)

import Data.Iso

type A = Int
type B = String
type C = Boolean

-- Most of the isomorphisms should have fairly strong parametric guarantees
main = run [consoleReporter] do
  describe "Data.Iso" do
    describe "id" do
      testIso (id :: Iso A A)
    {- compose
     - sym
     -}
    describe "prodIdent" do
      testIso (prodIdent :: Iso A (Tuple Unit A))
    describe "prodAssoc" do
      testIso (prodAssoc :: Iso (Tuple A (Tuple B C)) (Tuple (Tuple A B) C))
    describe "prodComm" do
      testIso (prodComm :: Iso (Tuple A B) (Tuple B A))
    {- prodZeroZero
     - coprodIdent
     -}
    describe "coprodAssoc" do
      testIso (coprodAssoc :: Iso (Either A (Either B C)) (Either (Either A B) C))
    describe "coprodComm" do
      testIso (coprodComm :: Iso (Either A B) (Either B A))
    describe "distribute" do
      testIso (distribute :: Iso (Tuple A (Either B C)) (Either (Tuple A B) (Tuple A C)))
    describe "onePlusMaybe" do
      testIso (onePlusMaybe :: Iso (Either Unit A) (Maybe A))
    describe "onePlusOneIsTwo" do
      testIso (onePlusOneIsTwo :: Iso (Either Unit Unit) Boolean)
    {- expProdSum
     - expOne
     - expZero
     - functorCong
     - bifunctorCongLeft
     - contraCong
     - profunctorCongLeft
     -}


testIso :: forall a b r.
           (Arbitrary a, Arbitrary b, Eq a, Eq b, Show a, Show b)
        => Iso a b
        -> Spec (random :: RANDOM | r) Unit
testIso iso = do
  it "has `backward <<< forward` as the identity" do
    isId (backwards iso <<< forwards iso)
  it "has `forward <<< backward` as the identity" do
    isId (forwards iso <<< backwards iso)

isId :: forall a r.
        (Arbitrary a, Eq a, Show a)
     => (a -> a)
     -> Aff (random :: RANDOM | r) Unit
isId f = quickCheck \x -> f x === x
