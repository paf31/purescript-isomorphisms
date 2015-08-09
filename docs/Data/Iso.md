## Module Data.Iso

#### `Iso`

``` purescript
data Iso a b
  = Iso (a -> b) (b -> a)
```

An isomorphism between types `a` and `b` consists of a pair of functions 
`f :: a -> b` and `g :: b -> a`, satisfying the following laws:

- `f <<< g = id`
- `g <<< f = id`

The isomorphisms in this library satisfy these laws by construction.

##### Instances
``` purescript
instance semigroupoidIso :: Semigroupoid Iso
instance categoryIso :: Category Iso
```

#### `forwards`

``` purescript
forwards :: forall a b. Iso a b -> a -> b
```

Run an isomorphism "forwards".

#### `backwards`

``` purescript
backwards :: forall a b. Iso a b -> b -> a
```

Run an isomorphism "backwards".

#### `sym`

``` purescript
sym :: forall a b. Iso a b -> Iso b a
```

Isomorphism is symmetric

#### `prodIdent`

``` purescript
prodIdent :: forall a. Iso a (Tuple Unit a)
```

#### `prodAssoc`

``` purescript
prodAssoc :: forall a b c. Iso (Tuple a (Tuple b c)) (Tuple (Tuple a b) c)
```

#### `prodComm`

``` purescript
prodComm :: forall a b. Iso (Tuple a b) (Tuple b a)
```

#### `prodZeroZero`

``` purescript
prodZeroZero :: forall a b. Iso (Tuple Void a) Void
```

#### `coprodIdent`

``` purescript
coprodIdent :: forall a. Iso a (Either Void a)
```

#### `coprodAssoc`

``` purescript
coprodAssoc :: forall a b c. Iso (Either a (Either b c)) (Either (Either a b) c)
```

#### `coprodComm`

``` purescript
coprodComm :: forall a b. Iso (Either a b) (Either b a)
```

#### `distribute`

``` purescript
distribute :: forall a b c. Iso (Tuple a (Either b c)) (Either (Tuple a b) (Tuple a c))
```

#### `onePlusMaybe`

``` purescript
onePlusMaybe :: forall a. Iso (Either Unit a) (Maybe a)
```

#### `onePlusOneIsTwo`

``` purescript
onePlusOneIsTwo :: forall a. Iso (Either Unit Unit) Boolean
```

#### `expProdSum`

``` purescript
expProdSum :: forall a b c. Iso (Tuple (b -> a) (c -> a)) (Either b c -> a)
```

#### `expExpProd`

``` purescript
expExpProd :: forall a b c. Iso (Tuple a b -> c) (a -> b -> c)
```

#### `expOne`

``` purescript
expOne :: forall a. Iso (Unit -> a) a
```

#### `expZero`

``` purescript
expZero :: forall a. Iso (Void -> a) Unit
```

#### `functorCong`

``` purescript
functorCong :: forall a b f. (Functor f) => Iso a b -> Iso (f a) (f b)
```

#### `bifunctorCongLeft`

``` purescript
bifunctorCongLeft :: forall a a' b f. (Bifunctor f) => Iso a a' -> Iso (f a b) (f a' b)
```

#### `contraCong`

``` purescript
contraCong :: forall a b c f. (Contravariant f) => Iso a b -> Iso (f a) (f b)
```

#### `profunctorCongLeft`

``` purescript
profunctorCongLeft :: forall a a' b c f. (Profunctor f) => Iso a a' -> Iso (f a b) (f a' b)
```


