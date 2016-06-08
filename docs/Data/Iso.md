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
Semigroupoid Iso
Category Iso
```

#### `type (≅)`

``` purescript
infix 4 type Iso as ype (≅
```

#### `forwards`

``` purescript
forwards :: forall a b. a ≅ b -> a -> b
```

Run an isomorphism "forwards".

#### `backwards`

``` purescript
backwards :: forall a b. a ≅ b -> b -> a
```

Run an isomorphism "backwards".

#### `sym`

``` purescript
sym :: forall a b. a ≅ b -> b ≅ a
```

Isomorphism is symmetric

#### `prodIdent`

``` purescript
prodIdent :: forall a. (Tuple Unit a) ≅ a
```

$1 \times a = a$

#### `prodAssoc`

``` purescript
prodAssoc :: forall a b c. (Tuple a (Tuple b c)) ≅ (Tuple (Tuple a b) c)
```

$a \times (b \times c) = (a \times b) \times c$

#### `prodComm`

``` purescript
prodComm :: forall a b. (Tuple a b) ≅ (Tuple b a)
```

$a \times b = b \times a$

#### `prodZeroZero`

``` purescript
prodZeroZero :: forall a. (Tuple Void a) ≅ Void
```

$0 \times a = 0$

#### `coprodIdent`

``` purescript
coprodIdent :: forall a. (Either Void a) ≅ a
```

$0 + a = a$

#### `coprodAssoc`

``` purescript
coprodAssoc :: forall a b c. (Either a (Either b c)) ≅ (Either (Either a b) c)
```

$a + (b + c) = (a + b) + c$

#### `coprodComm`

``` purescript
coprodComm :: forall a b. (Either a b) ≅ (Either b a)
```

$a + b = b + a$

#### `distribute`

``` purescript
distribute :: forall a b c. (Tuple a (Either b c)) ≅ (Either (Tuple a b) (Tuple a c))
```

$a \times (b + c) = (a \times b) + (a \times c)$

#### `onePlusMaybe`

``` purescript
onePlusMaybe :: forall a. (Either Unit a) ≅ (Maybe a)
```

$1 + a = `Maybe` a$

#### `onePlusOneIsTwo`

``` purescript
onePlusOneIsTwo :: (Either Unit Unit) ≅ Boolean
```

$1 + 1 = 2$

#### `expProdSum`

``` purescript
expProdSum :: forall a b c. (Tuple (b -> a) (c -> a)) ≅ (Either b c -> a)
```

$a^b \times a^c = a^{b + c}$

#### `expExpProd`

``` purescript
expExpProd :: forall a b c. (Tuple a b -> c) ≅ (a -> b -> c)
```

$c^{a \times b} = (c^{b})^{a}$

#### `expOne`

``` purescript
expOne :: forall a. (Unit -> a) ≅ a
```

$a^1 = a$

#### `expZero`

``` purescript
expZero :: forall a. (Void -> a) ≅ Unit
```

$a^0 = 1$

#### `functorCong`

``` purescript
functorCong :: forall a b f. Functor f => a ≅ b -> (f a) ≅ (f b)
```

$a = b \implies f(a) = f(b)$

#### `bifunctorCongLeft`

``` purescript
bifunctorCongLeft :: forall a a' b f. Bifunctor f => a ≅ a' -> (f a b) ≅ (f a' b)
```

$a = a\prime \implies f(a, b) = f(a\prime, b)$

#### `contraCong`

``` purescript
contraCong :: forall a b f. Contravariant f => a ≅ b -> (f a) ≅ (f b)
```

$a = b \implies f(a) = f(b)$

#### `profunctorCongLeft`

``` purescript
profunctorCongLeft :: forall a a' b f. Profunctor f => a ≅ a' -> (f a b) ≅ (f a' b)
```

$a = a\prime \implies f(a, b) = f(a\prime, b)$


