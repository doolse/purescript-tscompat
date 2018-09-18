module Data.TSCompat.Class where 

import Data.TSCompat (Any, OneOf)
import Data.TSCompat.React (ReactNode)
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Prim.Row as Row
import Prim.RowList as RL
import React (ReactElement)
import Type.Data.Boolean as B
import Type.Row (RProxy(..))

class TsTypeExist t (rl :: RL.RowList) (o :: B.Boolean) | t rl -> o
instance consCheck :: (IsTSEq t t2 eq, TsTypeExist t tail tailEq, B.Or eq tailEq out) 
    => TsTypeExist t (RL.Cons "typed" t2 tail) out
instance nilCheck :: TsTypeExist t RL.Nil B.False

-- else instance constCheck :: (IsTSEq l t t2 o, TsTypeExist l t r tail to, B.Or o to res) => TsTypeExist l t r (RL.Cons "typed" t2 tail) res

class IsTSAllProps (out :: RL.RowList) (b :: # Type)
instance consAll :: (IsTSAllProps tail b, Row.Cons s any btail b) => IsTSAllProps (RL.Cons s t tail) b
instance nilAll :: IsTSAllProps RL.Nil b 

class TSCompatible (s :: Symbol) ta tb (b :: B.Boolean)
instance onlyTrue :: TSCompatible s ta tb B.True

class IsTSEqRL (l :: RL.RowList) (b :: # Type)
instance consOptEQ :: (Row.Cons s tb tailb b, IsTSEq ta tb eq, TSCompatible s ta tb eq, IsTSEqRL taila b) => IsTSEqRL (RL.Cons s ta taila) b
instance nilRLEQ :: IsTSEqRL RL.Nil b

class IsTSEq a b (o :: B.Boolean) | a b -> o
instance reflTSEq :: IsTSEq a a B.True
else instance anyType :: IsTSEq a Any B.True
else instance typeUnion :: (RL.RowToList b rl, TsTypeExist a rl eq) => IsTSEq a (OneOf b) eq
else instance intTSNumber :: IsTSEq Int Number B.True
else instance isReactElementString :: IsTSEq String ReactNode B.True
else instance isReactElementNumber :: IsTSEq Number ReactNode B.True
else instance isReactElementInt :: IsTSEq Int ReactNode B.True
else instance isReactElementReactElement :: IsTSEq ReactElement ReactNode B.True
else instance isReactElementArray :: IsTSEq (Array ReactElement) ReactNode B.True
else instance effAsFunc :: IsTSEq (Effect a) (EffectFn1 e a) B.True
else instance effFunc1Eq :: (IsTSEq a a2 eqa, IsTSEq b b2 eqb, B.And eqa eqb eq) => IsTSEq (EffectFn1 a b) (EffectFn1 a2 b2) eq
else instance secondLevel :: IsTSEq a b eq => IsTSEq (m a) (m b) eq
else instance no :: IsTSEq a b B.False
 
class IsTSRecord (a :: # Type) (all :: # Type) (mand :: # Type) 
instance recordInst :: (RL.RowToList a al, RL.RowToList mand ml, IsTSEqRL al all, IsTSAllProps ml a) => IsTSRecord a all mand

-- test :: forall r b. RL.RowToList b bl TsTypeExist t bl => RProxy b -> Boolean
-- test a = true

-- testIt = test (RProxy :: RProxy (type :: Boolean, type :: String))