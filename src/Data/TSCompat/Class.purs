module Data.TSCompat.Class where 

import Data.Nullable (Nullable)
import Data.TSCompat (Any, OneOf, OptionRecord)
import Data.TSCompat.React (ReactNode)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)
import Prim.Row as Row
import Prim.RowList as RL
import React (class IsReactElement)
import Type.Data.Boolean as B
import Type.Data.Symbol as Sym
import Type.Row (RProxy(..))

class TsTypeExists t (rl :: RL.RowList) (opt:: B.Boolean) (o :: B.Boolean) | t rl -> o
instance consCheck :: (IsEq t t2 opt eq, TsTypeExists t tail opt tailEq, B.Or eq tailEq out) 
    => TsTypeExists t (RL.Cons "typed" t2 tail) opt out
instance nilCheck :: TsTypeExists t RL.Nil opt B.False

class ConstainsAll (out :: RL.RowList) (b :: # Type)
instance consAll :: (ConstainsAll tail b, Row.Cons s any btail b) => ConstainsAll (RL.Cons s t tail) b
instance nilAll :: ConstainsAll RL.Nil b 

{-- By using this class we can get pretty good error messages --}
class TSCompatible (s :: Symbol) ta tb (b :: B.Boolean)
instance onlyTrue :: TSCompatible s ta tb B.True
instance tryAgain :: TSCompatible s a a B.False

class IsOptional (s :: Symbol) (m :: RL.RowList) (b :: B.Boolean) | s m -> b 
instance consOpt :: (Sym.Equals s s2 eq, B.Not eq neq, IsOptional s tail tailopt, B.And neq tailopt opt) => IsOptional s (RL.Cons s2 any tail) opt
instance nilOpt :: IsOptional s RL.Nil B.True

class IsEqRowList (l :: RL.RowList) (b :: # Type) (m :: RL.RowList)
instance consOptEQ :: (Row.Cons s tb tailb b, IsOptional s m o, IsEq ta tb o eq, 
    TSCompatible s ta tb eq, IsEqRowList taila b m) => IsEqRowList (RL.Cons s ta taila) b m
instance nilRLEQ :: IsEqRowList RL.Nil b m

class IsEq a b (o :: B.Boolean) (eq :: B.Boolean) | a b o -> eq
instance reflTSEq :: IsEq a a o B.True
else instance anyIsEq :: IsEq a Any o B.True
else instance unionIsEq :: (RL.RowToList b rl, TsTypeExists a rl o eq) 
    => IsEq a (OneOf b) o eq
else instance intIsNumber :: IsEq Int Number o B.True
else instance reactElement :: IsReactElement a => IsEq a ReactNode o B.True
else instance effectAsFn1 :: IsEq (Effect a) (EffectFn1 e a) o B.True
else instance effFunc1Eq :: (IsEq a a2 o eqa, IsEq b b2 o eqb, B.And eqa eqb eq) 
    => IsEq (EffectFn1 a b) (EffectFn1 a2 b2) o eq
else instance effFunc2Eq :: (IsEq a a2 o eqa, IsEq b b2 o eqb, B.And eqa eqb eqab, IsEq c c2 o eqc, B.And eqab eqc eq) 
    => IsEq (EffectFn2 a b c) (EffectFn2 a2 b2 c2) o eq
else instance typeConsEq :: IsEq a b o eq => IsEq (m a) (m b) o eq
else instance optRecord :: (RL.RowToList a al, RL.RowToList mand ml, 
    IsEqRowList al all ml, ConstainsAll ml a) => IsEq (Record a) (OptionRecord all mand) o B.True
else instance optionalNull :: IsEq a b B.False eq => IsEq (Nullable a) b B.True eq
else instance notEq :: IsEq a b o B.False
 
class IsTSEq a b 
instance anyTSEq :: IsEq a b B.False B.True => IsTSEq a b
