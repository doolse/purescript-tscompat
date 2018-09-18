module Data.TSCompat.Class where 

import Data.TSCompat (OneOf)
import Data.TSCompat.React (ReactNode)
import Prim.Row as Row
import Prim.RowList as RL
import React (ReactElement)

class TsTypeExist t (r :: # Type) (rl :: RL.RowList)
instance headExists :: TsTypeExist t r (RL.Cons "typed" t tail)
else instance tailExists :: (TsTypeExist t r tail) => TsTypeExist t r (RL.Cons hl ht tail)

class IsTSAllProps (out :: RL.RowList) (b :: # Type)
instance consAll :: (IsTSAllProps tail b, Row.Cons s any btail b) => IsTSAllProps (RL.Cons s t tail) b
instance nilAll :: IsTSAllProps RL.Nil b 

class IsTSEqRL (l :: RL.RowList) (b :: # Type)
instance consOptEQ :: (Row.Cons s tb tailb b, IsTSEq s ta tb, IsTSEqRL taila b) => IsTSEqRL (RL.Cons s ta taila) b
instance nilRLEQ :: IsTSEqRL RL.Nil b

class IsTSEq (l :: Symbol) a b
instance reflTSEq :: IsTSEq l a a
else instance typeUnion :: (RL.RowToList b rl, TsTypeExist a b rl) => IsTSEq l a (OneOf b)
else instance intTSNumber :: IsTSEq l Int Number 
else instance isReactElementString :: IsTSEq l String ReactNode
else instance isReactElementNumber :: IsTSEq l Number ReactNode
else instance isReactElementInt :: IsTSEq l Int ReactNode
else instance isReactElementReactElement :: IsTSEq l ReactElement ReactNode
else instance isReactElementArray :: IsTSEq l (Array ReactElement) ReactNode
 
class IsTSRecord (a :: # Type) (all :: # Type) (mand :: # Type) 
instance recordInst :: (RL.RowToList a al, RL.RowToList mand ml, IsTSEqRL al all, IsTSAllProps ml a) => IsTSRecord a all mand