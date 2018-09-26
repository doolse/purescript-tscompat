module Test.Main where 

import Prelude

import Data.Array as Array
import Data.List as L
import Data.Nullable (Nullable)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.TSCompat (Any, OneOf, OptionRecord)
import Data.TSCompat.Class (class IsEq)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Uncurried (EffectFn1)
import Prim.RowList as RL
import Type.Data.Boolean (BProxy(..), False, True)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

class TypeAsString a where 
  asString :: Proxy a -> String 

class RowListAsStrings (a :: RL.RowList) where 
  asStrings :: RLProxy a -> L.List {l::String, t::String}

unionString :: forall rt rl. RL.RowToList rt rl => RowListAsStrings rl => RProxy rt -> String 
unionString _ = "(" <> (joinWith "|" (map _.t $ Array.fromFoldable $ asStrings (RLProxy :: RLProxy rl))) <> ")"

nameType :: { t :: String, l :: String} -> String
nameType {l,t} = l <> " :: " <> t 

instance consString :: (TypeAsString t, IsSymbol s, RowListAsStrings tail) => RowListAsStrings (RL.Cons s t tail) 
  where asStrings _ = L.Cons ({l: reflectSymbol (SProxy :: SProxy s), t: asString (Proxy :: Proxy t)}) (asStrings (RLProxy :: RLProxy tail))
instance nilTString :: RowListAsStrings RL.Nil where asStrings _ = L.Nil

instance intString :: TypeAsString Int where asString _ = "Int"
instance numberString :: TypeAsString Number where asString _ = "Number"
instance stringString :: TypeAsString String where asString _ = "String"
instance booleanString :: TypeAsString Boolean where asString _ = "Boolean"
instance unitString :: TypeAsString Unit where asString _ = "Unit"
instance nullableString :: TypeAsString a => TypeAsString (Nullable a) where asString _ = "Nullable " <> asString (Proxy :: Proxy a)
instance effectString :: TypeAsString a => TypeAsString (Effect a) where asString _ = "Effect " <> asString (Proxy :: Proxy a)
instance effectFn1String :: (TypeAsString a, TypeAsString b) => TypeAsString (EffectFn1 a b) where 
  asString _ = "EffectFn1 " <> asString (Proxy :: Proxy a) <> " " <> asString (Proxy :: Proxy b)
instance oneOfString :: (RL.RowToList o rl, RowListAsStrings rl) => TypeAsString (OneOf o) where 
  asString _ = unionString (RProxy :: RProxy o) 
instance recordString :: (RL.RowToList o rl, RowListAsStrings rl) => TypeAsString (Record o) where 
  asString _ = "{" <> (joinWith ", " $ nameType <$> (Array.fromFoldable $ asStrings (RLProxy :: RLProxy rl))) <> "}"
instance optRecordString :: (RL.RowToList o rl, RL.RowToList m rlm, RowListAsStrings rl, RowListAsStrings rlm) => TypeAsString (OptionRecord o m) where 
  asString _ = let 
    mandLabels = _.l <$> asStrings (RLProxy :: RLProxy rlm)
    nameOptType {l,t} = let question = if L.any (eq l) mandLabels then "" else "?" in l <> question <> " :: " <> t 
    in "{" <> (joinWith ", " $ nameOptType <$> (Array.fromFoldable $ asStrings (RLProxy :: RLProxy rl))) <> "}"

instance anyString :: TypeAsString Any where asString _ = "Any"

bTrue :: BProxy True
bTrue = BProxy

bFalse :: BProxy False
bFalse = BProxy 

intT :: Proxy Int
intT = Proxy

numberT :: Proxy Number
numberT = Proxy

stringT :: Proxy String 
stringT = Proxy
 
booleanT :: Proxy Boolean
booleanT = Proxy

anyT :: Proxy Any
anyT = Proxy

nullableT :: forall a. Proxy a -> Proxy (Nullable a)
nullableT _ = Proxy

compare :: forall a b o eq. 
    IsEq a b o eq => 
    TypeAsString a => 
    TypeAsString b => 
    BProxy o -> BProxy eq -> String -> Proxy a -> Proxy b -> Effect Unit
compare _ _ op a b = log $ asString a <> op <> asString b

oneOf2 :: forall a b. Proxy a -> Proxy b -> Proxy (OneOf (typed::a,typed::b))
oneOf2 _ _ = Proxy

compareEQ :: forall a b. IsEq a b False True => TypeAsString a => TypeAsString b => Proxy a -> Proxy b -> Effect Unit
compareEQ = compare bFalse bTrue " = "
compareNEQ :: forall a b. IsEq a b False False => TypeAsString a => TypeAsString b => Proxy a -> Proxy b -> Effect Unit
compareNEQ = compare bFalse bFalse " /= "
compareOptEQ :: forall a b. IsEq a b True True => TypeAsString a => TypeAsString b => Proxy a -> Proxy b -> Effect Unit
compareOptEQ = compare bTrue bTrue " o= "

main :: Effect Unit 
main = do 
  compareEQ intT numberT
  compareNEQ stringT numberT
  compareOptEQ (nullableT intT) intT
  compareNEQ (nullableT intT) intT
  compareEQ booleanT (oneOf2 intT booleanT)
  compareNEQ booleanT (oneOf2 stringT intT)
  compareEQ booleanT (oneOf2 intT (oneOf2 stringT booleanT))
  compareEQ booleanT anyT
  compareEQ stringT anyT
  compareEQ (Proxy :: Proxy {disabled::Boolean}) (Proxy :: Proxy (OptionRecord (disabled::Boolean) ()))
  compareEQ (Proxy :: Proxy {disabled::Boolean}) (Proxy :: Proxy (OptionRecord (disabled::Boolean,opt::String) (disabled::Boolean)))
  compareEQ (Proxy :: Proxy {disabled::Boolean, opt::Nullable String}) 
            (Proxy :: Proxy (OptionRecord (disabled::Boolean,opt::OneOf (typed::String, typed::Int)) (disabled::Boolean)))
  compareEQ (Proxy :: Proxy (Effect Unit)) (Proxy :: Proxy (EffectFn1 String Unit)) 
  compareNEQ (Proxy :: Proxy (Effect String)) (Proxy :: Proxy (EffectFn1 String Unit)) 
  -- compareEQ (Proxy :: Proxy {disabled::Boolean}) (Proxy :: Proxy (OptionRecord (disabled::Boolean,open::Boolean) (open::Boolean)))
