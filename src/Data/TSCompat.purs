module Data.TSCompat where 

foreign import data Any :: Type
foreign import data OneOf :: # Type -> Type
foreign import data StringConst :: Symbol -> Type
foreign import data OptionRecord :: # Type -> # Type -> Type
