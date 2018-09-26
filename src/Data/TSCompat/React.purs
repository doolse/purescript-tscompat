module Data.TSCompat.React where 

import Data.Nullable (Nullable)
import Data.TSCompat (OneOf)
import React (ReactClass, ReactElement, unsafeCreateElement) as R
import Unsafe.Coerce (unsafeCoerce)

type ReactNode = OneOf (
    typed :: Number,
    typed :: String, 
    typed :: R.ReactElement, 
    typed :: Nullable R.ReactElement,
    typed :: Array R.ReactElement
)

unsafeCreateElement :: forall clz props child. R.ReactClass clz -> props -> child -> R.ReactElement
unsafeCreateElement = unsafeCoerce R.unsafeCreateElement