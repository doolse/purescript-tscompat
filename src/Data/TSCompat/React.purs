module Data.TSCompat.React where 

import React (ReactClass, ReactElement, unsafeCreateElement) as R
import Unsafe.Coerce (unsafeCoerce)

foreign import data ReactNode :: Type 

unsafeCreateElement :: forall clz props child. R.ReactClass clz -> props -> child -> R.ReactElement
unsafeCreateElement = unsafeCoerce R.unsafeCreateElement