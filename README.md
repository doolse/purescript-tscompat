# purescript-tscompat

This library aims to provide types and type classes for typechecking interop with Typescript code without the need for conversion. 

In conjuction with [purescript-readts](http://github.com/doolse/purescript-readts), purescript bindings for JS libraries which are written in Typescript or have type definitions can be created with little effort.

## Types and Typeclasses 

`IsTSEq a b` - Given a standard purescript type `a` does it match `b` where `b` can contain the following types:

|Type|TS type|Matches|
|----|-------|-------|
|`Any`|`any`|Anything|
|`OneOf (typed :: TypeA, typed :: TypeB, typed :: TypeC)`|`(TypeA\|TypeB\|TypeC)`|Either one of TypeA,TypeB or TypeC|
|`StringConst "text"`|`"text"`|An exact string|
|`OptionRecord (optional :: TypeA, mandatory :: TypeB) (mandatory :: TypeB)`|`{optional? : TypeA; mandatory: TypeB }`|A record which must contain all the fields in the second row type

In addition to these equivalences the following additional rules apply:

`Int` will match `Number`

`Effect b` matches `EffectFn1 a b` - very useful for event handlers when you don't need the event.

`Nullable a` matches `a` if it's one of the optional properties in `OptionRecord`.

## Examples 

Given the following definitions:
```purescript
type DialogPropsO r = (
  key :: OneOf (
    typed :: String,
    typed :: Number),
  fullScreen :: Boolean,
  fullWidth :: Boolean,
  scroll :: OneOf (
    typed :: StringConst "paper",
    typed :: StringConst "body")
  | r)

type DialogPropsM = (open :: Boolean)
type DialogProps = DialogPropsO DialogPropsM

paper :: StringConst "paper"
paper = unsafeCoerce "paper"

body :: StringConst "body"
body = unsafeCoerce "body"

dialog :: forall a. IsTSEq (Record a) (OptionRecord DialogProps DialogPropsM) => Record a -> Unit
```
Usage:
```purescript
dialog {key: 1}
-- Fails because `open` is not specified
dialog {open: true, scroll: body} 
-- Succeeds 
dialog {open: false, key: "stringKey", fullWidth: toNullable $ Just true}
-- Succeeds because optional fields can be Nullable
```
