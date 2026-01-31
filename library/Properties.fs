module Ifai.Library.Properties

type NumericProperty<'T when 'T : comparison> =
    { Id    : string
      Name  : string
      Value : 'T
      Min   : 'T option
      Max   : 'T option }
    
type IntProperty = NumericProperty<int>

type FloatProperty = NumericProperty<float>

type BoolProperty =
    { Id    : string
      Name  : string
      Value : bool }

type StringProperty =
    { Id       : string
      Name     : string
      Value    : string
      MinLength: int option
      MaxLength: int option }

type Property =
    | IntProperty of IntProperty
    | FloatProperty of FloatProperty
    | StringProperty of StringProperty
    | BoolProperty of BoolProperty
    
