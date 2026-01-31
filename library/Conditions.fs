module Ifai.Library.Conditions

type Condition<'a> = {
    Id: string
    Name: string
    Predicate: 'a -> bool
}

