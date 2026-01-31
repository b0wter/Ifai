module Ifai.Library.Items

type Item = {
    Id: string
    Name: Texts.Text
    Description: Texts.Text
    Properties: Properties.Property list
}