namespace Ifai.ContentParser

open System

type ComplexLine = {
    Indentation: uint
    Key: string
    Value: string option
    StartsWithDash: bool  
}


type ContentLine =
    | ComplexLine of ComplexLine
    | StringLine of string


module IndentationParser =
    let parseLine (line: string) : ContentLine option =
        if String.IsNullOrWhiteSpace line then
            None
        else
            // Remove comments but preserve indentation
            let commentIndex = line.IndexOf('#')
            let contentBeforeComment = 
                if commentIndex >= 0 then 
                    line.Substring(0, commentIndex).TrimEnd()
                else 
                    line.TrimEnd()
            
            if String.IsNullOrWhiteSpace contentBeforeComment then
                None
            else
                if contentBeforeComment.Contains(":") || contentBeforeComment.Contains("-") then
                    // Structured line (ContentLine)
                    let trimmed = contentBeforeComment.TrimStart()
                    let indentation = uint (contentBeforeComment.Length - trimmed.Length)
                    let startsWithDash = trimmed.StartsWith("-")
                    let contentWithoutDash = 
                        if startsWithDash then 
                            trimmed.Substring(1).TrimStart() 
                        else 
                            trimmed

                    let colonIndex = contentWithoutDash.IndexOf(':')
                    let key, value =
                        if colonIndex >= 0 then
                            let k = contentWithoutDash.Substring(0, colonIndex).Trim()
                            let vStr = contentWithoutDash.Substring(colonIndex + 1).Trim()
                            let v = if String.IsNullOrWhiteSpace vStr then None else Some vStr
                            k, v
                        else
                            contentWithoutDash.Trim(), None
                    
                    Some (ContentLine.ComplexLine {
                        Indentation = indentation
                        Key = key
                        Value = value
                        StartsWithDash = startsWithDash
                    })
                else
                    // Unstructured line (StringLine)
                    Some (ContentLine.StringLine contentBeforeComment)

    let parse (content: string) : ContentLine list =
        content.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose parseLine
        |> Array.toList
