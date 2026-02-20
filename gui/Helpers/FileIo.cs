using System;
using System.IO;
using System.Text.Json;
using System.Text.Json.Serialization;
using Ifai.Lib;
using Microsoft.FSharp.Core;

namespace Ifai.Gui.Helpers;

public class FileIo : IFileIO
{
    private readonly JsonSerializerOptions _options;

    public FileIo()
    {
        _options =
            JsonFSharpOptions
                .Default()
                .WithAllowNullFields()
                .ToJsonSerializerOptions();
    }

    public WriteFileResult WriteFile(string filename, bool allowOverwrite, string content)
    {
        try
        {
            if (File.Exists(filename) && !allowOverwrite)
            {
                return WriteFileResult.NewAlreadyExists(filename);
            }

            File.WriteAllText(filename, content);
            return WriteFileResult.Success;
        }
        catch (Exception ex)
        {
            return WriteFileResult.NewFailure(ex.Message);
        }
    }

    public FSharpResult<string, string> Serialize(object obj)
    {
        try
        {
            var json = JsonSerializer.Serialize(obj, _options);
            return FSharpResult<string, string>.NewOk(json);
        }
        catch (Exception ex)
        {
            return FSharpResult<string, string>.NewError(ex.Message);
        }
    }
}