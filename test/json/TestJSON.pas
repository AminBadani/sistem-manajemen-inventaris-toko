{$MODE DELPHI}

PROGRAM TestJSON;
USES crt, fpjson, jsonparser, sysutils, classes, jsonconf;
VAR 
  JsonData: TJSONData;
  JsonItem: TJSONData;

  JsonObject: TJSONObject;
  i: integer;

function LoadJSON(const namaFile: string): TJSONData;
var 
  TemporaryFile: TMemoryStream;
begin
  Result := Nil;
  TemporaryFile := TMemoryStream.Create;
  try
    try
      TemporaryFile.loadFromFile(namaFile);
      Result := GetJSON(TemporaryFile);
    except
      on E:Exception do
        writeln('File ', namaFile, ' could not be found');
    end;
  finally
    TemporaryFile.free;
  end;
end;

procedure CreateJSON;
var 
  TemporaryStringList: TStringList;
  NewJsonFile: TJSONConfig;
begin
  NewJsonFile := TJSONConfig.Create(nil);

  try
    NewJsonFile.Formatted := True;
    NewJsonFile.Filename := 'sample.json';
    TemporaryStringList := TStringList.Create;

    try
      TemporaryStringList.SetStrings(['durable', 'economic']);
      NewJsonFile.SetValue('/cars/toyota corolla', TemporaryStringList);
      TemporaryStringList.SetStrings(['cheap', 'elegance']);
      NewJsonFile.SetValue('/cars/hyundai i20', TemporaryStringList);
      TemporaryStringList.SetStrings(['speed', 'versatile']);
      NewJsonFile.SetValue('/motorcycle/Bimota KB4', TemporaryStringList);
      TemporaryStringList.SetStrings(['cheap', 'economic']);
      NewJsonFile.SetValue('/motorcycle/Ducati DesertX', TemporaryStringList);
      NewJsonFile.SetValue('/motorcycle', 'Honda Beat');
    finally
      TemporaryStringList.Free;
    end;
  finally
    NewJsonFile.Free;
  end;
end;

BEGIN
  ClrScr;

  CreateJSON;

  JsonData := LoadJSON('test.json');

  for i := 0 to JsonData.Count - 1 do
    begin  
      JsonItem := JsonData.Items[i];

      JsonObject := JsonItem as TJSONObject;
      WriteLn(JsonObject.Get('nama'));
    end;

  JsonData.Free;
END.