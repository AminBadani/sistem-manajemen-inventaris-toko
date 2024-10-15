{$MODE DELPHI}
program TestText;

uses SysUtils, Classes;
  
type
  NPC = record
    name: string;
    token: integer;
    Species: string;
    IsAlive: boolean;
    description: string;
    pronoun: string;
    relationship: integer;  //1-5 1=unfriendly 3=neutral 5=friendly
    location: integer; //Location token, cannot be negative, except -1023.
  end;
  
var
  n: NPC;
  m: TMemoryStream;
  
begin
  n.name := 'hello there';
  n.Species := 'developer';
  
  m := TMemoryStream.Create;
  try
    m.Write(n, SizeOf(n));
    m.SaveToFile('file.txt');
  finally  
    m.Free;
  end;
  
  //zero NPC var
  FillChar(n, SizeOf(n), 0);
  
  //read
  m := TMemoryStream.Create;
  try
    m.LoadFromFile('file.txt');
    //read to record variable
    m.Read(n, SizeOf(n));
  
    WriteLn('NPC name = ', n.name);
    WriteLn('Species = ', n.Species);
  finally
    m.Free;
  end;
  
  readln;
end.