program WitnessSeed;

uses
  System.SysUtils,
  WitnessSeedUnit in 'WitnessSeedUnit.pas';

begin
  try
    var Witness := TWitnessSeed.Create;
    try
      Witness.Run;
    finally
      Witness.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.