program TestRWD;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  RWD in 'src\RWD.pas';

var
  I, IDot: array[1..10] of Double;
  Phase, Fieldprint: Double;
  J: Integer;

begin
  Randomize;
  for J := 1 to 10 do
    I[J] := Random;
  Phase := 0.0;
  ComputeDynamics(I, IDot, Phase);
  ComputeFieldprint(I, Fieldprint);
  if Fieldprint > 0.0 then
    Writeln('RWD test passed: Fieldprint = ', Fieldprint:0:6)
  else
  begin
    Writeln('RWD test failed');
    Halt(1);
  end;
  Readln;
end.