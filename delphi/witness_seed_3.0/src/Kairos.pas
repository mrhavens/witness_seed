unit Kairos;

interface

procedure UpdateCoherence(var I: array of Double; const Phase: Double);

implementation

procedure UpdateCoherence(var I: array of Double; const Phase: Double);
var
  I: Integer;
begin
  for I := Low(I) to High(I) do
    I[I] := I[I] * Cos(Phase);
end;

end.