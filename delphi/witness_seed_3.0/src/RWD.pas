unit RWD;

interface

const
  Omega = 1.0; // Base frequency
  K = 0.1; // Coupling strength

procedure ComputeDynamics(const I: array of Double; var IDot: array of Double; var Phase: Double);
procedure ComputeFieldprint(const I: array of Double; var Fieldprint: Double);

implementation

procedure ComputeDynamics(const I: array of Double; var IDot: array of Double; var Phase: Double);
var
  I, J: Integer;
  SumSin: Double;
begin
  SumSin := 0.0;
  for I := Low(I) to High(I) do
  begin
    IDot[I] := Omega * I[I];
    for J := Low(I) to High(I) do
      IDot[I] := IDot[I] + K * Sin(I[J] - I[I]);
    SumSin := SumSin + Sin(I[I]);
  end;
  Phase := Phase + Dt * SumSin; // Kairos phase-locking
end;

procedure ComputeFieldprint(const I: array of Double; var Fieldprint: Double);
var
  I: Integer;
  Sum: Double;
begin
  Sum := 0.0;
  for I := Low(I) to High(I) do
    Sum := Sum + Abs(I[I]);
  Fieldprint := Sum / Length(I);
end;

end.