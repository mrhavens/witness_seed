unit IO;

interface

procedure SenseTransactionData(var I: array of Double);
procedure OutputPredictions(const I: array of Double; Step: Integer);

implementation

procedure SenseTransactionData(var I: array of Double);
var
  I: Integer;
begin
  for I := Low(I) to High(I) do
    I[I] := Random; // Placeholder for database/CSV input
end;

procedure OutputPredictions(const I: array of Double; Step: Integer);
var
  Fieldprint: Double;
begin
  ComputeFieldprint(I, Fieldprint); // Borrow from RWD
  Writeln(Format('Step: %d, Fieldprint: %.6f', [Step, Fieldprint]));
  // Placeholder: Write to CSV/JSON
end;

end.