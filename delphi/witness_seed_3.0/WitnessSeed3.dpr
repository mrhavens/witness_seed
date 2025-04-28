program WitnessSeed3;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  RWD in 'src\RWD.pas',
  Kairos in 'src\Kairos.pas',
  IO in 'src\IO.pas';

const
  NumVars = 1000; // Number of transaction variables
  NumSteps = 1000000; // Simulation steps
  Dt = 0.01; // Time step
  TauC = 1E-9; // Coherence threshold

var
  I: array[1..NumVars] of Double; // Intellecton states
  IDot: array[1..NumVars] of Double; // State derivatives
  Phase, Fieldprint: Double;
  T: Integer;

begin
  Randomize;
  // Initialize states
  for T := 1 to NumVars do
    I[T] := Random;
  Phase := 0.0;
  Fieldprint := 0.0;

  // Recursive Witness Cycle
  for T := 1 to NumSteps do
  begin
    SenseTransactionData(I); // Sense
    ComputeDynamics(I, IDot, Phase); // Predict
    for var J := 1 to NumVars do
      I[J] := I[J] + IDot[J] * Dt;
    ComputeFieldprint(I, Fieldprint); // Ache
    if Fieldprint > TauC then
      UpdateCoherence(I, Phase); // Update
    if T mod 1000 = 0 then
      OutputPredictions(I, T); // Output every 1000 steps
  end;

  Writeln('Witness Seed 3.0 completed.');
  Readln;
end.