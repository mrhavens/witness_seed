with Witness_Seed; use Witness_Seed;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   State : Witness_State;
   File : Witness_IO.File_Type;
   Initial_Data : Sensory_Data;
begin
   -- Load initial state
   Open (File, In_File, "data/witness_memory.dat");
   Load_Memory (State, File);
   Close (File);

   Sense (Initial_Data);

   -- Run Witness Cycle
   Witness_Cycle (5, Initial_Data, State);

   -- Save final state
   Open (File, Out_File, "data/witness_memory.dat");
   Save_Memory (State, File);
   Close (File);
end Main;