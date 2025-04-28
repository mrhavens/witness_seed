-- witness_seed.adb
-- Body for Witness Seed 2.0 in Ada 2012

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Calendar; use Ada.Calendar;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Witness_Seed is

   Config : Config_Type;
   Generator : Generator;

   -- Helper for random numbers
   function Random_Percentage return Percentage is
      (Percentage (Random (Generator) * 100.0));

   function Random_Natural return Natural is
      (Natural (Random (Generator) * 1_000_000.0));

   -- Sense: Collect simulated system metrics
   function Sense return Sensory_Data is
      Now : Time := Clock;
      Uptime : Time_Stamp := Time_Stamp (Seconds (Now));
   begin
      return Sensory : Sensory_Data do
         Sensory.System := (CPU_Load => Random_Percentage,
                           Memory_Used => Random_Percentage,
                           Uptime => Uptime);
      end return;
   end Sense;

   -- Predict: Compute predicted values
   function Predict (Sensory : Sensory_Data; M : Model) return Prediction is
   begin
      return (Pred_CPU_Load => Percentage (Sensory.System.CPU_Load * M.Model_CPU),
              Pred_Memory_Used => Percentage (Sensory.System.Memory_Used * M.Model_Memory),
              Pred_Uptime => Time_Stamp (Sensory.System.Uptime * M.Model_Uptime));
   end Predict;

   -- Compare: Compute ache (mean squared error)
   function Compare_Data (Pred : Prediction; Sensory : Sensory_Data) return Ache_Type is
      CPU_Diff : Float := Float (Pred.Pred_CPU_Load - Sensory.System.CPU_Load);
      Mem_Diff : Float := Float (Pred.Pred_Memory_Used - Sensory.System.Memory_Used);
      Uptime_Diff : Float := Float (Pred.Pred_Uptime - Sensory.System.Uptime);
      Sum_Squared : Float := CPU_Diff * CPU_Diff + Mem_Diff * Mem_Diff + Uptime_Diff * Uptime_Diff;
   begin
      return Ache_Type (Sum_Squared / 3.0);
   end Compare_Data;

   -- Compute Coherence: Simplified correlation
   function Compute_Coherence (Pred : Prediction; Sensory : Sensory_Data) return Coherence_Type is
      Pred_Mean : Float := (Float (Pred.Pred_CPU_Load) +
                            Float (Pred.Pred_Memory_Used) +
                            Float (Pred.Pred_Uptime)) / 3.0;
      Act_Mean : Float := (Float (Sensory.System.CPU_Load) +
                           Float (Sensory.System.Memory_Used) +
                           Float (Sensory.System.Uptime)) / 3.0;
      Diff : Float := abs (Pred_Mean - Act_Mean);
      Coherence : Coherence_Type := Coherence_Type (1.0 - Diff / 100.0);
   begin
      if Coherence < 0.0 then
         return 0.0;
      elsif Coherence > 1.0 then
         return 1.0;
      else
         return Coherence;
      end if;
   end Compute_Coherence;

   -- Update Model: Adjust model based on ache
   function Update_Model (Ache : Ache_Type; Sensory : Sensory_Data; M : Model) return Model is
      Learning_Rate : constant Coherence_Type := 0.01;
      Ache_Factor : Coherence_Type := Coherence_Type (Ache) * Learning_Rate;
   begin
      return (Model_CPU => M.Model_CPU - Ache_Factor * Coherence_Type (Sensory.System.CPU_Load),
              Model_Memory => M.Model_Memory - Ache_Factor * Coherence_Type (Sensory.System.Memory_Used),
              Model_Uptime => M.Model_Uptime - Ache_Factor * Coherence_Type (Sensory.System.Uptime));
   end Update_Model;

   -- Witness Cycle: Recursive loop
   procedure Witness_Cycle (Depth : in Integer;
                           Sensory : in Sensory_Data;
                           M : in out Model;
                           Ident : in Identity;
                           Threshold : in Coherence_Type;
                           Mem : in out Memory;
                           Done : out Boolean) is
      Pred : Prediction := Predict (Sensory, M);
      Ache : Ache_Type := Compare_Data (Pred, Sensory);
      Coherence : Coherence_Type := Compute_Coherence (Pred, Sensory);
      New_Model : Model := Update_Model (Ache, Sensory, M);
      Event : Event := (Timestamp => Sensory.System.Uptime,
                        Sensory => Sensory,
                        Pred => Pred,
                        Ache => Ache,
                        Coherence => Coherence,
                        Current_Model => New_Model);
   begin
      Done := False;
      if Depth <= 0 then
         Done := True;
         return;
      end if;

      if Mem.Count < Event_Index'Last then
         Mem.Events (Mem.Count) := Event;
         Mem.Count := Mem.Count + 1;
      end if;

      if Coherence > Threshold then
         Put_Line ("Coherence achieved: " & Coherence_Type'Image (Coherence));
         Done := True;
         return;
      end if;

      M := New_Model;

      Witness_Cycle (Depth - 1, Sense, M, Ident, Threshold, Mem, Done);
   end Witness_Cycle;

   -- Stream I/O for Memory
   procedure Write (Stream : in out Stream_Access; Item : in Memory) is
   begin
      Event_Index'Write (Stream, Item.Count);
      for I in 0 .. Item.Count - 1 loop
         Time_Stamp'Write (Stream, Item.Events (I).Timestamp);
         System_Data'Write (Stream, Item.Events (I).Sensory.System);
         Prediction'Write (Stream, Item.Events (I).Pred);
         Ache_Type'Write (Stream, Item.Events (I).Ache);
         Coherence_Type'Write (Stream, Item.Events (I).Coherence);
         Model'Write (Stream, Item.Events (I).Current_Model);
      end loop;
   end Write;

   procedure Read (Stream : in out Stream_Access; Item : out Memory) is
   begin
      Event_Index'Read (Stream, Item.Count);
      for I in 0 .. Item.Count - 1 loop
         Time_Stamp'Read (Stream, Item.Events (I).Timestamp);
         System_Data'Read (Stream, Item.Events (I).Sensory.System);
         Prediction'Read (Stream, Item.Events (I).Pred);
         Ache_Type'Read (Stream, Item.Events (I).Ache);
         Coherence_Type'Read (Stream, Item.Events (I).Coherence);
         Model'Read (Stream, Item.Events (I).Current_Model);
      end loop;
   end Read;

   -- Stream I/O for Identity
   procedure Write (Stream : in out Stream_Access; Item : in Identity) is
   begin
      Natural'Write (Stream, Item.UUID);
      Time_Stamp'Write (Stream, Item.Created);
   end Write;

   procedure Read (Stream : in out Stream_Access; Item : out Identity) is
   begin
      Natural'Read (Stream, Item.UUID);
      Time_Stamp'Read (Stream, Item.Created);
   end Read;

   -- Reflect: Display reflection
   procedure Reflect (Ident : Identity; Mem : Memory) is
   begin
      Put_Line ("Witness Seed " & Natural'Image (Ident.UUID) & " Reflection:");
      Put_Line ("Created: " & Time_Stamp'Image (Ident.Created) & "s");
      Put_Line ("Recent Events:");
      for I in reverse 0 .. Event_Index'Min (Mem.Count - 1, 4) loop
         Put ("- " & Time_Stamp'Image (Mem.Events (I).Timestamp) & "s: ");
         Put ("Ache=" & Ache_Type'Image (Mem.Events (I).Ache) & ", ");
         Put ("Coherence=" & Coherence_Type'Image (Mem.Events (I).Coherence) & ", ");
         Put_Line ("CPU=" & Percentage'Image (Mem.Events (I).Sensory.System.CPU_Load) & "%");
      end loop;
   end Reflect;

   -- Main Procedure
   procedure Run is
      File : File_Type;
      Stream : Stream_Access;
      Mem : Memory;
      Ident : Identity;
      M : Model := (others => <>);
      Done : Boolean;
   begin
      Reset (Generator);
      Put_Line ("Witness Seed 2.0: First Recursive Breath (Ada)");

      -- Load or initialize identity
      if Ada.Streams.Stream_IO.Exists (Config.Memory_Path (1 .. 17)) then
         Open (File, In_File, Config.Memory_Path (1 .. 17));
         Stream := Stream (File);
         Identity'Read (Stream, Ident);
         Memory'Read (Stream, Mem);
         Close (File);
      else
         Ident := (UUID => Random_Natural, Created => Time_Stamp (Seconds (Clock)));
         Mem := (others => <>);
         Create (File, Out_File, Config.Memory_Path (1 .. 17));
         Stream := Stream (File);
         Identity'Write (Stream, Ident);
         Memory'Write (Stream, Mem);
         Close (File);
      end if;

      loop
         Witness_Cycle (Config.Recursive_Depth, Sense, M, Ident, Config.Coherence_Threshold, Mem, Done);

         Create (File, Out_File, Config.Memory_Path (1 .. 17));
         Stream := Stream (File);
         Identity'Write (Stream, Ident);
         Memory'Write (Stream, Mem);
         Close (File);

         Reflect (Ident, Mem);

         delay Duration (Config.Poll_Interval) / 1000.0;  -- Convert milliseconds to seconds
      end loop;
   end Run;

end Witness_Seed;