with AUnit.Assertions; use AUnit.Assertions;
with Chunk;
use type Chunk.Op_Code;
use type Chunk.Code_Array_Access;
with Value;
use type Value.Value;

package body Chunk_Tests is

   procedure Test_Empty (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Testee : Chunk.Chunk;
   begin
      Chunk.Init (Testee);

      Assert (Testee.Count = 0, "Should be empty");
      Assert (Testee.Capacity = 0, "Should not have a capacity");
      Assert (Testee.Code = null, "Should not have data");
   end Test_Empty;

   procedure Test_Append (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Testee : Chunk.Chunk;
   begin
      Chunk.Init (Testee);

      Chunk.Write (Testee, Chunk.Op_Return);

      Assert (Testee.Count = 1, "Should contain element");
      Assert (Testee.Code /= null, "Should have data");
      Assert
        (Testee.Code (0) = Chunk.Op_Return, "Should contain appended element");
      Assert (Testee.Capacity >= Testee.Count, "Should have enough capacity");
   end Test_Append;

   procedure Test_Grow (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Testee : Chunk.Chunk;
   begin
      Chunk.Init (Testee);
      Chunk.Write (Testee, Chunk.Op_Constant);

      for I in Testee.Count .. Testee.Capacity - 1 loop
         Chunk.Write (Testee, Chunk.Op_Return);
      end loop;

      Assert (Testee.Capacity >= Testee.Count, "Should have enough capacity");
      Chunk.Write (Testee, Chunk.Op_Constant);

      Assert (Testee.Capacity >= Testee.Count, "Should have enough capacity");

      Assert
        (Testee.Code (Testee.Count - 2) = Chunk.Op_Return,
         "Should contain element");
      Assert
        (Testee.Code (Testee.Count - 1) = Chunk.Op_Constant,
         "Should contain element");
   end Test_Grow;

   procedure Test_Free (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Testee : Chunk.Chunk;
      Index  : Natural;
   begin
      Chunk.Init (Testee);

      for I in 1 .. 13 loop
         Chunk.Write (Testee, Chunk.Op_Return);
         Chunk.Write (Testee, Chunk.Op_Constant);
      end loop;

      Assert (Testee.Count = 26, "Should appended elements");
      Assert (Testee.Capacity >= Testee.Count, "Should have enough capacity");

      Index := Chunk.Add_Constant (Testee, 42.0);
      Assert (Testee.Constants.Count = 1, "Should add constant");

      Chunk.Free (Testee);
      Assert (Testee.Count = 0, "Should be empty");
      Assert (Testee.Capacity = 0, "Should not have a capacity");
      Assert (Testee.Code = null, "Should not have data");

      Assert (Testee.Constants.Count = 0, "Should free constants");
   end Test_Free;

   procedure Test_Add_Constant (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Testee            : Chunk.Chunk;
      Index_Pi, Index_E : Natural;
   begin
      Chunk.Init (Testee);

      Index_E := Chunk.Add_Constant (Testee, 2.718);
      Index_Pi := Chunk.Add_Constant (Testee, 3.1415);

      Assert (Testee.Constants.Count = 2, "Should contain constant");
      Assert (Testee.Constants.Value (Index_E) = 2.718, "Should add constant");
      Assert
        (Testee.Constants.Value (Index_Pi) = 3.1415, "Should add constant");
   end Test_Add_Constant;

   overriding
   procedure Register_Tests (T : in out Chunk_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Empty'Access, "Empty");
      Register_Routine (T, Test_Append'Access, "Append");
      Register_Routine (T, Test_Add_Constant'Access, "Add_Constant");
      Register_Routine (T, Test_Grow'Access, "Grow");
      Register_Routine (T, Test_Free'Access, "Free");
   end Register_Tests;

   overriding
   function Name (T : Chunk_Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Chunk package");
   end Name;

end Chunk_Tests;
