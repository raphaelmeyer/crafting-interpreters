with AUnit.Assertions; use AUnit.Assertions;
with Chunk;            use Chunk;

package body Chunk_Tests is

   procedure Test_Empty (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Testee : Chunk.Chunk;
   begin
      Chunk.Init (Testee);

      Assert (Testee.Count = 0, "Should be empty");
   end Test_Empty;

   procedure Test_Append (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Testee : Chunk.Chunk;
   begin
      Chunk.Init (Testee);

      Chunk.Write (Testee, Chunk.Op_Return);

      Assert (Testee.Count = 1, "Should contain element");
      Assert
        (Testee.Code (0) = Chunk.Op_Return, "Should contain appended element");
      Assert (Testee.Capacity >= Testee.Count, "Should have enough capacity");
   end Test_Append;

   procedure Test_Grow (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Testee : Chunk.Chunk;
   begin
      Chunk.Init (Testee);
      Chunk.Write (Testee, Op_Constant);

      for I in Testee.Count .. Testee.Capacity - 1 loop
         Chunk.Write (Testee, Chunk.Op_Return);
      end loop;

      Assert (Testee.Capacity >= Testee.Count, "Should have enough capacity");
      Chunk.Write (Testee, Op_Constant);

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
   begin
      Chunk.Init (Testee);

      for I in 1 .. 13 loop
         Chunk.Write (Testee, Chunk.Op_Return);
         Chunk.Write (Testee, Op_Constant);
      end loop;

      Assert (Testee.Count = 26, "Should appended elements");
      Assert (Testee.Capacity >= Testee.Count, "Should have enough capacity");

      Chunk.Free (Testee);
      Assert (Testee.Count = 0, "Should be empty");
   end Test_Free;

   overriding
   procedure Register_Tests (T : in out Chunk_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Empty'Access, "Empty");
      Register_Routine (T, Test_Append'Access, "Append");
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
