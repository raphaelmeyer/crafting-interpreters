with AUnit.Assertions; use AUnit.Assertions;
with Chunk;

package body Chunk_Tests is

   procedure Test_Empty_Chunk (T : in out AUnit.Test_Cases.Test_Case'Class) is

      Testee : Chunk.Chunk;
   begin
      Chunk.Init (Testee);

      Assert (Testee.Count = 0, "Should be empty");
      Assert (Testee.Capacity = 0, "Should have capacity of 0");
   end Test_Empty_Chunk;

   overriding
   procedure Register_Tests (T : in out Chunk_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Empty_Chunk'Access, "Empty chunk");
   end Register_Tests;

   overriding
   function Name (T : Chunk_Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Chunk package");
   end Name;

end Chunk_Tests;
