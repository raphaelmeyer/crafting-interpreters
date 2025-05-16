with AUnit;
with AUnit.Test_Cases;

package Chunk_Tests is

   type Chunk_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding
   procedure Register_Tests (T : in out Chunk_Test);

   overriding
   function Name (T : Chunk_Test) return AUnit.Message_String;

end Chunk_Tests;
