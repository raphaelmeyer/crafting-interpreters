with Chunk_Tests;

package body Alox_Suite is
   use AUnit.Test_Suites;

   Result           : aliased Test_Suite;
   Chunk_Test_Cases : aliased Chunk_Tests.Chunk_Test;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Chunk_Test_Cases'Access);

      return Result'Access;
   end Suite;

end Alox_Suite;
