with AUnit;
with AUnit.Test_Cases;

package Value_Tests is

   type Value_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding
   procedure Register_Tests (T : in out Value_Test);

   overriding
   function Name (T : Value_Test) return AUnit.Message_String;

end Value_Tests;
