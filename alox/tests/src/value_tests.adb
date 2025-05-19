with AUnit.Assertions; use AUnit.Assertions;
with Value;
use type Value.Value;
use type Value.Value_Array_Array_Access;

package body Value_Tests is

   procedure Test_Empty (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Testee : Value.Value_Array;
   begin
      Value.Init (Testee);

      Assert (Testee.Count = 0, "Should be empty");
      Assert (Testee.Capacity = 0, "Should not have a capacity");
      Assert (Testee.Value = null, "Should not have data");
   end Test_Empty;

   procedure Test_Append (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Testee : Value.Value_Array;
   begin
      Value.Init (Testee);

      Value.Write (Testee, 3.14);

      Assert (Testee.Count = 1, "Should contain element");
      Assert (Testee.Value /= null, "Should have data");
      Assert (Testee.Value (0) = 3.14, "Should contain appended element");
      Assert (Testee.Capacity >= Testee.Count, "Should have enough capacity");
   end Test_Append;

   procedure Test_Grow (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Testee : Value.Value_Array;
   begin
      Value.Init (Testee);
      Value.Write (Testee, 1.0);

      for I in Testee.Count .. Testee.Capacity - 1 loop
         Value.Write (Testee, 2.0);
      end loop;

      Assert (Testee.Capacity >= Testee.Count, "Should have enough capacity");
      Value.Write (Testee, 3.0);

      Assert (Testee.Capacity >= Testee.Count, "Should have enough capacity");

      Assert (Testee.Value (Testee.Count - 2) = 2.0, "Should contain element");
      Assert (Testee.Value (Testee.Count - 1) = 3.0, "Should contain element");
   end Test_Grow;

   procedure Test_Free (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Testee : Value.Value_Array;
   begin
      Value.Init (Testee);

      for I in 1 .. 42 loop
         Value.Write (Testee, Value.Value (Float (I) * 3.14));
      end loop;

      Assert (Testee.Count = 42, "Should appended elements");
      Assert (Testee.Capacity >= Testee.Count, "Should have enough capacity");

      Value.Free (Testee);
      Assert (Testee.Count = 0, "Should be empty");
      Assert (Testee.Capacity = 0, "Should not have a capacity");
      Assert (Testee.Value = null, "Should not have data");
   end Test_Free;

   overriding
   procedure Register_Tests (T : in out Value_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Empty'Access, "Empty");
      Register_Routine (T, Test_Append'Access, "Append");
      Register_Routine (T, Test_Grow'Access, "Grow");
      Register_Routine (T, Test_Free'Access, "Free");
   end Register_Tests;

   overriding
   function Name (T : Value_Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Value_Array package");
   end Name;

end Value_Tests;
