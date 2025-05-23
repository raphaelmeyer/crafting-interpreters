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
      Assert (Testee.Values = null, "Should not have data");
   end Test_Empty;

   procedure Test_Append (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Testee : Value.Value_Array;
   begin
      Value.Init (Testee);

      Value.Write (Testee, 3.14);

      Assert (Testee.Count = 1, "Should contain element");
      Assert (Testee.Values /= null, "Should have data");
      Assert (Testee.Values (0) = 3.14, "Should contain appended element");
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

      Assert
        (Testee.Values (Testee.Count - 2) = 2.0, "Should contain element");
      Assert
        (Testee.Values (Testee.Count - 1) = 3.0, "Should contain element");
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
      Assert (Testee.Values = null, "Should not have data");
   end Test_Free;

   procedure Test_To_String (T : in out AUnit.Test_Cases.Test_Case'Class) is
      procedure Assert_String_Eq (Actual : String; Expected : String) is
      begin
         Assert
           (Actual = Expected,
            "Expected: " & Expected & ", Actual: " & Actual);
      end Assert_String_Eq;
   begin
      Assert_String_Eq (Value.To_String (23.0), "23");
      Assert_String_Eq (Value.To_String (2.3), "2.3");
      Assert_String_Eq (Value.To_String (0.023), "0.023");
      Assert_String_Eq (Value.To_String (1234567.8), "1.23457E+06");
      Assert_String_Eq (Value.To_String (-456789123.456), "-4.56789E+08");
      Assert_String_Eq (Value.To_String (0.0000000123), "1.23000E-08");
      Assert_String_Eq (Value.To_String (-0.00000004567), "-4.56700E-08");
      Assert_String_Eq (Value.To_String (0.0), "0");
   end Test_To_String;

   overriding
   procedure Register_Tests (T : in out Value_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Empty'Access, "Empty");
      Register_Routine (T, Test_Append'Access, "Append");
      Register_Routine (T, Test_Grow'Access, "Grow");
      Register_Routine (T, Test_Free'Access, "Free");
      Register_Routine (T, Test_To_String'Access, "To String");
   end Register_Tests;

   overriding
   function Name (T : Value_Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Value_Array package");
   end Name;

end Value_Tests;
