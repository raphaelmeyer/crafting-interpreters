with Ada.Finalization;

package Value is
   type Value is new Float;

   type Value_Array_Array is array (Natural range <>) of Value;
   type Value_Array_Array_Access is access all Value_Array_Array;

   type Value_Array is new Ada.Finalization.Controlled with record
      Count    : Natural;
      Capacity : Natural;
      Values   : Value_Array_Array_Access;
   end record;

   procedure Init (VA : in out Value_Array);
   procedure Write (VA : in out Value_Array; V : Value);
   procedure Free (VA : in out Value_Array);

   overriding
   procedure Finalize (Obj : in out Value_Array);

   function To_String (Value : Float) return String;

private
   procedure Grow (VA : in out Value_Array);

end Value;
