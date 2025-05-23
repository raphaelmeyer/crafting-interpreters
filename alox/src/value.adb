with Ada.Float_Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

package body Value is
   procedure Free is new
     Ada.Unchecked_Deallocation (Value_Array_Array, Value_Array_Array_Access);

   procedure Init (VA : in out Value_Array) is
   begin
      VA.Capacity := 0;
      VA.Count := 0;
      VA.Values := null;
   end Init;

   procedure Write (VA : in out Value_Array; V : Value) is
   begin
      if VA.Capacity < VA.Count + 1 then
         Grow (VA);
      end if;
      VA.Values (VA.Count) := V;
      VA.Count := VA.Count + 1;
   end Write;

   procedure Free (VA : in out Value_Array) is
   begin
      if VA.Values /= null then
         Free (VA.Values);
      end if;
      Init (VA);
   end Free;

   overriding
   procedure Finalize (Obj : in out Value_Array) is
   begin
      Free (Obj);
   end Finalize;

   function To_String (Value : Float) return String is
      Buffer : String (1 .. 32);

      function Trim_Trailing (S : String; C : Character) return String is
         I : Natural;
      begin
         for I in reverse S'Range loop
            if S (I) /= C then
               return S (S'First .. I);
            end if;
         end loop;
         return S;
      end Trim_Trailing;

   begin
      if Value = 0.0 then
         return "0";
      elsif abs Value < 1.0E-4 or 1.0E+6 <= abs Value then
         Ada.Float_Text_IO.Put (To => Buffer, Item => Value);
         return Ada.Strings.Fixed.Trim (Buffer, Ada.Strings.Both);
      else
         Ada.Float_Text_IO.Put
           (To => Buffer, Item => Value, Aft => 6, Exp => 0);
         return
           Trim_Trailing
             (Trim_Trailing
                (Ada.Strings.Fixed.Trim (Buffer, Ada.Strings.Both), '0'),
              '.');
      end if;
   end To_String;

   procedure Grow (VA : in out Value_Array) is
      Old_Code : Value_Array_Array_Access := VA.Values;
   begin
      VA.Capacity := (if VA.Capacity < 8 then 8 else VA.Capacity * 2);
      VA.Values := new Value_Array_Array (0 .. VA.Capacity - 1);
      if Old_Code /= null then
         VA.Values (Old_Code'Range) := Old_Code.all (Old_Code'Range);
      end if;
      Free (Old_Code);
   end Grow;

end Value;
