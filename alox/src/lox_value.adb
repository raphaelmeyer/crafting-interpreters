with Ada.Float_Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Lox_Value is

   procedure Write (VA : in out Value_Array; V : Value) is
   begin
      VA.Append (V);
   end Write;

   procedure Print (V : Value) is
   begin
      Ada.Text_IO.Put (To_String (Float (V)));
   end Print;

   function To_String (V : Float) return String is
      Buffer : String (1 .. 32);

      function Trim_Trailing (S : String; C : Character) return String is
      begin
         for I in reverse S'Range loop
            if S (I) /= C then
               return S (S'First .. I);
            end if;
         end loop;
         return S;
      end Trim_Trailing;

   begin
      if V = 0.0 then
         return "0";
      elsif abs V < 1.0E-4 or else 1.0E+6 <= abs V then
         Ada.Float_Text_IO.Put (To => Buffer, Item => V);
         return Ada.Strings.Fixed.Trim (Buffer, Ada.Strings.Both);
      else
         Ada.Float_Text_IO.Put (To => Buffer, Item => V, Aft => 6, Exp => 0);
         return
           Trim_Trailing
             (Trim_Trailing
                (Ada.Strings.Fixed.Trim (Buffer, Ada.Strings.Both), '0'),
              '.');
      end if;
   end To_String;

end Lox_Value;
