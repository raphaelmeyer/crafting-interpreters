with Ada.Float_Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Lox_Value is
   function Make_Nil return Value is
   begin
      return (Kind => VAL_NIL);
   end Make_Nil;

   function Is_Nil (V : Value) return Boolean is
   begin
      return V.Kind = VAL_NIL;
   end Is_Nil;

   function Make_Bool (Bool : Boolean) return Value is
   begin
      return (VAL_BOOL, Bool_Value => Bool);
   end Make_Bool;

   function Is_Bool (V : Value) return Boolean is
   begin
      return V.Kind = VAL_BOOL;
   end Is_Bool;

   function Make_Number (Number : Float) return Value is
   begin
      return (VAL_NUMBER, Number_Value => Number);
   end Make_Number;

   function Is_Number (V : Value) return Boolean is
   begin
      return V.Kind = VAL_NUMBER;
   end Is_Number;

   function Make_String (Str : Unbounded_String) return Value is
   begin
      return (VAL_STRING, String_Value => Str);
   end Make_String;

   function Is_String (V : Value) return Boolean is
   begin
      return V.Kind = VAL_STRING;
   end Is_String;

   procedure Write (VA : in out Value_Array; V : Value) is
   begin
      VA.Append (V);
   end Write;

   procedure Print_Value (V : Value) is
   begin
      case V.Kind is
         when VAL_BOOL   =>
            Ada.Text_IO.Put ((if V.Bool_Value then "true" else "false"));

         when VAL_NUMBER =>
            Ada.Text_IO.Put (To_String (V.Number_Value));

         when VAL_STRING =>
            Ada.Text_IO.Put (Unbounded.To_String (V.String_Value));

         when others     =>
            Ada.Text_IO.Put ("nil");
      end case;
   end Print_Value;

   function Values_Equal (A : Value; B : Value) return Boolean is
      use type Unbounded.Unbounded_String;
   begin
      if A.Kind /= B.Kind then
         return False;
      end if;
      case A.Kind is
         when VAL_BOOL   =>
            return A.Bool_Value = B.Bool_Value;

         when VAL_NIL    =>
            return True;

         when VAL_NUMBER =>
            return A.Number_Value = B.Number_Value;

         when VAL_STRING =>
            return A.String_Value = B.String_Value;
      end case;
   end Values_Equal;

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
