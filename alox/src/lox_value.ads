with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Lox_Value is
   package Unbounded renames Ada.Strings.Unbounded;
   subtype Unbounded_String is Unbounded.Unbounded_String;

   type Value_Type is (VAL_BOOL, VAL_NIL, VAL_NUMBER, VAL_STRING);

   type Value (Kind : Value_Type := VAL_NIL) is record
      case Kind is
         when VAL_BOOL =>
            Bool_Value : Boolean;

         when VAL_NUMBER =>
            Number_Value : Float;

         when VAL_STRING =>
            String_Value : Unbounded_String;

         when others =>
            null;
      end case;
   end record;

   package Value_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Value);

   subtype Value_Array is Value_Vectors.Vector;

   function Make_Nil return Value;
   function Is_Nil (V : Value) return Boolean;

   function Make_Bool (Bool : Boolean) return Value;
   function Is_Bool (V : Value) return Boolean;

   function Make_Number (Number : Float) return Value;
   function Is_Number (V : Value) return Boolean;

   function Make_String (Str : Unbounded_String) return Value;
   function Is_String (V : Value) return Boolean;

   procedure Write (VA : in out Value_Array; V : Value);

   procedure Print_Value (V : Value);

   function Values_Equal (A : Value; B : Value) return Boolean;

private
   function To_String (V : Float) return String;

end Lox_Value;
