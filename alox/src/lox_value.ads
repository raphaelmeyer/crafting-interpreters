with Ada.Containers.Vectors;

package Lox_Value is
   type Value_Type is (VAL_BOOL, VAL_NIL, VAL_NUMBER);

   type Value (Kind : Value_Type := VAL_NIL) is record
      case Kind is
         when VAL_BOOL =>
            Bool_Value : Boolean;

         when VAL_NUMBER =>
            Number_Value : Float;

         when others =>
            null;
      end case;
   end record;

   package Value_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Value);

   subtype Value_Array is Value_Vectors.Vector;

   function Make_Number (Number : Float) return Value;
   function Is_Number (V : Value) return Boolean;

   procedure Write (VA : in out Value_Array; V : Value);

   procedure Print (V : Value);

private
   function To_String (V : Float) return String;

end Lox_Value;
