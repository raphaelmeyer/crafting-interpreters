with Ada.Containers.Vectors;

package Lox_Value is
   type Value is new Float;

   package Value_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Value);

   subtype Value_Array is Value_Vectors.Vector;

   procedure Write (VA : in out Value_Array; V : Value);

   procedure Print (V : Value);

private
   function To_String (V : Float) return String;

end Lox_Value;
