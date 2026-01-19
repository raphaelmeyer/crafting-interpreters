with Lox_Value;

with Ada.Containers.Vectors;

package Lox_Chunk is
   type Byte is mod 2 ** 8 with Size => 8;
   type Op_Code is (Op_Constant, Op_Negate, Op_Return) with Size => 8;

   package Byte_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Byte);

   package Natural_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Natural);

   type Chunk is record
      Code      : Byte_Vectors.Vector;
      Lines     : Natural_Vectors.Vector;
      Constants : Lox_Value.Value_Array;
   end record;

   procedure Write (C : in out Chunk; B : Byte; Line : Natural);
   procedure Write (C : in out Chunk; O : Op_Code; Line : Natural);

   function Add_Constant
     (C : in out Chunk; Value : Lox_Value.Value) return Natural;

end Lox_Chunk;
