with Lox_Value;
with Lox_Types; use Lox_Types;

with Ada.Containers.Vectors;

package Lox_Chunk is
   type Op_Code is
     (OP_CONSTANT,
      OP_NIL,
      OP_TRUE,
      OP_FALSE,
      OP_POP,
      OP_GET_LOCAL,
      OP_SET_LOCAL,
      OP_GET_GLOBAL,
      OP_DEFINE_GLOBAL,
      OP_SET_GLOBAL,
      OP_EQUAL,
      OP_GREATER,
      OP_LESS,
      OP_ADD,
      OP_SUBTRACT,
      OP_MULTIPLY,
      OP_DIVIDE,
      OP_NOT,
      OP_NEGATE,
      OP_PRINT,
      OP_JUMP,
      OP_JUMP_IF_FALSE,
      OP_LOOP,
      OP_RETURN)
   with Size => 8;

   package Byte_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Byte);

   package Natural_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Natural);

   type Chunk is limited record
      Code      : Byte_Vectors.Vector;
      Lines     : Natural_Vectors.Vector;
      Constants : Lox_Value.Value_Array;
   end record;

   type Chunk_Access is access all Chunk;
   type Chunk_Read_Access is access constant Chunk;

   procedure Init (C : in out Chunk);

   procedure Write (C : in out Chunk; B : Byte; Line : Natural);
   procedure Write (C : in out Chunk; O : Op_Code; Line : Natural);

   function Add_Constant
     (C : in out Chunk; Value : Lox_Value.Value) return Natural;

end Lox_Chunk;
