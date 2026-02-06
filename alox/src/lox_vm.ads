with Lox_Chunk;
with Lox_Scanner;
with Lox_Value;

with Ada.Containers.Hashed_Maps;

package Lox_VM is
   type VM_Context is limited private;

   type Interpret_Result is
     (INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR);

   type Stack_Index is range 0 .. 255;
   type Stack_Array is array (Stack_Index) of Lox_Value.Value;

   procedure Init (VM : in out VM_Context);

   function Interpret
     (VM     : in out VM_Context;
      Source : Lox_Scanner.Source_Code;
      Chunk  : Lox_Chunk.Chunk_Access) return Interpret_Result;

private
   function Hash_String
     (Key : Lox_Value.Unbounded_String) return Ada.Containers.Hash_Type;

   package Hash_Table is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Lox_Value.Unbounded_String,
        Element_Type    => Lox_Value.Value,
        Hash            => Hash_String,
        Equivalent_Keys => Lox_Value.Unbounded."=",
        "="             => Lox_Value.Values_Equal);

   type VM_Context is limited record
      Chunk     : Lox_Chunk.Chunk_Read_Access;
      IP        : Lox_Chunk.Byte_Vectors.Cursor;
      Stack     : Stack_Array;
      Stack_Top : Stack_Index;
      Globals   : Hash_Table.Map;
   end record;

   procedure Push (VM : in out VM_Context; Value : Lox_Value.Value);
   function Pop (VM : in out VM_Context) return Lox_Value.Value;
   function Peek
     (VM : in out VM_Context; Distance : Integer) return Lox_Value.Value;

   function Is_Falsey (Value : Lox_Value.Value) return Boolean;
   procedure Concatenate (VM : in out VM_Context);

   procedure Reset_Stack (VM : in out VM_Context);
   procedure Runtime_Error (VM : in out VM_Context; Message : String);

   function Read_Byte (VM : in out VM_Context) return Lox_Chunk.Byte;
   function Read_Constant (VM : in out VM_Context) return Lox_Value.Value;
   function Read_String
     (VM : in out VM_Context) return Lox_Value.Unbounded_String;
   function Run (VM : in out VM_Context) return Interpret_Result;

end Lox_VM;
