with Lox_Chunk;
with Lox_Scanner;
with Lox_Value;

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

   procedure Push (VM : in out VM_Context; Value : Lox_Value.Value);
   function Pop (VM : in out VM_Context) return Lox_Value.Value;
   function Peek
     (VM : in out VM_Context; Distance : Integer) return Lox_Value.Value;
   function Is_Falsey (Value : Lox_Value.Value) return Boolean;

private

   type VM_Context is limited record
      Chunk     : Lox_Chunk.Chunk_Read_Access;
      IP        : Lox_Chunk.Byte_Vectors.Cursor;
      Stack     : Stack_Array;
      Stack_Top : Stack_Index;
   end record;

   procedure Reset_Stack (VM : in out VM_Context);
   procedure Runtime_Error (VM : in out VM_Context; Message : String);

   function Read_Byte (VM : in out VM_Context) return Lox_Chunk.Byte;
   function Read_Constant (VM : in out VM_Context) return Lox_Value.Value;
   function Run (VM : in out VM_Context) return Interpret_Result;

end Lox_VM;
