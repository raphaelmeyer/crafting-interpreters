with Lox_Chunk;
with Lox_Scanner;
with Lox_Value;

package Lox_VM is
   type InterpretResult is
     (Interpret_OK, Interpret_Compile_Error, Interpret_Runtime_Error);

   type Stack_Index is range 0 .. 255;
   type Stack_Array is array (Stack_Index) of Lox_Value.Value;

   type VM_Context is record
      Trace_Execution : Boolean;
      Chunk           : Lox_Chunk.Chunk;
      IP              : Lox_Chunk.Byte_Vectors.Cursor;
      Stack           : Stack_Array;
      Stack_Top       : Stack_Index;
   end record;

   procedure Init (VM : in out VM_Context);

   function Interpret
     (VM : in out VM_Context; Source : Lox_Scanner.Source_Code)
      return InterpretResult;

   procedure Push (VM : in out VM_Context; Value : Lox_Value.Value);
   function Pop (VM : in out VM_Context) return Lox_Value.Value;

private
   procedure Reset_Stack (VM : in out VM_Context);

   function Read_Byte (VM : in out VM_Context) return Lox_Chunk.Byte;
   function Read_Constant (VM : in out VM_Context) return Lox_Value.Value;
   function Run (VM : in out VM_Context) return InterpretResult;

end Lox_VM;
