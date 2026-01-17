with Lox_Chunk;
with Lox_Value;

package Lox_VM is
   type InterpretResult is
     (Interpret_OK, Interpret_Compile_Error, Interpret_Runtime_Error);

   type VM_Context is record
      Chunk : Lox_Chunk.Chunk;
      IP    : Lox_Chunk.Byte_Vectors.Cursor;
   end record;

   function Interpret
     (VM : in out VM_Context; Chunk : Lox_Chunk.Chunk) return InterpretResult;

private
   function Read_Byte (VM : in out VM_Context) return Lox_Chunk.Byte;
   function Read_Constant (VM : in out VM_Context) return Lox_Value.Value;
   function Run (VM : in out VM_Context) return InterpretResult;

end Lox_VM;
