with Lox_Chunk;

package Debug is
   procedure Disassemble_Chunk
     (Chunk : Lox_Chunk.Chunk_Read_Access; Name : String);

   function Disassemble_Instruction
     (Chunk : Lox_Chunk.Chunk_Read_Access; Offset : Natural) return Natural;

   procedure Enable_Trace_Execution;
   procedure Enable_Print_Code;

   function Trace_Execution_Enabled return Boolean;
   function Print_Code_Enabled return Boolean;

private
   Print_Code      : Boolean := False;
   Trace_Execution : Boolean := False;

   function Constant_Instruction
     (Name : String; Chunk : Lox_Chunk.Chunk_Read_Access; Offset : Natural)
      return Natural;
   function Simple_Instruction
     (Name : String; Offset : Natural) return Natural;
   function Byte_Instruction
     (Name : String; Chunk : Lox_Chunk.Chunk_Read_Access; Offset : Natural)
      return Natural;
end Debug;
