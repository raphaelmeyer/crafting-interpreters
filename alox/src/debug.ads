with Lox_Chunk;

package Debug is
   procedure DisassembleChunk
     (Chunk : Lox_Chunk.Chunk_Read_Access; Name : String);

   function DisassembleInstruction
     (Chunk : Lox_Chunk.Chunk_Read_Access; Offset : Natural) return Natural;

   procedure Enable_Trace_Execution;
   procedure Enable_Print_Code;

   function Trace_Execution_Enabled return Boolean;
   function Print_Code_Enabled return Boolean;

private
   Print_Code      : Boolean := False;
   Trace_Execution : Boolean := False;

   function ConstantInstruction
     (Name : String; Chunk : Lox_Chunk.Chunk_Read_Access; Offset : Natural)
      return Natural;
   function SimpleInstruction (Name : String; Offset : Natural) return Natural;

end Debug;
