with Lox_Chunk;

package Debug is
   procedure DisassembleChunk (Chunk : Lox_Chunk.Chunk; Name : String);

   procedure DisassembleInstruction
     (Chunk : Lox_Chunk.Chunk; Offset : Natural);

private
   function DisassembleInstruction
     (Chunk : Lox_Chunk.Chunk; Offset : Natural) return Natural;

   function ConstantInstruction
     (Name : String; Chunk : Lox_Chunk.Chunk; Offset : Natural) return Natural;
   function SimpleInstruction (Name : String; Offset : Natural) return Natural;

end Debug;
