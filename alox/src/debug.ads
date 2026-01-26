with Lox_Chunk;

package Debug is
   procedure DisassembleChunk
     (Chunk : Lox_Chunk.Chunk_Read_Access; Name : String);

   function DisassembleInstruction
     (Chunk : Lox_Chunk.Chunk_Read_Access; Offset : Natural) return Natural;

private
   function ConstantInstruction
     (Name : String; Chunk : Lox_Chunk.Chunk_Read_Access; Offset : Natural)
      return Natural;
   function SimpleInstruction (Name : String; Offset : Natural) return Natural;

end Debug;
