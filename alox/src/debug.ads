with Chunk;

package Debug is

   procedure DisassembleChunk (C : Chunk.Chunk; Name : String);

private
   function DisassembleInstruction
     (C : Chunk.Chunk; Offset : Natural) return Natural;

   function SimpleInstruction (Name : String; Offset : Natural) return Natural;

end Debug;
