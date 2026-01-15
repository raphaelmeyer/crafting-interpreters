with Debug;
with Lox_Chunk;

procedure ALox is
   Chunk : Lox_Chunk.Chunk;
   Index : Natural;
begin
   Index := Lox_Chunk.Add_Constant (Chunk, 1.2);
   Lox_Chunk.Write (Chunk, Lox_Chunk.Op_Constant, 123);
   Lox_Chunk.Write (Chunk, Lox_Chunk.Byte (Index), 123);

   Lox_Chunk.Write (Chunk, Lox_Chunk.Op_Return, 123);

   Debug.DisassembleChunk (Chunk, "test chunk");

end ALox;
