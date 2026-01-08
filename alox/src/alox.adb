with Ada.Text_IO;
with Chunk;
with Debug;

procedure ALox is
   Test : Chunk.Chunk;
begin
   Ada.Text_IO.Put_Line ("alox");
   Chunk.Init (Test);
   Chunk.Write (Test, Chunk.Op_Return);

   Debug.DisassembleChunk (Test, "test chunk");

   Chunk.Free (Test);
end ALox;
