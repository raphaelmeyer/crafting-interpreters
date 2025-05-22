with Ada.Text_IO;
with Chunk;
with Debug;

procedure ALox is
   Test  : Chunk.Chunk;
   Index : Natural;
begin
   Ada.Text_IO.Put_Line ("alox");
   Chunk.Init (Test);

   Index := Chunk.Add_Constant (Test, 1.2);
   Chunk.Write (Test, Chunk.Op_Constant, 123);
   Chunk.Write (Test, Chunk.Byte (Index), 123);

   Chunk.Write (Test, Chunk.Op_Return, 123);

   Debug.DisassembleChunk (Test, "test chunk");

   Chunk.Free (Test);
end ALox;
