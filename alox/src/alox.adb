with Ada.Text_IO;
with Chunk;

procedure ALox is
   Test : Chunk.Chunk;
begin
   Ada.Text_IO.Put_Line ("alox");
   Chunk.Init (Test);
   Chunk.Write (Test, Chunk.Op_Return);

   Ada.Text_IO.Put_Line (Chunk.Op_Code'Image (Test.Code (0)));

   Chunk.Free (Test);
end ALox;
