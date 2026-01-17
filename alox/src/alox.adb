with Debug;
with Lox_Chunk;
with Lox_VM;

with Ada.Text_IO;

procedure ALox is
   Chunk  : Lox_Chunk.Chunk;
   Index  : Natural;
   VM     : Lox_VM.VM_Context;
   Result : Lox_VM.InterpretResult;
   use type Lox_VM.InterpretResult;
begin
   Index := Lox_Chunk.Add_Constant (Chunk, 1.2);
   Lox_Chunk.Write (Chunk, Lox_Chunk.Op_Constant, 123);
   Lox_Chunk.Write (Chunk, Lox_Chunk.Byte (Index), 123);

   Lox_Chunk.Write (Chunk, Lox_Chunk.Op_Return, 123);

   Debug.DisassembleChunk (Chunk, "test chunk");

   Result := Lox_VM.Interpret (VM, Chunk);
   if Result /= Lox_VM.Interpret_OK then
      Ada.Text_IO.Put_Line ("error");
   end if;
end ALox;
