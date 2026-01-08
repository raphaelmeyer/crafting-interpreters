with Ada.Text_IO;
with Ada.Integer_Text_IO;

package body Debug is

   procedure DisassembleChunk (C : Chunk.Chunk; Name : String) is
      Offset : Natural := 0;
   begin
      Ada.Text_IO.Put_Line ("== " & Name & " ==");
      while Offset < C.Count loop
         Offset := DisassembleInstruction (C, Offset);
      end loop;
   end DisassembleChunk;

   function DisassembleInstruction
     (C : Chunk.Chunk; Offset : Natural) return Natural
   is
      Instruction : Chunk.Op_Code;
   begin
      Ada.Integer_Text_IO.Put (Offset, Width => 4);
      Ada.Text_IO.Put (" ");

      Instruction := C.Code (Offset);
      case Instruction is
         when Chunk.Op_Return =>
            return SimpleInstruction ("OP_RETURN", Offset);

         when others =>
            Ada.Text_IO.Put_Line
              ("Unknown opcode " & Chunk.Op_Code'Image (Instruction));
            return Offset + 1;
      end case;
   end DisassembleInstruction;

   function SimpleInstruction (Name : String; Offset : Natural) return Natural
   is
   begin
      Ada.Text_IO.Put_Line (Name);
      return Offset + 1;
   end SimpleInstruction;

end Debug;
