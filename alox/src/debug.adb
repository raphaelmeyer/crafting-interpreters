with Ada.Integer_Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;

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
      Instruction : Chunk.Byte;
      Buffer      : String (1 .. 32);
   begin
      Ada.Integer_Text_IO.Put (To => Buffer, Item => Offset);
      Ada.Text_IO.Put
        (Ada.Strings.Fixed.Tail
           (Ada.Strings.Fixed.Trim (Buffer, Ada.Strings.Both), 4, '0')
         & " ");

      if Offset > 0 and then C.Lines (Offset) = C.Lines (Offset - 1) then
         Ada.Text_IO.Put ("   | ");
      else
         Ada.Integer_Text_IO.Put (C.Lines (Offset), Width => 4);
         Ada.Text_IO.Put (" ");
      end if;

      Instruction := C.Code (Offset);
      case Instruction is
         when Chunk.Op_Constant'Enum_Rep =>
            return ConstantInstruction ("OP_CONSTANT", C, Offset);

         when Chunk.Op_Return'Enum_Rep =>
            return SimpleInstruction ("OP_RETURN", Offset);

         when others =>
            Ada.Text_IO.Put ("Unknown opcode ");
            Ada.Integer_Text_IO.Put (Integer (Instruction));
            Ada.Text_IO.New_Line;
            return Offset + 1;
      end case;
   end DisassembleInstruction;

   function ConstantInstruction
     (Name : String; C : Chunk.Chunk; Offset : Natural) return Natural
   is
      Index : Natural;
   begin
      Index := Natural (C.Code (Offset + 1));
      Ada.Text_IO.Put (Ada.Strings.Fixed.Head (Name, 16) & " ");
      Ada.Integer_Text_IO.Put (Index, Width => 4);
      Ada.Text_IO.Put (" '");
      PrintValue (C.Constants.Values (Index));
      Ada.Text_IO.Put_Line ("'");
      return Offset + 2;
   end ConstantInstruction;

   function SimpleInstruction (Name : String; Offset : Natural) return Natural
   is
   begin
      Ada.Text_IO.Put_Line (Name);
      return Offset + 1;
   end SimpleInstruction;

   procedure PrintValue (V : Value.Value) is
      Buffer : String (1 .. 32);
   begin
      Ada.Text_IO.Put (Value.To_String (Float (V)));
   end PrintValue;

end Debug;
