with Ada.Integer_Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Lox_Value;

package body Debug is

   procedure DisassembleChunk
     (Chunk : Lox_Chunk.Chunk_Read_Access; Name : String)
   is
      Offset : Natural := Chunk.Code.First_Index;
   begin
      Ada.Text_IO.Put_Line ("== " & Name & " ==");
      while Offset < Natural (Chunk.Code.Length) loop
         Offset := DisassembleInstruction (Chunk, Offset);
      end loop;
   end DisassembleChunk;

   function DisassembleInstruction
     (Chunk : Lox_Chunk.Chunk_Read_Access; Offset : Natural) return Natural
   is
      Instruction : Lox_Chunk.Byte;
      Buffer      : String (1 .. 32);
   begin
      Ada.Integer_Text_IO.Put (To => Buffer, Item => Offset);
      Ada.Text_IO.Put
        (Ada.Strings.Fixed.Tail
           (Ada.Strings.Fixed.Trim (Buffer, Ada.Strings.Both), 4, '0')
         & " ");

      if Offset > 0 and then Chunk.Lines (Offset) = Chunk.Lines (Offset - 1)
      then
         Ada.Text_IO.Put ("   | ");
      else
         Ada.Integer_Text_IO.Put (Chunk.Lines (Offset), Width => 4);
         Ada.Text_IO.Put (" ");
      end if;

      Instruction := Chunk.Code (Offset);
      case Instruction is
         when Lox_Chunk.Op_Constant'Enum_Rep =>
            return ConstantInstruction ("OP_CONSTANT", Chunk, Offset);

         when Lox_Chunk.Op_Add'Enum_Rep      =>
            return SimpleInstruction ("OP_ADD", Offset);

         when Lox_Chunk.Op_Subtract'Enum_Rep =>
            return SimpleInstruction ("OP_SUBTRACT", Offset);

         when Lox_Chunk.Op_Multiply'Enum_Rep =>
            return SimpleInstruction ("OP_MULTIPLY", Offset);

         when Lox_Chunk.Op_Divide'Enum_Rep   =>
            return SimpleInstruction ("OP_DIVIDE", Offset);

         when Lox_Chunk.Op_Negate'Enum_Rep   =>
            return SimpleInstruction ("OP_NEGATE", Offset);

         when Lox_Chunk.Op_Return'Enum_Rep   =>
            return SimpleInstruction ("OP_RETURN", Offset);

         when others                         =>
            Ada.Text_IO.Put ("Unknown opcode ");
            Ada.Integer_Text_IO.Put (Integer (Instruction));
            Ada.Text_IO.New_Line;
            return Offset + 1;
      end case;
   end DisassembleInstruction;

   function ConstantInstruction
     (Name : String; Chunk : Lox_Chunk.Chunk_Read_Access; Offset : Natural)
      return Natural
   is
      Byte  : Lox_Chunk.Byte;
      Index : Natural;
   begin
      Byte := Chunk.Code (Offset + 1);
      Index := Natural (Byte);
      Ada.Text_IO.Put (Ada.Strings.Fixed.Head (Name, 16) & " ");
      Ada.Integer_Text_IO.Put (Index, Width => 4);
      Ada.Text_IO.Put (" '");
      Lox_Value.Print (Chunk.Constants (Index));
      Ada.Text_IO.Put_Line ("'");
      return Offset + 2;
   end ConstantInstruction;

   function SimpleInstruction (Name : String; Offset : Natural) return Natural
   is
   begin
      Ada.Text_IO.Put_Line (Name);
      return Offset + 1;
   end SimpleInstruction;

end Debug;
