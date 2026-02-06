with Ada.Integer_Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Lox_Value;

package body Debug is

   procedure Disassemble_Chunk
     (Chunk : Lox_Chunk.Chunk_Read_Access; Name : String)
   is
      Offset : Natural := Chunk.Code.First_Index;
   begin
      Ada.Text_IO.Put_Line ("== " & Name & " ==");
      while Offset < Natural (Chunk.Code.Length) loop
         Offset := Disassemble_Instruction (Chunk, Offset);
      end loop;
   end Disassemble_Chunk;

   function Disassemble_Instruction
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
         when Lox_Chunk.OP_CONSTANT'Enum_Rep =>
            return Constant_Instruction ("OP_CONSTANT", Chunk, Offset);

         when Lox_Chunk.OP_NIL'Enum_Rep      =>
            return Simple_Instruction ("OP_NIL", Offset);

         when Lox_Chunk.OP_TRUE'Enum_Rep     =>
            return Simple_Instruction ("OP_TRUE", Offset);

         when Lox_Chunk.OP_FALSE'Enum_Rep    =>
            return Simple_Instruction ("OP_FALSE", Offset);

         when Lox_Chunk.OP_EQUAL'Enum_Rep    =>
            return Simple_Instruction ("OP_EQUAL", Offset);

         when Lox_Chunk.OP_GREATER'Enum_Rep  =>
            return Simple_Instruction ("OP_GREATER", Offset);

         when Lox_Chunk.OP_LESS'Enum_Rep     =>
            return Simple_Instruction ("OP_LESS", Offset);

         when Lox_Chunk.OP_ADD'Enum_Rep      =>
            return Simple_Instruction ("OP_ADD", Offset);

         when Lox_Chunk.OP_SUBTRACT'Enum_Rep =>
            return Simple_Instruction ("OP_SUBTRACT", Offset);

         when Lox_Chunk.OP_MULTIPLY'Enum_Rep =>
            return Simple_Instruction ("OP_MULTIPLY", Offset);

         when Lox_Chunk.OP_DIVIDE'Enum_Rep   =>
            return Simple_Instruction ("OP_DIVIDE", Offset);

         when Lox_Chunk.OP_NOT'Enum_Rep      =>
            return Simple_Instruction ("OP_NOT", Offset);

         when Lox_Chunk.OP_NEGATE'Enum_Rep   =>
            return Simple_Instruction ("OP_NEGATE", Offset);

         when Lox_Chunk.OP_PRINT'Enum_Rep    =>
            return Simple_Instruction ("OP_PRINT", Offset);

         when Lox_Chunk.OP_RETURN'Enum_Rep   =>
            return Simple_Instruction ("OP_RETURN", Offset);

         when others                         =>
            Ada.Text_IO.Put ("Unknown opcode ");
            Ada.Integer_Text_IO.Put (Integer (Instruction));
            Ada.Text_IO.New_Line;
            return Offset + 1;
      end case;
   end Disassemble_Instruction;

   procedure Enable_Trace_Execution is
   begin
      Trace_Execution := True;
   end Enable_Trace_Execution;

   procedure Enable_Print_Code is
   begin
      Print_Code := True;
   end Enable_Print_Code;

   function Trace_Execution_Enabled return Boolean is
   begin
      return Trace_Execution;
   end Trace_Execution_Enabled;

   function Print_Code_Enabled return Boolean is
   begin
      return Print_Code;
   end Print_Code_Enabled;

   function Constant_Instruction
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
      Lox_Value.Print_Value (Chunk.Constants (Index));
      Ada.Text_IO.Put_Line ("'");
      return Offset + 2;
   end Constant_Instruction;

   function Simple_Instruction (Name : String; Offset : Natural) return Natural
   is
   begin
      Ada.Text_IO.Put_Line (Name);
      return Offset + 1;
   end Simple_Instruction;

end Debug;
