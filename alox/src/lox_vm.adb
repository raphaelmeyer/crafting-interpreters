with Debug;

with Ada.Text_IO;

package body Lox_VM is
   procedure Init (VM : in out VM_Context) is
   begin
      Reset_Stack (VM);
   end Init;

   function Interpret
     (VM : in out VM_Context; Chunk : Lox_Chunk.Chunk) return InterpretResult
   is
   begin
      VM.Chunk := Chunk;
      VM.IP := Chunk.Code.First;
      return Run (VM);
   end Interpret;

   procedure Push (VM : in out VM_Context; Value : Lox_Value.Value) is
   begin
      VM.Stack (VM.Stack_Top) := Value;
      VM.Stack_Top := Stack_Index'Succ (VM.Stack_Top);
   end Push;

   function Pop (VM : in out VM_Context) return Lox_Value.Value is
   begin
      VM.Stack_Top := Stack_Index'Pred (VM.Stack_Top);
      return VM.Stack (VM.Stack_Top);
   end Pop;

   procedure Reset_Stack (VM : in out VM_Context) is
   begin
      VM.Stack_Top := 0;
   end Reset_Stack;

   function Read_Byte (VM : in out VM_Context) return Lox_Chunk.Byte is
      Result : Lox_Chunk.Byte;
   begin
      Result := Lox_Chunk.Byte_Vectors.Element (VM.IP);
      VM.IP := Lox_Chunk.Byte_Vectors.Next (VM.IP);
      return Result;
   end Read_Byte;

   function Read_Constant (VM : in out VM_Context) return Lox_Value.Value is
      Index : Natural;
   begin
      Index := Natural (Read_Byte (VM));
      return VM.Chunk.Constants (Index);
   end Read_Constant;

   function Run (VM : in out VM_Context) return InterpretResult is
      Instruction : Lox_Chunk.Byte;
      Value       : Lox_Value.Value;
   begin
      loop
         if VM.Trace_Execution then
            Debug.DisassembleInstruction
              (VM.Chunk, Lox_Chunk.Byte_Vectors.To_Index (VM.IP));
         end if;

         Instruction := Read_Byte (VM);
         case Instruction is
            when Lox_Chunk.Op_Constant'Enum_Rep =>
               Value := Read_Constant (VM);
               Lox_Value.Print (Value);
               Ada.Text_IO.New_Line;

            when Lox_Chunk.Op_Return'Enum_Rep   =>
               return Interpret_OK;

            when others                         =>
               return Interpret_Runtime_Error;
         end case;
      end loop;
   end Run;

end Lox_VM;
