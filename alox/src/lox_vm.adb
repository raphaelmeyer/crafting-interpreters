with Debug;
with Lox_Compiler;

with Ada.Text_IO;

package body Lox_VM is
   procedure Init (VM : in out VM_Context) is
   begin
      Reset_Stack (VM);
   end Init;

   function Interpret
     (VM     : in out VM_Context;
      Source : Lox_Scanner.Source_Code;
      Chunk  : Lox_Chunk.Chunk_Access) return InterpretResult
   is
      Result : InterpretResult;
   begin
      Lox_Chunk.Init (Chunk.all);

      if not Lox_Compiler.Compile (Source, Chunk) then
         return Interpret_Compile_Error;
      end if;

      VM.Chunk := Lox_Chunk.Chunk_Read_Access (Chunk);
      VM.IP := VM.Chunk.Code.First;

      Result := Run (VM);

      VM.Chunk := null;
      VM.IP := Lox_Chunk.Byte_Vectors.No_Element;

      return Result;
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

   generic
      with
        function Op
          (A : Lox_Value.Value; B : Lox_Value.Value) return Lox_Value.Value;
   procedure Binary_Op (VM : in out VM_Context);

   procedure Binary_Op (VM : in out VM_Context) is
      A : Lox_Value.Value;
      B : Lox_Value.Value;
   begin
      B := Pop (VM);
      A := Pop (VM);
      Push (VM, Op (A, B));
   end Binary_Op;

   procedure Binary_Op_Add is new Binary_Op (Lox_Value."+");
   procedure Binary_Op_Subtract is new Binary_Op (Lox_Value."-");
   procedure Binary_Op_Multiply is new Binary_Op (Lox_Value."*");
   procedure Binary_Op_Divide is new Binary_Op (Lox_Value."/");

   function Run (VM : in out VM_Context) return InterpretResult is
      Instruction : Lox_Chunk.Byte;
      Value       : Lox_Value.Value;
      Unused      : Natural;
      use type Lox_Value.Value;
   begin
      loop
         if Debug.Trace_Execution_Enabled then
            Ada.Text_IO.Put ("          ");
            for V of
              VM.Stack (Stack_Index'First .. Stack_Index'Pred (VM.Stack_Top))
            loop
               Ada.Text_IO.Put ("[ ");
               Lox_Value.Print (V);
               Ada.Text_IO.Put (" ]");
            end loop;
            Ada.Text_IO.New_Line;
            Unused :=
              Debug.DisassembleInstruction
                (VM.Chunk, Lox_Chunk.Byte_Vectors.To_Index (VM.IP));
         end if;

         Instruction := Read_Byte (VM);
         case Instruction is
            when Lox_Chunk.Op_Constant'Enum_Rep =>
               Value := Read_Constant (VM);
               Push (VM, Value);

            when Lox_Chunk.Op_Add'Enum_Rep      =>
               Binary_Op_Add (VM);

            when Lox_Chunk.Op_Subtract'Enum_Rep =>
               Binary_Op_Subtract (VM);

            when Lox_Chunk.Op_Multiply'Enum_Rep =>
               Binary_Op_Multiply (VM);

            when Lox_Chunk.Op_Divide'Enum_Rep   =>
               Binary_Op_Divide (VM);

            when Lox_Chunk.Op_Negate'Enum_Rep   =>
               Push (VM, -Pop (VM));

            when Lox_Chunk.Op_Return'Enum_Rep   =>
               Lox_Value.Print (Pop (VM));
               Ada.Text_IO.New_Line;
               return Interpret_OK;

            when others                         =>
               return Interpret_Runtime_Error;
         end case;
      end loop;
   end Run;

end Lox_VM;
