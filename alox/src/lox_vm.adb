with Debug;
with Lox_Compiler;

with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Lox_VM is
   procedure Init (VM : in out VM_Context) is
   begin
      Reset_Stack (VM);
   end Init;

   function Interpret
     (VM     : in out VM_Context;
      Source : Lox_Scanner.Source_Code;
      Chunk  : Lox_Chunk.Chunk_Access) return Interpret_Result
   is
      Result : Interpret_Result;
   begin
      Lox_Chunk.Init (Chunk.all);

      if not Lox_Compiler.Compile (Source, Chunk) then
         return INTERPRET_COMPILE_ERROR;
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

   function Peek
     (VM : in out VM_Context; Distance : Integer) return Lox_Value.Value
   is
      Peek_Index : constant Integer := Integer (VM.Stack_Top) - Distance - 1;
   begin
      if Peek_Index < 0 then
         return (Kind => Lox_Value.VAL_NIL);
      end if;
      return VM.Stack (Stack_Index (Peek_Index));
   end Peek;

   procedure Reset_Stack (VM : in out VM_Context) is
   begin
      VM.Stack_Top := 0;
   end Reset_Stack;

   procedure Runtime_Error (VM : in out VM_Context; Message : String) is
      Index  : constant Natural := Lox_Chunk.Byte_Vectors.To_Index (VM.IP);
      Line   : constant Natural := VM.Chunk.Lines (Index);
      Buffer : String (1 .. 8);
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Message);

      Ada.Integer_Text_IO.Put (To => Buffer, Item => Line);
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "[line "
         & Ada.Strings.Fixed.Trim (Buffer, Ada.Strings.Both)
         & "] in script");

      Reset_Stack (VM);
   end Runtime_Error;

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
      with function Op (A : Float; B : Float) return Float;
   function Binary_Op (VM : in out VM_Context) return Interpret_Result;

   function Binary_Op (VM : in out VM_Context) return Interpret_Result is
   begin
      if not Lox_Value.Is_Number (Peek (VM, 0))
        or else not Lox_Value.Is_Number (Peek (VM, 1))
      then
         Runtime_Error (VM, "Operands must be numbers.");
         return INTERPRET_RUNTIME_ERROR;
      end if;

      declare
         B      : constant Float := Pop (VM).Number_Value;
         A      : constant Float := Pop (VM).Number_Value;
         Result : constant Float := Op (A, B);
      begin
         Push (VM, Lox_Value.Make_Number (Result));
         return INTERPRET_OK;
      end;
   end Binary_Op;

   function Binary_Op_Add is new Binary_Op ("+");
   function Binary_Op_Subtract is new Binary_Op ("-");
   function Binary_Op_Multiply is new Binary_Op ("*");
   function Binary_Op_Divide is new Binary_Op ("/");

   function Run (VM : in out VM_Context) return Interpret_Result is
      Instruction : Lox_Chunk.Byte;
      Value       : Lox_Value.Value;
      Unused      : Natural;
      Result      : Interpret_Result;
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
            when Lox_Chunk.OP_CONSTANT'Enum_Rep =>
               Value := Read_Constant (VM);
               Push (VM, Value);

            when Lox_Chunk.OP_NIL'Enum_Rep      =>
               Push (VM, Lox_Value.Make_Nil);

            when Lox_Chunk.OP_TRUE'Enum_Rep     =>
               Push (VM, Lox_Value.Make_Bool (True));

            when Lox_Chunk.OP_FALSE'Enum_Rep    =>
               Push (VM, Lox_Value.Make_Bool (False));

            when Lox_Chunk.OP_ADD'Enum_Rep      =>
               Result := Binary_Op_Add (VM);
               if Result /= INTERPRET_OK then
                  return Result;
               end if;

            when Lox_Chunk.OP_SUBTRACT'Enum_Rep =>
               Result := Binary_Op_Subtract (VM);
               if Result /= INTERPRET_OK then
                  return Result;
               end if;

            when Lox_Chunk.OP_MULTIPLY'Enum_Rep =>
               Result := Binary_Op_Multiply (VM);
               if Result /= INTERPRET_OK then
                  return Result;
               end if;

            when Lox_Chunk.OP_DIVIDE'Enum_Rep   =>
               Result := Binary_Op_Divide (VM);
               if Result /= INTERPRET_OK then
                  return Result;
               end if;

            when Lox_Chunk.OP_NEGATE'Enum_Rep   =>
               if not Lox_Value.Is_Number (Peek (VM, 0)) then
                  Runtime_Error (VM, "Operand must be a number.");
                  return INTERPRET_RUNTIME_ERROR;
               end if;

               declare
                  Value : constant Float := Pop (VM).Number_Value;
               begin
                  Push (VM, Lox_Value.Make_Number (-Value));
               end;

            when Lox_Chunk.OP_RETURN'Enum_Rep   =>
               Lox_Value.Print (Pop (VM));
               Ada.Text_IO.New_Line;
               return INTERPRET_OK;

            when others                         =>
               return INTERPRET_RUNTIME_ERROR;
         end case;
      end loop;
   end Run;

end Lox_VM;
