with Debug;
with Lox_Compiler;

with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Lox_VM is
   use type Lox_Value.Lox_Float;

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

   function Hash_String
     (Key : Lox_Value.Unbounded_String) return Ada.Containers.Hash_Type
   is
      use type Ada.Containers.Hash_Type;
      Hash : Ada.Containers.Hash_Type := 2166136261;
      Char : Character;
   begin
      for Index in 1 .. Lox_Value.Unbounded.Length (Key) loop
         Char := Lox_Value.Unbounded.Element (Key, Index);
         Hash := Hash xor Ada.Containers.Hash_Type (Character'Pos (Char));
         Hash := Hash * 16777619;
      end loop;
      return Hash;
   end Hash_String;

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

   function Is_Falsey (Value : Lox_Value.Value) return Boolean is
   begin
      return
        Lox_Value.Is_Nil (Value)
        or else (Lox_Value.Is_Bool (Value) and then not Value.Bool_Value);
   end Is_Falsey;

   procedure Concatenate (VM : in out VM_Context) is
      B : constant String :=
        Lox_Value.Unbounded.To_String (Pop (VM).String_Value);
      A : constant String :=
        Lox_Value.Unbounded.To_String (Pop (VM).String_Value);
   begin
      Push
        (VM,
         Lox_Value.Make_String
           (Lox_Value.Unbounded.To_Unbounded_String (A & B)));
   end Concatenate;

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
      Result : constant Lox_Chunk.Byte :=
        Lox_Chunk.Byte_Vectors.Element (VM.IP);
   begin
      VM.IP := Lox_Chunk.Byte_Vectors.Next (VM.IP);
      return Result;
   end Read_Byte;

   function Read_Short (VM : in out VM_Context) return Short is
      High : Lox_Chunk.Byte;
      Low  : Lox_Chunk.Byte;
   begin
      High := Lox_Chunk.Byte_Vectors.Element (VM.IP);
      VM.IP := Lox_Chunk.Byte_Vectors.Next (VM.IP);
      Low := Lox_Chunk.Byte_Vectors.Element (VM.IP);
      VM.IP := Lox_Chunk.Byte_Vectors.Next (VM.IP);
      return (Short (High) * 256 + Short (Low));
   end Read_Short;

   function Read_Constant (VM : in out VM_Context) return Lox_Value.Value is
      Index : constant Natural := Natural (Read_Byte (VM));
   begin
      return VM.Chunk.Constants (Index);
   end Read_Constant;

   function Read_String
     (VM : in out VM_Context) return Lox_Value.Unbounded_String is
   begin
      return Read_Constant (VM).String_Value;
   end Read_String;

   generic
      type Result_Type is private;
      with
        function Op
          (A : Lox_Value.Lox_Float; B : Lox_Value.Lox_Float)
           return Result_Type;
      with function Make (R : Result_Type) return Lox_Value.Value;
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
         B      : constant Lox_Value.Lox_Float := Pop (VM).Number_Value;
         A      : constant Lox_Value.Lox_Float := Pop (VM).Number_Value;
         Result : constant Result_Type := Op (A, B);
      begin
         Push (VM, Make (Result));
         return INTERPRET_OK;
      end;
   end Binary_Op;

   function Binary_Op_Greater is new
     Binary_Op (Boolean, ">", Lox_Value.Make_Bool);
   function Binary_Op_Less is new
     Binary_Op (Boolean, "<", Lox_Value.Make_Bool);
   function Binary_Op_Subtract is new
     Binary_Op (Lox_Value.Lox_Float, "-", Lox_Value.Make_Number);
   function Binary_Op_Multiply is new
     Binary_Op (Lox_Value.Lox_Float, "*", Lox_Value.Make_Number);
   function Binary_Op_Divide is new
     Binary_Op (Lox_Value.Lox_Float, "/", Lox_Value.Make_Number);

   function Run (VM : in out VM_Context) return Interpret_Result is
      Instruction : Lox_Chunk.Byte;
      Result      : Interpret_Result;
   begin
      loop
         if Debug.Trace_Execution_Enabled then
            Ada.Text_IO.Put ("          ");
            for V of
              VM.Stack (Stack_Index'First .. Stack_Index'Pred (VM.Stack_Top))
            loop
               Ada.Text_IO.Put ("[ ");
               Lox_Value.Print_Value (V);
               Ada.Text_IO.Put (" ]");
            end loop;
            Ada.Text_IO.New_Line;
            declare
               Unused : Natural :=
                 Debug.Disassemble_Instruction
                   (VM.Chunk, Lox_Chunk.Byte_Vectors.To_Index (VM.IP));
            begin
               null;
            end;
         end if;

         Instruction := Read_Byte (VM);
         case Instruction is
            when Lox_Chunk.OP_CONSTANT'Enum_Rep      =>
               declare
                  Value : constant Lox_Value.Value := Read_Constant (VM);
               begin
                  Push (VM, Value);
               end;

            when Lox_Chunk.OP_NIL'Enum_Rep           =>
               Push (VM, Lox_Value.Make_Nil);

            when Lox_Chunk.OP_TRUE'Enum_Rep          =>
               Push (VM, Lox_Value.Make_Bool (True));

            when Lox_Chunk.OP_FALSE'Enum_Rep         =>
               Push (VM, Lox_Value.Make_Bool (False));

            when Lox_Chunk.OP_POP'Enum_Rep           =>
               declare
                  Unused : constant Lox_Value.Value := Pop (VM);
               begin
                  null;
               end;

            when Lox_Chunk.OP_GET_LOCAL'Enum_Rep     =>
               declare
                  Slot : constant Lox_Chunk.Byte := Read_Byte (VM);
               begin
                  Push (VM, VM.Stack (Stack_Index (Slot)));
               end;

            when Lox_Chunk.OP_SET_LOCAL'Enum_Rep     =>
               declare
                  Slot : constant Lox_Chunk.Byte := Read_Byte (VM);
               begin
                  VM.Stack (Stack_Index (Slot)) := Peek (VM, 0);
               end;

            when Lox_Chunk.OP_GET_GLOBAL'Enum_Rep    =>
               declare
                  Name  : constant Lox_Value.Unbounded_String :=
                    Read_String (VM);
                  Value : constant Hash_Table.Cursor := VM.Globals.Find (Name);
                  use type Hash_Table.Cursor;
               begin
                  if Value = Hash_Table.No_Element then
                     Runtime_Error
                       (VM,
                        "Undefined variable '"
                        & Lox_Value.Unbounded.To_String (Name)
                        & "'.");
                     return INTERPRET_RUNTIME_ERROR;
                  end if;
                  Push (VM, Hash_Table.Element (Value));
               end;

            when Lox_Chunk.OP_DEFINE_GLOBAL'Enum_Rep =>
               declare
                  Name   : constant Lox_Value.Unbounded_String :=
                    Read_String (VM);
                  Unused : Lox_Value.Value;
               begin
                  if VM.Globals.Contains (Name) then
                     VM.Globals.Replace (Name, Peek (VM, 0));
                  else
                     VM.Globals.Insert (Name, Peek (VM, 0));
                  end if;
                  Unused := Pop (VM);
               end;

            when Lox_Chunk.OP_SET_GLOBAL'Enum_Rep    =>
               declare
                  Name : constant Lox_Value.Unbounded_String :=
                    Read_String (VM);
                  Item : constant Hash_Table.Cursor := VM.Globals.Find (Name);
                  use type Hash_Table.Cursor;
               begin
                  if Item = Hash_Table.No_Element then
                     Runtime_Error
                       (VM,
                        "Undefined variable '"
                        & Lox_Value.Unbounded.To_String (Name)
                        & "'.");
                     return INTERPRET_RUNTIME_ERROR;
                  end if;
                  VM.Globals.Replace_Element (Item, Peek (VM, 0));
               end;

            when Lox_Chunk.OP_EQUAL'Enum_Rep         =>
               declare
                  B : constant Lox_Value.Value := Pop (VM);
                  A : constant Lox_Value.Value := Pop (VM);
               begin
                  Push
                    (VM, Lox_Value.Make_Bool (Lox_Value.Values_Equal (A, B)));
               end;

            when Lox_Chunk.OP_GREATER'Enum_Rep       =>
               Result := Binary_Op_Greater (VM);
               if Result /= INTERPRET_OK then
                  return Result;
               end if;

            when Lox_Chunk.OP_LESS'Enum_Rep          =>
               Result := Binary_Op_Less (VM);
               if Result /= INTERPRET_OK then
                  return Result;
               end if;

            when Lox_Chunk.OP_ADD'Enum_Rep           =>
               if Lox_Value.Is_String (Peek (VM, 0))
                 and then Lox_Value.Is_String (Peek (VM, 1))
               then
                  Concatenate (VM);
               elsif Lox_Value.Is_Number (Peek (VM, 0))
                 and then Lox_Value.Is_Number (Peek (VM, 1))
               then
                  declare
                     B : constant Lox_Value.Lox_Float := Pop (VM).Number_Value;
                     A : constant Lox_Value.Lox_Float := Pop (VM).Number_Value;
                  begin
                     Push (VM, Lox_Value.Make_Number (A + B));
                  end;
               else
                  Runtime_Error
                    (VM, "Operands must be two numbers or two strings.");
                  return INTERPRET_RUNTIME_ERROR;
               end if;

            when Lox_Chunk.OP_SUBTRACT'Enum_Rep      =>
               Result := Binary_Op_Subtract (VM);
               if Result /= INTERPRET_OK then
                  return Result;
               end if;

            when Lox_Chunk.OP_MULTIPLY'Enum_Rep      =>
               Result := Binary_Op_Multiply (VM);
               if Result /= INTERPRET_OK then
                  return Result;
               end if;

            when Lox_Chunk.OP_DIVIDE'Enum_Rep        =>
               Result := Binary_Op_Divide (VM);
               if Result /= INTERPRET_OK then
                  return Result;
               end if;

            when Lox_Chunk.OP_NOT'Enum_Rep           =>
               Push (VM, Lox_Value.Make_Bool (Is_Falsey (Pop (VM))));

            when Lox_Chunk.OP_NEGATE'Enum_Rep        =>
               if not Lox_Value.Is_Number (Peek (VM, 0)) then
                  Runtime_Error (VM, "Operand must be a number.");
                  return INTERPRET_RUNTIME_ERROR;
               end if;

               declare
                  Value : constant Lox_Value.Lox_Float :=
                    Pop (VM).Number_Value;
               begin
                  Push (VM, Lox_Value.Make_Number (-Value));
               end;

            when Lox_Chunk.OP_PRINT'Enum_Rep         =>
               Lox_Value.Print_Value (Pop (VM));
               Ada.Text_IO.New_Line;

            when Lox_Chunk.OP_JUMP'Enum_Rep          =>
               declare
                  Offset : constant Short := Read_Short (VM);
               begin
                  VM.IP :=
                    VM.Chunk.Code.To_Cursor
                      (Lox_Chunk.Byte_Vectors.To_Index (VM.IP)
                       + Natural (Offset));
               end;

            when Lox_Chunk.OP_JUMP_IF_FALSE'Enum_Rep =>
               declare
                  Offset : constant Short := Read_Short (VM);
               begin
                  if Is_Falsey (Peek (VM, 0)) then
                     VM.IP :=
                       VM.Chunk.Code.To_Cursor
                         (Lox_Chunk.Byte_Vectors.To_Index (VM.IP)
                          + Natural (Offset));
                  end if;
               end;

            when Lox_Chunk.OP_RETURN'Enum_Rep        =>
               return INTERPRET_OK;

            when others                              =>
               return INTERPRET_RUNTIME_ERROR;
         end case;
      end loop;
   end Run;

end Lox_VM;
