with Debug;
with Lox_Compiler;

with Ada.Integer_Text_IO;
with Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Lox_VM is
   package Unbounded renames Ada.Strings.Unbounded;
   use type Lox_Value.Lox_Float;

   VM         : VM_Context;
   Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

   procedure Init_VM is
   begin
      Reset_Stack;

      Define_Native ("clock", Clock_Native'Access);
   end Init_VM;

   procedure Free_VM is
   begin
      Lox_Object.Free_Objects (VM.Objects);
   end Free_VM;

   function Interpret
     (Source : Lox_Scanner.Source_Code) return Interpret_Result
   is
      Unused : Boolean;
      Func   : Lox_Object.Obj_Function_Access;
      use type Lox_Object.Obj_Function_Access;
   begin
      Func := Lox_Compiler.Compile (Source);
      if Func = null then
         return INTERPRET_COMPILE_ERROR;
      end if;

      Push (Lox_Value.Make_Function (Func));
      Unused := Call (Func, 0);

      return Run;
   end Interpret;

   function New_Function return Lox_Object.Obj_Function_Access is
   begin
      return Lox_Object.New_Function (VM.Objects);
   end New_Function;

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

   procedure Push (Value : Lox_Value.Value) is
   begin
      VM.Stack (VM.Stack_Top) := Value;
      VM.Stack_Top := Stack_Index'Succ (VM.Stack_Top);
   end Push;

   function Pop return Lox_Value.Value is
   begin
      VM.Stack_Top := Stack_Index'Pred (VM.Stack_Top);
      return VM.Stack (VM.Stack_Top);
   end Pop;

   function Peek (Distance : Integer) return Lox_Value.Value is
      Peek_Index : constant Integer := Integer (VM.Stack_Top) - Distance - 1;
   begin
      if Peek_Index < 0 then
         return (Kind => Lox_Value.VAL_NIL);
      end if;
      return VM.Stack (Stack_Index (Peek_Index));
   end Peek;

   function Call
     (Func : Lox_Object.Obj_Function_Access; Arg_Count : Natural)
      return Boolean
   is
      function Arity_Error_Message return String is
         Arity_String     : String (1 .. 16);
         Arg_Count_String : String (1 .. 16);
      begin
         Ada.Integer_Text_IO.Put (To => Arity_String, Item => Func.Arity);
         Ada.Integer_Text_IO.Put (To => Arg_Count_String, Item => Arg_Count);
         return
           "Expected "
           & Ada.Strings.Fixed.Trim (Arity_String, Ada.Strings.Both)
           & " arguments but got "
           & Ada.Strings.Fixed.Trim (Arg_Count_String, Ada.Strings.Both)
           & ".";
      end Arity_Error_Message;

   begin
      if Arg_Count /= Func.Arity then
         Runtime_Error (Arity_Error_Message);
         return False;
      end if;

      if VM.Frame_Count > Natural (Call_Frame_Index'Last) then
         Runtime_Error ("Stack overflow.");
         return False;
      end if;

      declare
         Frame : Call_Frame renames
           VM.Frames (Call_Frame_Index (VM.Frame_Count));
      begin

         VM.Frame_Count := Natural'Succ (VM.Frame_Count);

         Frame.Func := Func;
         Frame.IP := Func.all.Chunk.Code.First;
         Frame.Slots := VM.Stack_Top - Stack_Index (Arg_Count) - 1;
      end;
      return True;
   end Call;

   function Call_Native
     (Native : Lox_Value.Native_Fn; Arg_Count : Natural) return Boolean
   is
      Result : constant Lox_Value.Value := Native.all;
   begin
      VM.Stack_Top := VM.Stack_Top - Stack_Index (Arg_Count) - 1;
      Push (Result);
      return True;
   end Call_Native;

   function Call_Value
     (Callee : Lox_Value.Value; Arg_Count : Natural) return Boolean is
   begin
      if Lox_Value.Is_Function (Callee) then
         return
           Call
             (Lox_Object.Obj_Function_Access (Callee.Function_Value),
              Arg_Count);

      elsif Lox_Value.Is_Native (Callee) then
         return Call_Native (Callee.Native_Value, Arg_Count);

      end if;

      Runtime_Error ("Can only call functions and classes.");
      return False;
   end Call_Value;

   function Is_Falsey (Value : Lox_Value.Value) return Boolean is
   begin
      return
        Lox_Value.Is_Nil (Value)
        or else (Lox_Value.Is_Bool (Value) and then not Value.Bool_Value);
   end Is_Falsey;

   procedure Concatenate is
      B : constant String := Lox_Value.Unbounded.To_String (Pop.String_Value);
      A : constant String := Lox_Value.Unbounded.To_String (Pop.String_Value);
   begin
      Push
        (Lox_Value.Make_String
           (Lox_Value.Unbounded.To_Unbounded_String (A & B)));
   end Concatenate;

   procedure Reset_Stack is
   begin
      VM.Stack_Top := 0;
      VM.Frame_Count := 0;
   end Reset_Stack;

   procedure Runtime_Error (Message : String) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Message);

      for I in reverse
        Call_Frame_Index'First
        .. Call_Frame_Index (Natural'Pred (VM.Frame_Count))
      loop
         declare
            Frame       : Call_Frame renames VM.Frames (I);
            Instruction : constant Natural :=
              Lox_Chunk.Byte_Vectors.To_Index
                (Lox_Chunk.Byte_Vectors.Previous (Frame.IP));
            use type Unbounded.Unbounded_String;
         begin
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "[line ");
            Ada.Integer_Text_IO.Put
              (Ada.Text_IO.Standard_Error,
               Frame.Func.Chunk.Lines (Instruction),
               Width => 0);
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "] in ");
            if Frame.Func.Name = Unbounded.Null_Unbounded_String then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "script");
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Unbounded.To_String (Frame.Func.Name) & "()");
            end if;
         end;
      end loop;

      Reset_Stack;
   end Runtime_Error;

   procedure Define_Native (Name : String; Func : Lox_Value.Native_Fn) is
      Unused : Lox_Value.Value;
   begin
      Push (Lox_Value.Make_String (Unbounded.To_Unbounded_String (Name)));
      Push (Lox_Value.Make_Native (Func));
      VM.Globals.Insert (VM.Stack (0).String_Value, VM.Stack (1));
      Unused := Pop;
      Unused := Pop;
   end Define_Native;

   function Clock_Native return Lox_Value.Value is
      use type Ada.Real_Time.Time;
      Now : constant Ada.Real_Time.Time_Span :=
        Ada.Real_Time.Clock - Start_Time;
   begin
      return
        Lox_Value.Make_Number
          ((Is_Valid => True,
            Value    => Long_Float (Ada.Real_Time.To_Duration (Now))));
   end Clock_Native;

   function Read_Byte (Frame : in out Call_Frame) return Byte is
      Result : constant Byte := Lox_Chunk.Byte_Vectors.Element (Frame.IP);
   begin
      Frame.IP := Lox_Chunk.Byte_Vectors.Next (Frame.IP);
      return Result;
   end Read_Byte;

   function Read_Short (Frame : in out Call_Frame) return Short is
      High : Byte;
      Low  : Byte;
   begin
      High := Lox_Chunk.Byte_Vectors.Element (Frame.IP);
      Frame.IP := Lox_Chunk.Byte_Vectors.Next (Frame.IP);
      Low := Lox_Chunk.Byte_Vectors.Element (Frame.IP);
      Frame.IP := Lox_Chunk.Byte_Vectors.Next (Frame.IP);
      return (Short (High) * 256 + Short (Low));
   end Read_Short;

   function Read_Constant (Frame : in out Call_Frame) return Lox_Value.Value is
      Index : constant Natural := Natural (Read_Byte (Frame));
   begin
      return Frame.Func.Chunk.Constants (Index);
   end Read_Constant;

   function Read_String
     (Frame : in out Call_Frame) return Lox_Value.Unbounded_String is
   begin
      return Read_Constant (Frame).String_Value;
   end Read_String;

   generic
      type Result_Type is private;
      with
        function Op
          (A : Lox_Value.Lox_Float; B : Lox_Value.Lox_Float)
           return Result_Type;
      with function Make (R : Result_Type) return Lox_Value.Value;
   function Binary_Op return Interpret_Result;

   function Binary_Op return Interpret_Result is
   begin
      if not Lox_Value.Is_Number (Peek (0))
        or else not Lox_Value.Is_Number (Peek (1))
      then
         Runtime_Error ("Operands must be numbers.");
         return INTERPRET_RUNTIME_ERROR;
      end if;

      declare
         B      : constant Lox_Value.Lox_Float := Pop.Number_Value;
         A      : constant Lox_Value.Lox_Float := Pop.Number_Value;
         Result : constant Result_Type := Op (A, B);
      begin
         Push (Make (Result));
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

   function Run return Interpret_Result is
      Frame_Index : Call_Frame_Index :=
        Call_Frame_Index (Natural'Pred (VM.Frame_Count));
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
               Frame  : Call_Frame renames VM.Frames (Frame_Index);
               Unused : Natural :=
                 Debug.Disassemble_Instruction
                   (Frame.Func.Chunk,
                    Lox_Chunk.Byte_Vectors.To_Index (Frame.IP));
            begin
               null;
            end;
         end if;

         declare
            Frame       : Call_Frame renames VM.Frames (Frame_Index);
            Instruction : constant Byte := Read_Byte (Frame);
            Result      : Interpret_Result;
         begin
            case Instruction is
               when Lox_Chunk.OP_CONSTANT'Enum_Rep      =>
                  declare
                     Value : constant Lox_Value.Value := Read_Constant (Frame);
                  begin
                     Push (Value);
                  end;

               when Lox_Chunk.OP_NIL'Enum_Rep           =>
                  Push (Lox_Value.Make_Nil);

               when Lox_Chunk.OP_TRUE'Enum_Rep          =>
                  Push (Lox_Value.Make_Bool (True));

               when Lox_Chunk.OP_FALSE'Enum_Rep         =>
                  Push (Lox_Value.Make_Bool (False));

               when Lox_Chunk.OP_POP'Enum_Rep           =>
                  declare
                     Unused : constant Lox_Value.Value := Pop;
                  begin
                     null;
                  end;

               when Lox_Chunk.OP_PUSH'Enum_Rep          =>
                  Push (Peek (0));

               when Lox_Chunk.OP_GET_LOCAL'Enum_Rep     =>
                  declare
                     Slot  : constant Byte := Read_Byte (Frame);
                     Index : constant Stack_Index :=
                       Frame.Slots + Stack_Index (Slot);
                  begin
                     Push (VM.Stack (Index));
                  end;

               when Lox_Chunk.OP_SET_LOCAL'Enum_Rep     =>
                  declare
                     Slot  : constant Byte := Read_Byte (Frame);
                     Index : constant Stack_Index :=
                       Frame.Slots + Stack_Index (Slot);
                  begin
                     VM.Stack (Index) := Peek (0);
                  end;

               when Lox_Chunk.OP_GET_GLOBAL'Enum_Rep    =>
                  declare
                     Name  : constant Lox_Value.Unbounded_String :=
                       Read_String (Frame);
                     Value : constant Hash_Table.Cursor :=
                       VM.Globals.Find (Name);
                     use type Hash_Table.Cursor;
                  begin
                     if Value = Hash_Table.No_Element then
                        Runtime_Error
                          ("Undefined variable '"
                           & Lox_Value.Unbounded.To_String (Name)
                           & "'.");
                        return INTERPRET_RUNTIME_ERROR;
                     end if;
                     Push (Hash_Table.Element (Value));
                  end;

               when Lox_Chunk.OP_DEFINE_GLOBAL'Enum_Rep =>
                  declare
                     Name   : constant Lox_Value.Unbounded_String :=
                       Read_String (Frame);
                     Unused : Lox_Value.Value;
                  begin
                     if VM.Globals.Contains (Name) then
                        VM.Globals.Replace (Name, Peek (0));
                     else
                        VM.Globals.Insert (Name, Peek (0));
                     end if;
                     Unused := Pop;
                  end;

               when Lox_Chunk.OP_SET_GLOBAL'Enum_Rep    =>
                  declare
                     Name : constant Lox_Value.Unbounded_String :=
                       Read_String (Frame);
                     Item : constant Hash_Table.Cursor :=
                       VM.Globals.Find (Name);
                     use type Hash_Table.Cursor;
                  begin
                     if Item = Hash_Table.No_Element then
                        Runtime_Error
                          ("Undefined variable '"
                           & Lox_Value.Unbounded.To_String (Name)
                           & "'.");
                        return INTERPRET_RUNTIME_ERROR;
                     end if;
                     VM.Globals.Replace_Element (Item, Peek (0));
                  end;

               when Lox_Chunk.OP_EQUAL'Enum_Rep         =>
                  declare
                     B : constant Lox_Value.Value := Pop;
                     A : constant Lox_Value.Value := Pop;
                  begin
                     Push
                       (Lox_Value.Make_Bool (Lox_Value.Values_Equal (A, B)));
                  end;

               when Lox_Chunk.OP_GREATER'Enum_Rep       =>
                  Result := Binary_Op_Greater;
                  if Result /= INTERPRET_OK then
                     return Result;
                  end if;

               when Lox_Chunk.OP_LESS'Enum_Rep          =>
                  Result := Binary_Op_Less;
                  if Result /= INTERPRET_OK then
                     return Result;
                  end if;

               when Lox_Chunk.OP_ADD'Enum_Rep           =>
                  if Lox_Value.Is_String (Peek (0))
                    and then Lox_Value.Is_String (Peek (1))
                  then
                     Concatenate;
                  elsif Lox_Value.Is_Number (Peek (0))
                    and then Lox_Value.Is_Number (Peek (1))
                  then
                     declare
                        B : constant Lox_Value.Lox_Float := Pop.Number_Value;
                        A : constant Lox_Value.Lox_Float := Pop.Number_Value;
                     begin
                        Push (Lox_Value.Make_Number (A + B));
                     end;
                  else
                     Runtime_Error
                       ("Operands must be two numbers or two strings.");
                     return INTERPRET_RUNTIME_ERROR;
                  end if;

               when Lox_Chunk.OP_SUBTRACT'Enum_Rep      =>
                  Result := Binary_Op_Subtract;
                  if Result /= INTERPRET_OK then
                     return Result;
                  end if;

               when Lox_Chunk.OP_MULTIPLY'Enum_Rep      =>
                  Result := Binary_Op_Multiply;
                  if Result /= INTERPRET_OK then
                     return Result;
                  end if;

               when Lox_Chunk.OP_DIVIDE'Enum_Rep        =>
                  Result := Binary_Op_Divide;
                  if Result /= INTERPRET_OK then
                     return Result;
                  end if;

               when Lox_Chunk.OP_NOT'Enum_Rep           =>
                  Push (Lox_Value.Make_Bool (Is_Falsey (Pop)));

               when Lox_Chunk.OP_NEGATE'Enum_Rep        =>
                  if not Lox_Value.Is_Number (Peek (0)) then
                     Runtime_Error ("Operand must be a number.");
                     return INTERPRET_RUNTIME_ERROR;
                  end if;

                  declare
                     Value : constant Lox_Value.Lox_Float := Pop.Number_Value;
                  begin
                     Push (Lox_Value.Make_Number (-Value));
                  end;

               when Lox_Chunk.OP_PRINT'Enum_Rep         =>
                  Lox_Value.Print_Value (Pop);
                  Ada.Text_IO.New_Line;

               when Lox_Chunk.OP_JUMP'Enum_Rep          =>
                  declare
                     Offset : constant Short := Read_Short (Frame);
                  begin
                     Frame.IP :=
                       Frame.Func.Chunk.Code.To_Cursor
                         (Lox_Chunk.Byte_Vectors.To_Index (Frame.IP)
                          + Natural (Offset));
                  end;

               when Lox_Chunk.OP_JUMP_IF_FALSE'Enum_Rep =>
                  declare
                     Offset : constant Short := Read_Short (Frame);
                  begin
                     if Is_Falsey (Peek (0)) then
                        Frame.IP :=
                          Frame.Func.Chunk.Code.To_Cursor
                            (Lox_Chunk.Byte_Vectors.To_Index (Frame.IP)
                             + Natural (Offset));
                     end if;
                  end;

               when Lox_Chunk.OP_LOOP'Enum_Rep          =>
                  declare
                     Offset : constant Short := Read_Short (Frame);
                  begin
                     Frame.IP :=
                       Frame.Func.Chunk.Code.To_Cursor
                         (Lox_Chunk.Byte_Vectors.To_Index (Frame.IP)
                          - Natural (Offset));
                  end;

               when Lox_Chunk.OP_CALL'Enum_Rep          =>
                  declare
                     Arg_Count : constant Byte := Read_Byte (Frame);
                  begin
                     if not Call_Value
                              (Peek (Integer (Arg_Count)), Natural (Arg_Count))
                     then
                        return INTERPRET_RUNTIME_ERROR;
                     end if;
                     Frame_Index :=
                       Call_Frame_Index (Natural'Pred (VM.Frame_Count));
                  end;

               when Lox_Chunk.OP_RETURN'Enum_Rep        =>
                  declare
                     Result : constant Lox_Value.Value := Pop;
                     Unused : Lox_Value.Value;
                  begin
                     VM.Frame_Count := Natural'Pred (VM.Frame_Count);
                     if VM.Frame_Count = 0 then
                        Unused := Pop;
                        return INTERPRET_OK;
                     end if;

                     VM.Stack_Top := Frame.Slots;
                     Push (Result);
                     Frame_Index :=
                       Call_Frame_Index (Natural'Pred (VM.Frame_Count));
                  end;

               when others                              =>
                  return INTERPRET_RUNTIME_ERROR;
            end case;
         end;
      end loop;
   end Run;

end Lox_VM;
