with Debug;

with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Lox_VM;

package body Lox_Compiler is

   package Unbounded renames Ada.Strings.Unbounded;

   type Rules_Type is array (Lox_Scanner.TokenType) of Parse_Rule;
   Rules : constant Rules_Type :=
     [Lox_Scanner.TOKEN_LEFT_PAREN    =>
        (Grouping'Access, Call'Access, PREC_CALL),
      Lox_Scanner.TOKEN_RIGHT_PAREN   => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_LEFT_BRACE    => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_RIGHT_BRACE   => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_COMMA         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_DOT           => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_MINUS         =>
        (Unary'Access, Binary'Access, PREC_TERM),
      Lox_Scanner.TOKEN_PLUS          => (null, Binary'Access, PREC_TERM),
      Lox_Scanner.TOKEN_SEMICOLON     => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_SLASH         => (null, Binary'Access, PREC_FACTOR),
      Lox_Scanner.TOKEN_STAR          => (null, Binary'Access, PREC_FACTOR),
      Lox_Scanner.TOKEN_BANG          => (Unary'Access, null, PREC_NONE),
      Lox_Scanner.TOKEN_BANG_EQUAL    => (null, Binary'Access, PREC_EQUALITY),
      Lox_Scanner.TOKEN_EQUAL         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_EQUAL_EQUAL   => (null, Binary'Access, PREC_EQUALITY),
      Lox_Scanner.TOKEN_GREATER       =>
        (null, Binary'Access, PREC_COMPARISON),
      Lox_Scanner.TOKEN_GREATER_EQUAL =>
        (null, Binary'Access, PREC_COMPARISON),
      Lox_Scanner.TOKEN_LESS          =>
        (null, Binary'Access, PREC_COMPARISON),
      Lox_Scanner.TOKEN_LESS_EQUAL    =>
        (null, Binary'Access, PREC_COMPARISON),
      Lox_Scanner.TOKEN_IDENTIFIER    => (Variable'Access, null, PREC_NONE),
      Lox_Scanner.TOKEN_STRING        =>
        (String_Literal'Access, null, PREC_NONE),
      Lox_Scanner.TOKEN_NUMBER        => (Number'Access, null, PREC_NONE),
      Lox_Scanner.TOKEN_AND           => (null, Logical_And'Access, PREC_AND),
      Lox_Scanner.TOKEN_CLASS         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_ELSE          => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_FALSE         => (Literal'Access, null, PREC_NONE),
      Lox_Scanner.TOKEN_FOR           => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_FUN           => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_IF            => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_NIL           => (Literal'Access, null, PREC_NONE),
      Lox_Scanner.TOKEN_OR            => (null, Logical_Or'Access, PREC_OR),
      Lox_Scanner.TOKEN_PRINT         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_RETURN        => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_SUPER         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_THIS          => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_TRUE          => (Literal'Access, null, PREC_NONE),
      Lox_Scanner.TOKEN_VAR           => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_WHILE         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_CASE          => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_COLON         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_DEFAULT       => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_SWITCH        => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_ERROR         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_EOF           => (null, null, PREC_NONE)];

   function Compile
     (Source : Lox_Scanner.Source_Code) return Lox_Object.Object_Access
   is
      C        : Compiler_Context;
      Compiler : Compiler_Instance;
      Func     : Lox_Object.Object_Access := null;
   begin
      C.Scanner := Lox_Scanner.Init (Source);
      Init_Compiler (C, Compiler.Instance, TYPE_SCRIPT);

      C.Parser.Had_Error := False;
      C.Parser.Panic_Mode := False;

      Advance (C);

      while not Match (C, Lox_Scanner.TOKEN_EOF) loop
         Declaration (C);
      end loop;

      Func := End_Compiler (C);

      if C.Parser.Had_Error then
         return null;
      else
         return Func;
      end if;
   end Compile;

   function Just (Value : Natural) return Maybe_Natural is
   begin
      return (Has_Value => True, Value => Value);
   end Just;

   function None return Maybe_Natural is
   begin
      return (Has_Value => False);
   end None;

   overriding
   procedure Initialize (Compiler : in out Compiler_Instance) is
   begin
      Compiler.Instance := new Compiler_Type;
   end Initialize;

   overriding
   procedure Finalize (Compiler : in out Compiler_Instance) is
   begin
      Free (Compiler.Instance);
   end Finalize;

   function Current_Func
     (C : in out Compiler_Context) return Lox_Object.Object_Access is
   begin
      return C.Current.Func;
   end Current_Func;

   procedure Error_At
     (C : in out Compiler_Context; Token : Lox_Scanner.Token; Message : String)
   is
   begin
      if C.Parser.Panic_Mode then
         return;
      end if;
      C.Parser.Panic_Mode := True;

      declare
         Buffer : String (1 .. 8);
      begin
         Ada.Integer_Text_IO.Put (To => Buffer, Item => Token.Line);
         Ada.Text_IO.Put
           (Ada.Text_IO.Standard_Error,
            "[line "
            & Ada.Strings.Fixed.Trim (Buffer, Ada.Strings.Both)
            & "] Error");
      end;

      if Token.Kind = Lox_Scanner.TOKEN_EOF then
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, " at end");
      elsif Token.Kind = Lox_Scanner.TOKEN_ERROR then
         null;
      else
         Ada.Text_IO.Put
           (Ada.Text_IO.Standard_Error,
            " at '" & Lox_Scanner.Unbounded.To_String (Token.Lexeme) & "'");
      end if;

      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, ": " & Message);
      C.Parser.Had_Error := True;
   end Error_At;

   procedure Error (C : in out Compiler_Context; Message : String) is
   begin
      Error_At (C, C.Parser.Previous, Message);
   end Error;

   procedure Error_At_Current (C : in out Compiler_Context; Message : String)
   is
   begin
      Error_At (C, C.Parser.Current, Message);
   end Error_At_Current;

   procedure Advance (C : in out Compiler_Context) is
   begin
      C.Parser.Previous := C.Parser.Current;

      loop
         C.Parser.Current := Lox_Scanner.Scan_Token (C.Scanner);
         exit when C.Parser.Current.Kind /= Lox_Scanner.TOKEN_ERROR;

         Error_At_Current
           (C, Lox_Scanner.Unbounded.To_String (C.Parser.Current.Lexeme));
      end loop;
   end Advance;

   procedure Consume
     (C       : in out Compiler_Context;
      Kind    : Lox_Scanner.TokenType;
      Message : String) is
   begin
      if C.Parser.Current.Kind = Kind then
         Advance (C);
         return;
      end if;

      Error_At_Current (C, Message);
   end Consume;

   function Check
     (C : in out Compiler_Context; Kind : Lox_Scanner.TokenType) return Boolean
   is
   begin
      return C.Parser.Current.Kind = Kind;
   end Check;

   function Match
     (C : in out Compiler_Context; Kind : Lox_Scanner.TokenType) return Boolean
   is
   begin
      if not Check (C, Kind) then
         return False;
      end if;
      Advance (C);
      return True;
   end Match;

   procedure Emit_Byte (C : in out Compiler_Context; Value : Byte) is
   begin
      Lox_Chunk.Write (Current_Func (C).Chunk, Value, C.Parser.Previous.Line);
   end Emit_Byte;

   procedure Emit_Byte
     (C : in out Compiler_Context; Op_Code : Lox_Chunk.Op_Code) is
   begin
      Lox_Chunk.Write
        (Current_Func (C).Chunk, Op_Code, C.Parser.Previous.Line);
   end Emit_Byte;

   procedure Emit_Bytes
     (C : in out Compiler_Context; Byte_1 : Lox_Chunk.Op_Code; Byte_2 : Byte)
   is
   begin
      Emit_Byte (C, Byte_1);
      Emit_Byte (C, Byte_2);
   end Emit_Bytes;

   procedure Emit_Bytes
     (C      : in out Compiler_Context;
      Byte_1 : Lox_Chunk.Op_Code;
      Byte_2 : Lox_Chunk.Op_Code) is
   begin
      Emit_Byte (C, Byte_1);
      Emit_Byte (C, Byte_2);
   end Emit_Bytes;

   procedure Emit_Loop (C : in out Compiler_Context; Loop_Start : Natural) is
   begin
      Emit_Byte (C, Lox_Chunk.OP_LOOP);

      declare
         Offset : constant Natural :=
           Natural (Current_Func (C).Chunk.Code.Length) - Loop_Start + 2;
      begin
         if Offset > Natural (Short'Last) then
            Error (C, "Loop body too large.");
            return;
         end if;

         Emit_Byte (C, Byte (Offset / 256));
         Emit_Byte (C, Byte (Offset mod 256));
      end;
   end Emit_Loop;

   function Emit_Jump
     (C : in out Compiler_Context; Instruction : Lox_Chunk.Op_Code)
      return Natural is
   begin
      Emit_Byte (C, Instruction);
      declare
         Patch_Index : constant Natural :=
           Natural (Current_Func (C).Chunk.Code.Length);
      begin
         Emit_Byte (C, Byte'Last);
         Emit_Byte (C, Byte'Last);
         return Patch_Index;
      end;
   end Emit_Jump;

   procedure Emit_Return (C : in out Compiler_Context) is
   begin
      Emit_Byte (C, Lox_Chunk.OP_NIL);
      Emit_Byte (C, Lox_Chunk.OP_RETURN);
   end Emit_Return;

   function Make_Constant
     (C : in out Compiler_Context; Value : Lox_Value.Value) return Byte
   is
      Id : constant Natural :=
        Lox_Chunk.Add_Constant (Current_Func (C).Chunk, Value);
   begin
      if Id > Natural (Byte'Last) then
         Error (C, "Too many constants in one chunk.");
         return 0;
      end if;

      return Byte (Id);
   end Make_Constant;

   procedure Emit_Constant
     (C : in out Compiler_Context; Value : Lox_Value.Value)
   is
      Id : constant Byte := Make_Constant (C, Value);
   begin
      Emit_Bytes (C, Lox_Chunk.OP_CONSTANT, Id);
   end Emit_Constant;

   procedure Patch_Jump (C : in out Compiler_Context; Offset : Natural) is
      Jump : constant Natural :=
        Natural (Current_Func (C).Chunk.Code.Length) - Offset - 2;
   begin
      if Jump > Natural (Short'Last) then
         Error (C, "Too much code to jump over.");
         return;
      end if;

      Current_Func (C).Chunk.Code (Offset) := Byte (Jump / 256);
      Current_Func (C).Chunk.Code (Natural'Succ (Offset)) :=
        Byte (Jump mod 256);
   end Patch_Jump;

   procedure Init_Compiler
     (C        : in out Compiler_Context;
      Compiler : Compiler_Access;
      Kind     : Function_Kind) is
   begin
      Compiler.Enclosing := C.Current;
      Compiler.Func := null;
      Compiler.Kind := Kind;
      Compiler.Local_Count := 0;
      Compiler.Scope_Depth := 0;
      Compiler.Func := Lox_VM.New_Function;
      C.Current := Compiler;

      if Kind /= TYPE_SCRIPT then
         C.Current.Func.Name := C.Parser.Previous.Lexeme;
      end if;

      declare
         Local : Local_Type renames
           C.Current.Locals (Local_Index (C.Current.Local_Count));
      begin
         C.Current.Local_Count := Natural'Succ (C.Current.Local_Count);
         Local.Depth := Just (0);
         Local.Is_Captured := False;
         Local.Name.Lexeme := Unbounded.Null_Unbounded_String;
      end;
   end Init_Compiler;

   function End_Compiler
     (C : in out Compiler_Context) return Lox_Object.Object_Access
   is
      Func : Lox_Object.Object_Access := null;
      use type Unbounded.Unbounded_String;
   begin
      Emit_Return (C);
      Func := C.Current.Func;

      if Debug.Print_Code_Enabled then
         if not C.Parser.Had_Error then
            declare
               Name : constant String :=
                 (if Func.Name = Unbounded.Null_Unbounded_String
                  then "<script>"
                  else Unbounded.To_String (Func.Name));
            begin
               Debug.Disassemble_Chunk (Current_Func (C).Chunk, Name);
            end;
         end if;
      end if;

      C.Current := Compiler_Access (C.Current.Enclosing);
      return Func;
   end End_Compiler;

   procedure Begin_Scope (C : in out Compiler_Context) is
   begin
      C.Current.Scope_Depth := Natural'Succ (C.Current.Scope_Depth);
   end Begin_Scope;

   procedure End_Scope (C : in out Compiler_Context) is
      function Has_Variable_In_Scope return Boolean is
      begin
         if C.Current.Local_Count = 0 then
            return False;
         end if;
         declare
            Top_Index : constant Local_Index :=
              Local_Index (Natural'Pred (C.Current.Local_Count));
         begin
            return
              C.Current.Locals (Top_Index).Depth.Value > C.Current.Scope_Depth;
         end;
      end Has_Variable_In_Scope;

      function Is_Captured return Boolean is
         Top_Index : constant Local_Index :=
           Local_Index (Natural'Pred (C.Current.Local_Count));
      begin
         return C.Current.Locals (Top_Index).Is_Captured;
      end Is_Captured;
   begin
      C.Current.Scope_Depth := Natural'Pred (C.Current.Scope_Depth);

      while Has_Variable_In_Scope loop
         if Is_Captured then
            Emit_Byte (C, Lox_Chunk.OP_CLOSE_UPVALUE);
         else
            Emit_Byte (C, Lox_Chunk.OP_POP);
         end if;
         C.Current.Local_Count := Natural'Pred (C.Current.Local_Count);
      end loop;
   end End_Scope;

   procedure Binary
     (C : in out Compiler_Context; Can_Assign : Boolean with Unreferenced)
   is
      Operator_Type : constant Lox_Scanner.TokenType := C.Parser.Previous.Kind;
      Rule          : constant Parse_Rule := Get_Rule (Operator_Type);
   begin
      Parse_Precedence (C, Precedence_Type'Succ (Rule.Precedence));

      case Operator_Type is
         when Lox_Scanner.TOKEN_BANG_EQUAL    =>
            Emit_Bytes (C, Lox_Chunk.OP_EQUAL, Lox_Chunk.OP_NOT);

         when Lox_Scanner.TOKEN_EQUAL_EQUAL   =>
            Emit_Byte (C, Lox_Chunk.OP_EQUAL);

         when Lox_Scanner.TOKEN_GREATER       =>
            Emit_Byte (C, Lox_Chunk.OP_GREATER);

         when Lox_Scanner.TOKEN_GREATER_EQUAL =>
            Emit_Bytes (C, Lox_Chunk.OP_LESS, Lox_Chunk.OP_NOT);

         when Lox_Scanner.TOKEN_LESS          =>
            Emit_Byte (C, Lox_Chunk.OP_LESS);

         when Lox_Scanner.TOKEN_LESS_EQUAL    =>
            Emit_Bytes (C, Lox_Chunk.OP_GREATER, Lox_Chunk.OP_NOT);

         when Lox_Scanner.TOKEN_PLUS          =>
            Emit_Byte (C, Lox_Chunk.OP_ADD);

         when Lox_Scanner.TOKEN_MINUS         =>
            Emit_Byte (C, Lox_Chunk.OP_SUBTRACT);

         when Lox_Scanner.TOKEN_STAR          =>
            Emit_Byte (C, Lox_Chunk.OP_MULTIPLY);

         when Lox_Scanner.TOKEN_SLASH         =>
            Emit_Byte (C, Lox_Chunk.OP_DIVIDE);

         when others                          =>
            return;
      end case;
   end Binary;

   procedure Call
     (C : in out Compiler_Context; Can_Assign : Boolean with Unreferenced)
   is
      Arg_Count : constant Byte := Argument_List (C);
   begin
      Emit_Bytes (C, Lox_Chunk.OP_CALL, Arg_Count);
   end Call;

   procedure Literal
     (C : in out Compiler_Context; Can_Assign : Boolean with Unreferenced) is
   begin
      case C.Parser.Previous.Kind is
         when Lox_Scanner.TOKEN_FALSE =>
            Emit_Byte (C, Lox_Chunk.OP_FALSE);

         when Lox_Scanner.TOKEN_NIL   =>
            Emit_Byte (C, Lox_Chunk.OP_NIL);

         when Lox_Scanner.TOKEN_TRUE  =>
            Emit_Byte (C, Lox_Chunk.OP_TRUE);

         when others                  =>
            return;
      end case;
   end Literal;

   procedure Grouping
     (C : in out Compiler_Context; Can_Assign : Boolean with Unreferenced) is
   begin
      Expression (C);
      Consume
        (C, Lox_Scanner.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
   end Grouping;

   procedure Number
     (C : in out Compiler_Context; Can_Assign : Boolean with Unreferenced)
   is
      Lexeme : constant String :=
        Unbounded.To_String (C.Parser.Previous.Lexeme);
      Value  : constant Lox_Value.Lox_Float :=
        (Is_Valid => True, Value => Long_Float'Value (Lexeme));
   begin
      Emit_Constant (C, Lox_Value.Make_Number (Value));
   end Number;

   procedure String_Literal
     (C : in out Compiler_Context; Can_Assign : Boolean with Unreferenced)
   is
      From : constant Positive := Positive'Succ (1);
      To   : constant Natural :=
        Natural'Pred (Unbounded.Length (C.Parser.Previous.Lexeme));
      Str  : constant Lox_Value.Unbounded_String :=
        Unbounded.Unbounded_Slice (C.Parser.Previous.Lexeme, From, To);
   begin
      Emit_Constant (C, Lox_Value.Make_String (Str));
   end String_Literal;

   procedure Logical_And
     (C : in out Compiler_Context; Can_Assign : Boolean with Unreferenced)
   is
      End_Jump : constant Natural := Emit_Jump (C, Lox_Chunk.OP_JUMP_IF_FALSE);
   begin
      Emit_Byte (C, Lox_Chunk.OP_POP);
      Parse_Precedence (C, PREC_AND);

      Patch_Jump (C, End_Jump);
   end Logical_And;

   procedure Logical_Or
     (C : in out Compiler_Context; Can_Assign : Boolean with Unreferenced)
   is
      Else_Jump : constant Natural :=
        Emit_Jump (C, Lox_Chunk.OP_JUMP_IF_FALSE);
      End_Jump  : constant Natural := Emit_Jump (C, Lox_Chunk.OP_JUMP);
   begin
      Patch_Jump (C, Else_Jump);
      Emit_Byte (C, Lox_Chunk.OP_POP);

      Parse_Precedence (C, PREC_OR);
      Patch_Jump (C, End_Jump);
   end Logical_Or;

   procedure Named_Variable
     (C          : in out Compiler_Context;
      Name       : Lox_Scanner.Token;
      Can_Assign : Boolean)
   is
      Resolved_Local   : Local_Index;
      Resolved_Upvalue : Upvalue_Index;
      Arg              : Byte;
      Get_Op           : Lox_Chunk.Op_Code;
      Set_Op           : Lox_Chunk.Op_Code;
   begin
      if Resolve_Local (C, C.Current, Name, Resolved_Local) then
         Arg := Byte (Resolved_Local);
         Get_Op := Lox_Chunk.OP_GET_LOCAL;
         Set_Op := Lox_Chunk.OP_SET_LOCAL;
      elsif Resolve_Upvalue (C, C.Current, Name, Resolved_Upvalue) then
         Arg := Byte (Resolved_Upvalue);
         Get_Op := Lox_Chunk.OP_GET_UPVALUE;
         Set_Op := Lox_Chunk.OP_SET_UPVALUE;
      else
         Arg := Identifier_Constant (C, Name);
         Get_Op := Lox_Chunk.OP_GET_GLOBAL;
         Set_Op := Lox_Chunk.OP_SET_GLOBAL;
      end if;

      if Can_Assign and then Match (C, Lox_Scanner.TOKEN_EQUAL) then
         Expression (C);
         Emit_Bytes (C, Set_Op, Arg);
      else
         Emit_Bytes (C, Get_Op, Arg);
      end if;
   end Named_Variable;

   procedure Variable (C : in out Compiler_Context; Can_Assign : Boolean) is
   begin
      Named_Variable (C, C.Parser.Previous, Can_Assign);
   end Variable;

   procedure Unary
     (C : in out Compiler_Context; Can_Assign : Boolean with Unreferenced)
   is
      Kind : constant Lox_Scanner.TokenType := C.Parser.Previous.Kind;
   begin
      Parse_Precedence (C, PREC_UNARY);

      case Kind is
         when Lox_Scanner.TOKEN_BANG  =>
            Emit_Byte (C, Lox_Chunk.OP_NOT);

         when Lox_Scanner.TOKEN_MINUS =>
            Emit_Byte (C, Lox_Chunk.OP_NEGATE);

         when others                  =>
            return;

      end case;
   end Unary;

   procedure Expression (C : in out Compiler_Context) is
   begin
      Parse_Precedence (C, PREC_ASSIGNMENT);
   end Expression;

   procedure Block (C : in out Compiler_Context) is
   begin
      while not Check (C, Lox_Scanner.TOKEN_RIGHT_BRACE)
        and then not Check (C, Lox_Scanner.TOKEN_EOF)
      loop
         Declaration (C);
      end loop;

      Consume (C, Lox_Scanner.TOKEN_RIGHT_BRACE, "Expect '}' after block.");
   end Block;

   procedure Function_Declaration (C : in out Compiler_Context) is
      Global : constant Byte := Parse_Variable (C, "Expect function name.");
   begin
      Mark_Initialized (C);
      Function_Definition (C, TYPE_FUNCTION);
      Define_Variable (C, Global);
   end Function_Declaration;

   procedure Function_Definition
     (C : in out Compiler_Context; Kind : Function_Kind)
   is
      Compiler : Compiler_Instance;
      Func     : Lox_Object.Object_Access := null;
   begin
      Init_Compiler (C, Compiler.Instance, Kind);
      Begin_Scope (C);

      Consume
        (C, Lox_Scanner.TOKEN_LEFT_PAREN, "Expect '(' after function name.");

      if not Check (C, Lox_Scanner.TOKEN_RIGHT_PAREN) then
         loop
            C.Current.Func.Arity :=
              Natural'Succ (C.Current.Func.Arity);
            if C.Current.Func.Arity > 255 then
               Error_At_Current (C, "Can't have more than 255 parameters.");
            end if;
            declare
               Arg : constant Byte := Parse_Variable (C, "");
            begin
               Define_Variable (C, Arg);
            end;
            exit when not Match (C, Lox_Scanner.TOKEN_COMMA);
         end loop;
      end if;

      Consume
        (C, Lox_Scanner.TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
      Consume
        (C, Lox_Scanner.TOKEN_LEFT_BRACE, "Expect '{' before function body.");
      Block (C);

      Func := End_Compiler (C);
      Emit_Bytes
        (C,
         Lox_Chunk.OP_CLOSURE,
         Make_Constant (C, Lox_Value.Make_Function (Func)));

      if Func.Upvalue_Count > 0 then
         for Upvalue of
           Compiler.Instance.Upvalues
             (Upvalue_Index'First
              .. Upvalue_Index (Natural'Pred (Func.Upvalue_Count)))
         loop
            Emit_Byte (C, (if Upvalue.Is_Local then 1 else 0));
            Emit_Byte (C, Byte (Upvalue.Index));
         end loop;
      end if;
   end Function_Definition;

   procedure Variable_Declaration (C : in out Compiler_Context) is
      Global : constant Byte := Parse_Variable (C, "Expect variable name.");
   begin
      if Match (C, Lox_Scanner.TOKEN_EQUAL) then
         Expression (C);
      else
         Emit_Byte (C, Lox_Chunk.OP_NIL);
      end if;
      Consume
        (C,
         Lox_Scanner.TOKEN_SEMICOLON,
         "Expect ';' after variable declaration.");

      Define_Variable (C, Global);
   end Variable_Declaration;

   procedure Expression_Statement (C : in out Compiler_Context) is
   begin
      Expression (C);
      Consume (C, Lox_Scanner.TOKEN_SEMICOLON, "Expect ';' after expression.");
      Emit_Byte (C, Lox_Chunk.OP_POP);
   end Expression_Statement;

   procedure For_Statement (C : in out Compiler_Context) is
      Loop_Start : Natural;
      Exit_Jump  : Maybe_Natural := None;
   begin
      Begin_Scope (C);

      Consume (C, Lox_Scanner.TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
      if Match (C, Lox_Scanner.TOKEN_SEMICOLON) then
         --  No initializer
         null;
      elsif Match (C, Lox_Scanner.TOKEN_VAR) then
         Variable_Declaration (C);
      else
         Expression_Statement (C);
      end if;

      Loop_Start := Natural (Current_Func (C).Chunk.Code.Length);
      if not Match (C, Lox_Scanner.TOKEN_SEMICOLON) then
         Expression (C);
         Consume
           (C,
            Lox_Scanner.TOKEN_SEMICOLON,
            "Expect ';' after loop condition.");

         --  Jump out of the loop if the condition is false
         Exit_Jump := Just (Emit_Jump (C, Lox_Chunk.OP_JUMP_IF_FALSE));
         Emit_Byte (C, Lox_Chunk.OP_POP);
      end if;

      if not Match (C, Lox_Scanner.TOKEN_RIGHT_PAREN) then
         declare
            Body_Jump       : constant Natural :=
              Emit_Jump (C, Lox_Chunk.OP_JUMP);
            Increment_Start : constant Natural :=
              Natural (Current_Func (C).Chunk.Code.Length);
         begin
            Expression (C);
            Emit_Byte (C, Lox_Chunk.OP_POP);
            Consume
              (C,
               Lox_Scanner.TOKEN_RIGHT_PAREN,
               "Expect ')' after for clauses.");

            Emit_Loop (C, Loop_Start);
            Loop_Start := Increment_Start;
            Patch_Jump (C, Body_Jump);
         end;
      end if;

      Statement (C);
      Emit_Loop (C, Loop_Start);

      if Exit_Jump.Has_Value then
         Patch_Jump (C, Exit_Jump.Value);
         Emit_Byte (C, Lox_Chunk.OP_POP);
      end if;

      End_Scope (C);
   end For_Statement;

   procedure Print_Statement (C : in out Compiler_Context) is
   begin
      Expression (C);
      Consume (C, Lox_Scanner.TOKEN_SEMICOLON, "Expect ';' after value.");
      Emit_Byte (C, Lox_Chunk.OP_PRINT);
   end Print_Statement;

   procedure Return_Statement (C : in out Compiler_Context) is
   begin
      if C.Current.Kind = TYPE_SCRIPT then
         Error (C, "Can't return from top-level code.");
      end if;

      if Match (C, Lox_Scanner.TOKEN_SEMICOLON) then
         Emit_Return (C);
      else
         Expression (C);
         Consume
           (C, Lox_Scanner.TOKEN_SEMICOLON, "Expect ';' after return value.");
         Emit_Byte (C, Lox_Chunk.OP_RETURN);
      end if;
   end Return_Statement;

   procedure While_Statement (C : in out Compiler_Context) is
      Loop_Start : constant Natural :=
        Natural (Current_Func (C).Chunk.Code.Length);
   begin
      Consume (C, Lox_Scanner.TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
      Expression (C);
      Consume
        (C, Lox_Scanner.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

      declare
         Exit_Jump : constant Natural :=
           Emit_Jump (C, Lox_Chunk.OP_JUMP_IF_FALSE);
      begin
         Emit_Byte (C, Lox_Chunk.OP_POP);
         Statement (C);
         Emit_Loop (C, Loop_Start);

         Patch_Jump (C, Exit_Jump);
         Emit_Byte (C, Lox_Chunk.OP_POP);
      end;
   end While_Statement;

   procedure If_Statement (C : in out Compiler_Context) is
   begin
      Consume (C, Lox_Scanner.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
      Expression (C);
      Consume
        (C, Lox_Scanner.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

      declare
         Then_Jump : Natural;
         Else_Jump : Natural;
      begin
         Then_Jump := Emit_Jump (C, Lox_Chunk.OP_JUMP_IF_FALSE);
         Emit_Byte (C, Lox_Chunk.OP_POP);
         Statement (C);

         Else_Jump := Emit_Jump (C, Lox_Chunk.OP_JUMP);

         Patch_Jump (C, Then_Jump);
         Emit_Byte (C, Lox_Chunk.OP_POP);

         if Match (C, Lox_Scanner.TOKEN_ELSE) then
            Statement (C);
         end if;
         Patch_Jump (C, Else_Jump);
      end;
   end If_Statement;

   procedure Switch_Statement (C : in out Compiler_Context) is
      Next_Case : Natural;
      Exit_Jump : Natural;
      End_Jump  : Natural;
   begin
      Consume (C, Lox_Scanner.TOKEN_LEFT_PAREN, "Expect '(' after 'switch'.");
      Expression (C);
      Consume
        (C,
         Lox_Scanner.TOKEN_RIGHT_PAREN,
         "Expect ')' after switch expression.");
      Consume (C, Lox_Scanner.TOKEN_LEFT_BRACE, "Expect '{'.");

      Emit_Byte (C, Lox_Chunk.OP_FALSE);
      Next_Case := Emit_Jump (C, Lox_Chunk.OP_JUMP);

      Exit_Jump := Natural (Current_Func (C).Chunk.Code.Length);
      End_Jump := Emit_Jump (C, Lox_Chunk.OP_JUMP);

      --  case
      while Match (C, Lox_Scanner.TOKEN_CASE) loop
         Patch_Jump (C, Next_Case);
         Emit_Byte (C, Lox_Chunk.OP_POP); --  OP_EQUAL result

         Emit_Byte (C, Lox_Chunk.OP_PUSH);
         Expression (C);
         Emit_Byte (C, Lox_Chunk.OP_EQUAL);
         Next_Case := Emit_Jump (C, Lox_Chunk.OP_JUMP_IF_FALSE);

         --  matching case
         Emit_Byte (C, Lox_Chunk.OP_POP); --  OP_EQUAL result
         Consume
           (C, Lox_Scanner.TOKEN_COLON, "Expect ':' after case expression.");

         while not Check (C, Lox_Scanner.TOKEN_RIGHT_BRACE)
           and then not Check (C, Lox_Scanner.TOKEN_CASE)
           and then not Check (C, Lox_Scanner.TOKEN_DEFAULT)
         loop
            Statement (C);
         end loop;

         Emit_Loop (C, Exit_Jump);
      end loop;

      --  no matching case
      Patch_Jump (C, Next_Case);
      Emit_Byte (C, Lox_Chunk.OP_POP); --  OP_EQUAL result

      --  default
      if Match (C, Lox_Scanner.TOKEN_DEFAULT) then
         Consume (C, Lox_Scanner.TOKEN_COLON, "Expect ':' after 'default'.");

         while not Check (C, Lox_Scanner.TOKEN_RIGHT_BRACE) loop
            Statement (C);
         end loop;
      end if;

      Patch_Jump (C, End_Jump);
      Consume (C, Lox_Scanner.TOKEN_RIGHT_BRACE, "Expect '}'.");

      Emit_Byte (C, Lox_Chunk.OP_POP); --  switch expression
   end Switch_Statement;

   procedure Declaration (C : in out Compiler_Context) is
   begin
      if Match (C, Lox_Scanner.TOKEN_FUN) then
         Function_Declaration (C);
      elsif Match (C, Lox_Scanner.TOKEN_VAR) then
         Variable_Declaration (C);
      else
         Statement (C);
      end if;

      if C.Parser.Panic_Mode then
         Synchronize (C);
      end if;
   end Declaration;

   procedure Statement (C : in out Compiler_Context) is
   begin
      if Match (C, Lox_Scanner.TOKEN_PRINT) then
         Print_Statement (C);
      elsif Match (C, Lox_Scanner.TOKEN_FOR) then
         For_Statement (C);
      elsif Match (C, Lox_Scanner.TOKEN_IF) then
         If_Statement (C);
      elsif Match (C, Lox_Scanner.TOKEN_RETURN) then
         Return_Statement (C);
      elsif Match (C, Lox_Scanner.TOKEN_WHILE) then
         While_Statement (C);
      elsif Match (C, Lox_Scanner.TOKEN_SWITCH) then
         Switch_Statement (C);
      elsif Match (C, Lox_Scanner.TOKEN_LEFT_BRACE) then
         Begin_Scope (C);
         Block (C);
         End_Scope (C);
      else
         Expression_Statement (C);
      end if;
   end Statement;

   procedure Parse_Precedence
     (C : in out Compiler_Context; Precedence : Precedence_Type)
   is
      Prefix_Rule : Parse_Fn := null;
      Can_Assign  : Boolean := False;
   begin
      Advance (C);
      Prefix_Rule := Get_Rule (C.Parser.Previous.Kind).Prefix;
      if Prefix_Rule = null then
         Error (C, "Expect expression.");
         return;
      end if;

      Can_Assign := Precedence <= PREC_ASSIGNMENT;
      Prefix_Rule (C, Can_Assign);

      while Precedence <= Get_Rule (C.Parser.Current.Kind).Precedence loop
         Advance (C);
         declare
            Infix_Rule : constant Parse_Fn :=
              Get_Rule (C.Parser.Previous.Kind).Infix;
         begin
            Infix_Rule (C, Can_Assign);
         end;
      end loop;

      if Can_Assign and then Match (C, Lox_Scanner.TOKEN_EQUAL) then
         Error (C, "Invalid assignment target.");
      end if;
   end Parse_Precedence;

   function Identifier_Constant
     (C : in out Compiler_Context; Token : Lox_Scanner.Token) return Byte is
   begin
      return Make_Constant (C, Lox_Value.Make_String (Token.Lexeme));
   end Identifier_Constant;

   function Identifiers_Equal
     (A : Lox_Scanner.Token; B : Lox_Scanner.Token) return Boolean
   is
      use type Lox_Scanner.Unbounded_String;
   begin
      return A.Lexeme = B.Lexeme;
   end Identifiers_Equal;

   function Resolve_Local
     (C        : in out Compiler_Context;
      Compiler : Compiler_Access;
      Name     : Lox_Scanner.Token;
      Index    : out Local_Index) return Boolean is
   begin
      if Compiler.Local_Count = 0 then
         return False;
      end if;

      for I in reverse
        Local_Index'First .. Local_Index (Natural'Pred (Compiler.Local_Count))
      loop
         if Identifiers_Equal (Name, Compiler.Locals (I).Name) then
            if not Compiler.Locals (I).Depth.Has_Value then
               Error (C, "Can't read local variable in its own initializer.");
            end if;
            Index := I;
            return True;
         end if;
      end loop;

      return False;
   end Resolve_Local;

   procedure Add_Local (C : in out Compiler_Context; Name : Lox_Scanner.Token)
   is
   begin
      if C.Current.Local_Count > Natural (Local_Index'Last) then
         Error (C, "Too many local variables in function.");
         return;
      end if;

      declare
         Local : Local_Type renames
           C.Current.Locals (Local_Index (C.Current.Local_Count));
      begin
         C.Current.Local_Count := Natural'Succ (C.Current.Local_Count);
         Local.Name := Name;
         Local.Depth := None;
         Local.Is_Captured := False;
      end;
   end Add_Local;

   function Resolve_Upvalue
     (C        : in out Compiler_Context;
      Compiler : Compiler_Access;
      Name     : Lox_Scanner.Token;
      Index    : out Upvalue_Index) return Boolean
   is
      Local   : Local_Index;
      Upvalue : Upvalue_Index;
   begin
      if Compiler.Enclosing = null then
         return False;
      end if;

      if Resolve_Local (C, Compiler.Enclosing, Name, Local) then
         Compiler.Enclosing.Locals (Local).Is_Captured := True;
         Index := Add_Upvalue (C, Compiler, Upvalue_Slot_Index (Local), True);
         return True;
      end if;

      if Resolve_Upvalue (C, Compiler.Enclosing, Name, Upvalue) then
         Index :=
           Add_Upvalue (C, Compiler, Upvalue_Slot_Index (Upvalue), False);
         return True;
      end if;

      return False;
   end Resolve_Upvalue;

   function Add_Upvalue
     (C        : in out Compiler_Context;
      Compiler : Compiler_Access;
      Index    : Upvalue_Slot_Index;
      Is_Local : Boolean) return Upvalue_Index
   is
      Upvalue_Count : constant Natural := Compiler.Func.Upvalue_Count;
   begin
      if Upvalue_Count > 0 then
         for I in
           Upvalue_Index'First .. Upvalue_Index (Natural'Pred (Upvalue_Count))
         loop
            declare
               Upvalue : Upvalue_Type renames Compiler.Upvalues (I);
            begin
               if Upvalue.Index = Index and then Upvalue.Is_Local = Is_Local
               then
                  return I;
               end if;
            end;
         end loop;
      end if;

      if Upvalue_Count > Natural (Upvalue_Index'Last) then
         Error (C, "Too many closure variables in function.");
         return 0;
      end if;

      Compiler.Upvalues (Upvalue_Index (Upvalue_Count)).Is_Local := Is_Local;
      Compiler.Upvalues (Upvalue_Index (Upvalue_Count)).Index := Index;
      Compiler.Func.Upvalue_Count :=
        Natural'Succ (Compiler.Func.Upvalue_Count);

      return Upvalue_Index (Upvalue_Count);
   end Add_Upvalue;

   procedure Declare_Variable (C : in out Compiler_Context) is
      procedure Check_Duplicate_In_Same_Scope (Name : Lox_Scanner.Token) is
      begin
         if C.Current.Local_Count = 0 then
            return;
         end if;

         for I in reverse
           Local_Index'First
           .. Local_Index (Natural'Pred (C.Current.Local_Count))
         loop
            declare
               Local : Local_Type renames C.Current.Locals (I);
            begin
               exit when
                 Local.Depth.Has_Value
                 and then Local.Depth.Value < C.Current.Scope_Depth;

               if Identifiers_Equal (Name, Local.Name) then
                  Error
                    (C, "Already a variable with this name in this scope.");
               end if;
            end;
         end loop;
      end Check_Duplicate_In_Same_Scope;

   begin
      if C.Current.Scope_Depth = 0 then
         return;
      end if;

      declare
         Name : constant Lox_Scanner.Token := C.Parser.Previous;
      begin
         Check_Duplicate_In_Same_Scope (Name);

         Add_Local (C, Name);
      end;
   end Declare_Variable;

   function Parse_Variable
     (C : in out Compiler_Context; Error_Message : String) return Byte is
   begin
      Consume (C, Lox_Scanner.TOKEN_IDENTIFIER, Error_Message);

      Declare_Variable (C);
      if C.Current.Scope_Depth > 0 then
         return 0;
      end if;

      return Identifier_Constant (C, C.Parser.Previous);
   end Parse_Variable;

   procedure Mark_Initialized (C : in out Compiler_Context) is
   begin
      if C.Current.Scope_Depth = 0 then
         return;
      end if;

      declare
         Top_Index : constant Local_Index :=
           Local_Index (Natural'Pred (C.Current.Local_Count));
      begin
         C.Current.Locals (Top_Index).Depth := Just (C.Current.Scope_Depth);
      end;
   end Mark_Initialized;

   procedure Define_Variable (C : in out Compiler_Context; Global : Byte) is
   begin
      if C.Current.Scope_Depth > 0 then
         Mark_Initialized (C);
         return;
      end if;

      Emit_Bytes (C, Lox_Chunk.OP_DEFINE_GLOBAL, Global);
   end Define_Variable;

   function Argument_List (C : in out Compiler_Context) return Byte is
      Arg_Count : Byte := 0;
   begin
      if not Check (C, Lox_Scanner.TOKEN_RIGHT_PAREN) then
         loop
            Expression (C);
            if Arg_Count = 255 then
               Error (C, "Can't have more than 255 arguments.");
            end if;
            Arg_Count := Byte'Succ (Arg_Count);
            exit when not Match (C, Lox_Scanner.TOKEN_COMMA);
         end loop;
      end if;
      Consume
        (C, Lox_Scanner.TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
      return Arg_Count;
   end Argument_List;

   function Get_Rule (Kind : Lox_Scanner.TokenType) return Parse_Rule is
   begin
      return Rules (Kind);
   end Get_Rule;

   procedure Synchronize (C : in out Compiler_Context) is
   begin
      C.Parser.Panic_Mode := False;

      while C.Parser.Current.Kind /= Lox_Scanner.TOKEN_EOF loop
         if C.Parser.Previous.Kind = Lox_Scanner.TOKEN_SEMICOLON then
            return;
         end if;
         case C.Parser.Current.Kind is
            when Lox_Scanner.TOKEN_CLASS
               | Lox_Scanner.TOKEN_FUN
               | Lox_Scanner.TOKEN_VAR
               | Lox_Scanner.TOKEN_FOR
               | Lox_Scanner.TOKEN_IF
               | Lox_Scanner.TOKEN_WHILE
               | Lox_Scanner.TOKEN_PRINT
               | Lox_Scanner.TOKEN_RETURN =>
               return;

            when others                   =>
               null;
         end case;

         Advance (C);
      end loop;
   end Synchronize;

end Lox_Compiler;
