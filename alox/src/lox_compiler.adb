with Debug;

with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Lox_Compiler is

   package Unbounded renames Ada.Strings.Unbounded;

   type Rules_Type is array (Lox_Scanner.TokenType) of Parse_Rule;
   Rules : constant Rules_Type :=
     [Lox_Scanner.TOKEN_LEFT_PAREN    => (Grouping'Access, null, PREC_NONE),
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
     (Source : Lox_Scanner.Source_Code; Chunk : Lox_Chunk.Chunk_Access)
      return Boolean
   is
      C        : Compiler_Context;
      Compiler : Compiler_Instance;
   begin
      C.Scanner := Lox_Scanner.Init (Source);
      Init_Compiler (C, Compiler.Instance);
      C.Compiling_Chunk := Chunk;

      C.Parser.Had_Error := False;
      C.Parser.Panic_Mode := False;

      Advance (C);

      while not Match (C, Lox_Scanner.TOKEN_EOF) loop
         Declaration (C);
      end loop;

      End_Compiler (C);

      return not C.Parser.Had_Error;
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

   function Current_Chunk
     (C : in out Compiler_Context) return Lox_Chunk.Chunk_Access is
   begin
      return C.Compiling_Chunk;
   end Current_Chunk;

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
      Lox_Chunk.Write (Current_Chunk (C).all, Value, C.Parser.Previous.Line);
   end Emit_Byte;

   procedure Emit_Byte
     (C : in out Compiler_Context; Op_Code : Lox_Chunk.Op_Code) is
   begin
      Lox_Chunk.Write (Current_Chunk (C).all, Op_Code, C.Parser.Previous.Line);
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
           Natural (Current_Chunk (C).Code.Length) - Loop_Start + 2;
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
           Natural (Current_Chunk (C).Code.Length);
      begin
         Emit_Byte (C, Byte'Last);
         Emit_Byte (C, Byte'Last);
         return Patch_Index;
      end;
   end Emit_Jump;

   procedure Emit_Return (C : in out Compiler_Context) is
   begin
      Emit_Byte (C, Lox_Chunk.OP_RETURN);
   end Emit_Return;

   function Make_Constant
     (C : in out Compiler_Context; Value : Lox_Value.Value) return Byte
   is
      Id : constant Natural :=
        Lox_Chunk.Add_Constant (Current_Chunk (C).all, Value);
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
        Natural (Current_Chunk (C).Code.Length) - Offset - 2;
   begin
      if Jump > Natural (Short'Last) then
         Error (C, "Too much code to jump over.");
         return;
      end if;

      Current_Chunk (C).Code (Offset) := Byte (Jump / 256);
      Current_Chunk (C).Code (Natural'Succ (Offset)) := Byte (Jump mod 256);
   end Patch_Jump;

   procedure Init_Compiler
     (C : in out Compiler_Context; Compiler : Compiler_Access) is
   begin
      Compiler.Local_Count := 0;
      Compiler.Scope_Depth := 0;
      C.Current := Compiler;
   end Init_Compiler;

   procedure End_Compiler (C : in out Compiler_Context) is
   begin
      Emit_Return (C);

      if Debug.Print_Code_Enabled then
         if not C.Parser.Had_Error then
            Debug.Disassemble_Chunk
              (Lox_Chunk.Chunk_Read_Access (Current_Chunk (C)), "code");
         end if;
      end if;
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
   begin
      C.Current.Scope_Depth := Natural'Pred (C.Current.Scope_Depth);

      while Has_Variable_In_Scope loop
         Emit_Byte (C, Lox_Chunk.OP_POP);
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
      Resolved : Local_Index;
      Arg      : Byte;
      Get_Op   : Lox_Chunk.Op_Code;
      Set_Op   : Lox_Chunk.Op_Code;
   begin
      if Resolve_Local (C, C.Current, Name, Resolved) then
         Arg := Byte (Resolved);
         Get_Op := Lox_Chunk.OP_GET_LOCAL;
         Set_Op := Lox_Chunk.OP_SET_LOCAL;
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

      Loop_Start := Natural (Current_Chunk (C).Code.Length);
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
              Natural (Current_Chunk (C).Code.Length);
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

   procedure While_Statement (C : in out Compiler_Context) is
      Loop_Start : constant Natural := Natural (Current_Chunk (C).Code.Length);
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

   procedure Declaration (C : in out Compiler_Context) is
   begin
      if Match (C, Lox_Scanner.TOKEN_VAR) then
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
      elsif Match (C, Lox_Scanner.TOKEN_WHILE) then
         While_Statement (C);
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
      end;
   end Add_Local;

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
      Top_Index : constant Local_Index :=
        Local_Index (Natural'Pred (C.Current.Local_Count));
   begin
      C.Current.Locals (Top_Index).Depth := Just (C.Current.Scope_Depth);
   end Mark_Initialized;

   procedure Define_Variable (C : in out Compiler_Context; Global : Byte) is
   begin
      if C.Current.Scope_Depth > 0 then
         Mark_Initialized (C);
         return;
      end if;

      Emit_Bytes (C, Lox_Chunk.OP_DEFINE_GLOBAL, Global);
   end Define_Variable;

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
