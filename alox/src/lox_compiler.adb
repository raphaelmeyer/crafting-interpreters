with Debug;

with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Lox_VM;

package body Lox_Compiler is

   package Unbounded renames Ada.Strings.Unbounded;

   Context : Compiler_Context;

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
      Compiler : Compiler_Instance;
      Func     : Lox_Object.Object_Access := null;
   begin
      Context.Scanner := Lox_Scanner.Init (Source);
      Init_Compiler (Compiler.Instance, TYPE_SCRIPT);

      Context.Parser.Had_Error := False;
      Context.Parser.Panic_Mode := False;

      Advance;

      while not Match (Lox_Scanner.TOKEN_EOF) loop
         Declaration;
      end loop;

      Func := End_Compiler;

      if Context.Parser.Had_Error then
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

   function Current_Func return Lox_Object.Object_Access is
   begin
      return Context.Current.Func;
   end Current_Func;

   procedure Error_At (Token : Lox_Scanner.Token; Message : String) is
   begin
      if Context.Parser.Panic_Mode then
         return;
      end if;
      Context.Parser.Panic_Mode := True;

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
      Context.Parser.Had_Error := True;
   end Error_At;

   procedure Error (Message : String) is
   begin
      Error_At (Context.Parser.Previous, Message);
   end Error;

   procedure Error_At_Current (Message : String) is
   begin
      Error_At (Context.Parser.Current, Message);
   end Error_At_Current;

   procedure Advance is
   begin
      Context.Parser.Previous := Context.Parser.Current;

      loop
         Context.Parser.Current := Lox_Scanner.Scan_Token (Context.Scanner);
         exit when Context.Parser.Current.Kind /= Lox_Scanner.TOKEN_ERROR;

         Error_At_Current
           (Lox_Scanner.Unbounded.To_String (Context.Parser.Current.Lexeme));
      end loop;
   end Advance;

   procedure Consume (Kind : Lox_Scanner.TokenType; Message : String) is
   begin
      if Context.Parser.Current.Kind = Kind then
         Advance;
         return;
      end if;

      Error_At_Current (Message);
   end Consume;

   function Check (Kind : Lox_Scanner.TokenType) return Boolean is
   begin
      return Context.Parser.Current.Kind = Kind;
   end Check;

   function Match (Kind : Lox_Scanner.TokenType) return Boolean is
   begin
      if not Check (Kind) then
         return False;
      end if;
      Advance;
      return True;
   end Match;

   procedure Emit_Byte (Value : Byte) is
   begin
      Lox_Chunk.Write
        (Current_Func.Chunk, Value, Context.Parser.Previous.Line);
   end Emit_Byte;

   procedure Emit_Byte (Op_Code : Lox_Chunk.Op_Code) is
   begin
      Lox_Chunk.Write
        (Current_Func.Chunk, Op_Code, Context.Parser.Previous.Line);
   end Emit_Byte;

   procedure Emit_Bytes (Byte_1 : Lox_Chunk.Op_Code; Byte_2 : Byte) is
   begin
      Emit_Byte (Byte_1);
      Emit_Byte (Byte_2);
   end Emit_Bytes;

   procedure Emit_Bytes
     (Byte_1 : Lox_Chunk.Op_Code; Byte_2 : Lox_Chunk.Op_Code) is
   begin
      Emit_Byte (Byte_1);
      Emit_Byte (Byte_2);
   end Emit_Bytes;

   procedure Emit_Loop (Loop_Start : Natural) is
   begin
      Emit_Byte (Lox_Chunk.OP_LOOP);

      declare
         Offset : constant Natural :=
           Natural (Current_Func.Chunk.Code.Length) - Loop_Start + 2;
      begin
         if Offset > Natural (Short'Last) then
            Error ("Loop body too large.");
            return;
         end if;

         Emit_Byte (Byte (Offset / 256));
         Emit_Byte (Byte (Offset mod 256));
      end;
   end Emit_Loop;

   function Emit_Jump (Instruction : Lox_Chunk.Op_Code) return Natural is
   begin
      Emit_Byte (Instruction);
      declare
         Patch_Index : constant Natural :=
           Natural (Current_Func.Chunk.Code.Length);
      begin
         Emit_Byte (Byte'Last);
         Emit_Byte (Byte'Last);
         return Patch_Index;
      end;
   end Emit_Jump;

   procedure Emit_Return is
   begin
      Emit_Byte (Lox_Chunk.OP_NIL);
      Emit_Byte (Lox_Chunk.OP_RETURN);
   end Emit_Return;

   function Make_Constant (Value : Lox_Value.Value) return Byte is
      Id : constant Natural :=
        Lox_Chunk.Add_Constant (Current_Func.Chunk, Value);
   begin
      if Id > Natural (Byte'Last) then
         Error ("Too many constants in one chunk.");
         return 0;
      end if;

      return Byte (Id);
   end Make_Constant;

   procedure Emit_Constant (Value : Lox_Value.Value) is
      Id : constant Byte := Make_Constant (Value);
   begin
      Emit_Bytes (Lox_Chunk.OP_CONSTANT, Id);
   end Emit_Constant;

   procedure Patch_Jump (Offset : Natural) is
      Jump : constant Natural :=
        Natural (Current_Func.Chunk.Code.Length) - Offset - 2;
   begin
      if Jump > Natural (Short'Last) then
         Error ("Too much code to jump over.");
         return;
      end if;

      Current_Func.Chunk.Code (Offset) := Byte (Jump / 256);
      Current_Func.Chunk.Code (Natural'Succ (Offset)) := Byte (Jump mod 256);
   end Patch_Jump;

   procedure Iterate_Current_Functions (Action : not null Object_Action) is
      Compiler : Compiler_Access := Context.Current;
   begin
      while Compiler /= null loop
         Action (Compiler.Func);
         Compiler := Compiler_Access (Compiler.Enclosing);
      end loop;
   end Iterate_Current_Functions;

   procedure Init_Compiler (Compiler : Compiler_Access; Kind : Function_Kind)
   is
   begin
      Compiler.Enclosing := Context.Current;
      Compiler.Func := null;
      Compiler.Kind := Kind;
      Compiler.Local_Count := 0;
      Compiler.Scope_Depth := 0;
      Compiler.Func := Lox_VM.New_Function;
      Context.Current := Compiler;

      if Kind /= TYPE_SCRIPT then
         Context.Current.Func.Name := Context.Parser.Previous.Lexeme;
      end if;

      declare
         Local : Local_Type renames
           Context.Current.Locals (Local_Index (Context.Current.Local_Count));
      begin
         Context.Current.Local_Count :=
           Natural'Succ (Context.Current.Local_Count);
         Local.Depth := Just (0);
         Local.Is_Captured := False;
         Local.Name.Lexeme := Unbounded.Null_Unbounded_String;
      end;
   end Init_Compiler;

   function End_Compiler return Lox_Object.Object_Access is
      Func : Lox_Object.Object_Access := null;
      use type Unbounded.Unbounded_String;
   begin
      Emit_Return;
      Func := Context.Current.Func;

      if Debug.Print_Code_Enabled then
         if not Context.Parser.Had_Error then
            declare
               Name : constant String :=
                 (if Func.Name = Unbounded.Null_Unbounded_String
                  then "<script>"
                  else Unbounded.To_String (Func.Name));
            begin
               Debug.Disassemble_Chunk (Current_Func.Chunk, Name);
            end;
         end if;
      end if;

      Context.Current := Compiler_Access (Context.Current.Enclosing);
      return Func;
   end End_Compiler;

   procedure Begin_Scope is
   begin
      Context.Current.Scope_Depth :=
        Natural'Succ (Context.Current.Scope_Depth);
   end Begin_Scope;

   procedure End_Scope is
      function Has_Variable_In_Scope return Boolean is
      begin
         if Context.Current.Local_Count = 0 then
            return False;
         end if;
         declare
            Top_Index : constant Local_Index :=
              Local_Index (Natural'Pred (Context.Current.Local_Count));
         begin
            return
              Context.Current.Locals (Top_Index).Depth.Value
              > Context.Current.Scope_Depth;
         end;
      end Has_Variable_In_Scope;

      function Is_Captured return Boolean is
         Top_Index : constant Local_Index :=
           Local_Index (Natural'Pred (Context.Current.Local_Count));
      begin
         return Context.Current.Locals (Top_Index).Is_Captured;
      end Is_Captured;
   begin
      Context.Current.Scope_Depth :=
        Natural'Pred (Context.Current.Scope_Depth);

      while Has_Variable_In_Scope loop
         if Is_Captured then
            Emit_Byte (Lox_Chunk.OP_CLOSE_UPVALUE);
         else
            Emit_Byte (Lox_Chunk.OP_POP);
         end if;
         Context.Current.Local_Count :=
           Natural'Pred (Context.Current.Local_Count);
      end loop;
   end End_Scope;

   procedure Binary (Can_Assign : Boolean with Unreferenced) is
      Operator_Type : constant Lox_Scanner.TokenType :=
        Context.Parser.Previous.Kind;
      Rule          : constant Parse_Rule := Get_Rule (Operator_Type);
   begin
      Parse_Precedence (Precedence_Type'Succ (Rule.Precedence));

      case Operator_Type is
         when Lox_Scanner.TOKEN_BANG_EQUAL    =>
            Emit_Bytes (Lox_Chunk.OP_EQUAL, Lox_Chunk.OP_NOT);

         when Lox_Scanner.TOKEN_EQUAL_EQUAL   =>
            Emit_Byte (Lox_Chunk.OP_EQUAL);

         when Lox_Scanner.TOKEN_GREATER       =>
            Emit_Byte (Lox_Chunk.OP_GREATER);

         when Lox_Scanner.TOKEN_GREATER_EQUAL =>
            Emit_Bytes (Lox_Chunk.OP_LESS, Lox_Chunk.OP_NOT);

         when Lox_Scanner.TOKEN_LESS          =>
            Emit_Byte (Lox_Chunk.OP_LESS);

         when Lox_Scanner.TOKEN_LESS_EQUAL    =>
            Emit_Bytes (Lox_Chunk.OP_GREATER, Lox_Chunk.OP_NOT);

         when Lox_Scanner.TOKEN_PLUS          =>
            Emit_Byte (Lox_Chunk.OP_ADD);

         when Lox_Scanner.TOKEN_MINUS         =>
            Emit_Byte (Lox_Chunk.OP_SUBTRACT);

         when Lox_Scanner.TOKEN_STAR          =>
            Emit_Byte (Lox_Chunk.OP_MULTIPLY);

         when Lox_Scanner.TOKEN_SLASH         =>
            Emit_Byte (Lox_Chunk.OP_DIVIDE);

         when others                          =>
            return;
      end case;
   end Binary;

   procedure Call (Can_Assign : Boolean with Unreferenced) is
      Arg_Count : constant Byte := Argument_List;
   begin
      Emit_Bytes (Lox_Chunk.OP_CALL, Arg_Count);
   end Call;

   procedure Literal (Can_Assign : Boolean with Unreferenced) is
   begin
      case Context.Parser.Previous.Kind is
         when Lox_Scanner.TOKEN_FALSE =>
            Emit_Byte (Lox_Chunk.OP_FALSE);

         when Lox_Scanner.TOKEN_NIL   =>
            Emit_Byte (Lox_Chunk.OP_NIL);

         when Lox_Scanner.TOKEN_TRUE  =>
            Emit_Byte (Lox_Chunk.OP_TRUE);

         when others                  =>
            return;
      end case;
   end Literal;

   procedure Grouping (Can_Assign : Boolean with Unreferenced) is
   begin
      Expression;
      Consume (Lox_Scanner.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
   end Grouping;

   procedure Number (Can_Assign : Boolean with Unreferenced) is
      Lexeme : constant String :=
        Unbounded.To_String (Context.Parser.Previous.Lexeme);
      Value  : constant Lox_Value.Lox_Float :=
        (Is_Valid => True, Value => Long_Float'Value (Lexeme));
   begin
      Emit_Constant (Lox_Value.Make_Number (Value));
   end Number;

   procedure String_Literal (Can_Assign : Boolean with Unreferenced) is
      From : constant Positive := Positive'Succ (1);
      To   : constant Natural :=
        Natural'Pred (Unbounded.Length (Context.Parser.Previous.Lexeme));
      Str  : constant Lox_Value.Unbounded_String :=
        Unbounded.Unbounded_Slice (Context.Parser.Previous.Lexeme, From, To);
   begin
      Emit_Constant (Lox_Value.Make_String (Str));
   end String_Literal;

   procedure Logical_And (Can_Assign : Boolean with Unreferenced) is
      End_Jump : constant Natural := Emit_Jump (Lox_Chunk.OP_JUMP_IF_FALSE);
   begin
      Emit_Byte (Lox_Chunk.OP_POP);
      Parse_Precedence (PREC_AND);

      Patch_Jump (End_Jump);
   end Logical_And;

   procedure Logical_Or (Can_Assign : Boolean with Unreferenced) is
      Else_Jump : constant Natural := Emit_Jump (Lox_Chunk.OP_JUMP_IF_FALSE);
      End_Jump  : constant Natural := Emit_Jump (Lox_Chunk.OP_JUMP);
   begin
      Patch_Jump (Else_Jump);
      Emit_Byte (Lox_Chunk.OP_POP);

      Parse_Precedence (PREC_OR);
      Patch_Jump (End_Jump);
   end Logical_Or;

   procedure Named_Variable (Name : Lox_Scanner.Token; Can_Assign : Boolean) is
      Resolved_Local   : Local_Index;
      Resolved_Upvalue : Upvalue_Index;
      Arg              : Byte;
      Get_Op           : Lox_Chunk.Op_Code;
      Set_Op           : Lox_Chunk.Op_Code;
   begin
      if Resolve_Local (Context.Current, Name, Resolved_Local) then
         Arg := Byte (Resolved_Local);
         Get_Op := Lox_Chunk.OP_GET_LOCAL;
         Set_Op := Lox_Chunk.OP_SET_LOCAL;
      elsif Resolve_Upvalue (Context.Current, Name, Resolved_Upvalue) then
         Arg := Byte (Resolved_Upvalue);
         Get_Op := Lox_Chunk.OP_GET_UPVALUE;
         Set_Op := Lox_Chunk.OP_SET_UPVALUE;
      else
         Arg := Identifier_Constant (Name);
         Get_Op := Lox_Chunk.OP_GET_GLOBAL;
         Set_Op := Lox_Chunk.OP_SET_GLOBAL;
      end if;

      if Can_Assign and then Match (Lox_Scanner.TOKEN_EQUAL) then
         Expression;
         Emit_Bytes (Set_Op, Arg);
      else
         Emit_Bytes (Get_Op, Arg);
      end if;
   end Named_Variable;

   procedure Variable (Can_Assign : Boolean) is
   begin
      Named_Variable (Context.Parser.Previous, Can_Assign);
   end Variable;

   procedure Unary (Can_Assign : Boolean with Unreferenced) is
      Kind : constant Lox_Scanner.TokenType := Context.Parser.Previous.Kind;
   begin
      Parse_Precedence (PREC_UNARY);

      case Kind is
         when Lox_Scanner.TOKEN_BANG  =>
            Emit_Byte (Lox_Chunk.OP_NOT);

         when Lox_Scanner.TOKEN_MINUS =>
            Emit_Byte (Lox_Chunk.OP_NEGATE);

         when others                  =>
            return;

      end case;
   end Unary;

   procedure Expression is
   begin
      Parse_Precedence (PREC_ASSIGNMENT);
   end Expression;

   procedure Block is
   begin
      while not Check (Lox_Scanner.TOKEN_RIGHT_BRACE)
        and then not Check (Lox_Scanner.TOKEN_EOF)
      loop
         Declaration;
      end loop;

      Consume (Lox_Scanner.TOKEN_RIGHT_BRACE, "Expect '}' after block.");
   end Block;

   procedure Function_Declaration is
      Global : constant Byte := Parse_Variable ("Expect function name.");
   begin
      Mark_Initialized;
      Function_Definition (TYPE_FUNCTION);
      Define_Variable (Global);
   end Function_Declaration;

   procedure Function_Definition (Kind : Function_Kind) is
      Compiler : Compiler_Instance;
      Func     : Lox_Object.Object_Access := null;
   begin
      Init_Compiler (Compiler.Instance, Kind);
      Begin_Scope;

      Consume
        (Lox_Scanner.TOKEN_LEFT_PAREN, "Expect '(' after function name.");

      if not Check (Lox_Scanner.TOKEN_RIGHT_PAREN) then
         loop
            Context.Current.Func.Arity :=
              Natural'Succ (Context.Current.Func.Arity);
            if Context.Current.Func.Arity > 255 then
               Error_At_Current ("Can't have more than 255 parameters.");
            end if;
            declare
               Arg : constant Byte :=
                 Parse_Variable ("Expect parameter name.");
            begin
               Define_Variable (Arg);
            end;
            exit when not Match (Lox_Scanner.TOKEN_COMMA);
         end loop;
      end if;

      Consume (Lox_Scanner.TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
      Consume
        (Lox_Scanner.TOKEN_LEFT_BRACE, "Expect '{' before function body.");
      Block;

      Func := End_Compiler;
      Emit_Bytes
        (Lox_Chunk.OP_CLOSURE, Make_Constant (Lox_Value.Make_Function (Func)));

      if Func.Upvalue_Count > 0 then
         for Upvalue of
           Compiler.Instance.Upvalues
             (Upvalue_Index'First
              .. Upvalue_Index (Natural'Pred (Func.Upvalue_Count)))
         loop
            Emit_Byte ((if Upvalue.Is_Local then 1 else 0));
            Emit_Byte (Byte (Upvalue.Index));
         end loop;
      end if;
   end Function_Definition;

   procedure Class_Declaration is
      Name_Constant : Byte;
   begin
      Consume (Lox_Scanner.TOKEN_IDENTIFIER, "Expect class name.");
      Name_Constant := Identifier_Constant (Context.Parser.Previous);
      Declare_Variable;

      Emit_Bytes (Lox_Chunk.OP_CLASS, Name_Constant);
      Define_Variable (Name_Constant);

      Consume (Lox_Scanner.TOKEN_LEFT_BRACE, "Expect '{' before class body.");
      Consume (Lox_Scanner.TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
   end Class_Declaration;

   procedure Variable_Declaration is
      Global : constant Byte := Parse_Variable ("Expect variable name.");
   begin
      if Match (Lox_Scanner.TOKEN_EQUAL) then
         Expression;
      else
         Emit_Byte (Lox_Chunk.OP_NIL);
      end if;
      Consume
        (Lox_Scanner.TOKEN_SEMICOLON,
         "Expect ';' after variable declaration.");

      Define_Variable (Global);
   end Variable_Declaration;

   procedure Expression_Statement is
   begin
      Expression;
      Consume (Lox_Scanner.TOKEN_SEMICOLON, "Expect ';' after expression.");
      Emit_Byte (Lox_Chunk.OP_POP);
   end Expression_Statement;

   procedure For_Statement is
      Loop_Start : Natural;
      Exit_Jump  : Maybe_Natural := None;
   begin
      Begin_Scope;

      Consume (Lox_Scanner.TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
      if Match (Lox_Scanner.TOKEN_SEMICOLON) then
         --  No initializer
         null;
      elsif Match (Lox_Scanner.TOKEN_VAR) then
         Variable_Declaration;
      else
         Expression_Statement;
      end if;

      Loop_Start := Natural (Current_Func.Chunk.Code.Length);
      if not Match (Lox_Scanner.TOKEN_SEMICOLON) then
         Expression;
         Consume
           (Lox_Scanner.TOKEN_SEMICOLON, "Expect ';' after loop condition.");

         --  Jump out of the loop if the condition is false
         Exit_Jump := Just (Emit_Jump (Lox_Chunk.OP_JUMP_IF_FALSE));
         Emit_Byte (Lox_Chunk.OP_POP);
      end if;

      if not Match (Lox_Scanner.TOKEN_RIGHT_PAREN) then
         declare
            Body_Jump       : constant Natural :=
              Emit_Jump (Lox_Chunk.OP_JUMP);
            Increment_Start : constant Natural :=
              Natural (Current_Func.Chunk.Code.Length);
         begin
            Expression;
            Emit_Byte (Lox_Chunk.OP_POP);
            Consume
              (Lox_Scanner.TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

            Emit_Loop (Loop_Start);
            Loop_Start := Increment_Start;
            Patch_Jump (Body_Jump);
         end;
      end if;

      Statement;
      Emit_Loop (Loop_Start);

      if Exit_Jump.Has_Value then
         Patch_Jump (Exit_Jump.Value);
         Emit_Byte (Lox_Chunk.OP_POP);
      end if;

      End_Scope;
   end For_Statement;

   procedure Print_Statement is
   begin
      Expression;
      Consume (Lox_Scanner.TOKEN_SEMICOLON, "Expect ';' after value.");
      Emit_Byte (Lox_Chunk.OP_PRINT);
   end Print_Statement;

   procedure Return_Statement is
   begin
      if Context.Current.Kind = TYPE_SCRIPT then
         Error ("Can't return from top-level code.");
      end if;

      if Match (Lox_Scanner.TOKEN_SEMICOLON) then
         Emit_Return;
      else
         Expression;
         Consume
           (Lox_Scanner.TOKEN_SEMICOLON, "Expect ';' after return value.");
         Emit_Byte (Lox_Chunk.OP_RETURN);
      end if;
   end Return_Statement;

   procedure While_Statement is
      Loop_Start : constant Natural :=
        Natural (Current_Func.Chunk.Code.Length);
   begin
      Consume (Lox_Scanner.TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
      Expression;
      Consume (Lox_Scanner.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

      declare
         Exit_Jump : constant Natural :=
           Emit_Jump (Lox_Chunk.OP_JUMP_IF_FALSE);
      begin
         Emit_Byte (Lox_Chunk.OP_POP);
         Statement;
         Emit_Loop (Loop_Start);

         Patch_Jump (Exit_Jump);
         Emit_Byte (Lox_Chunk.OP_POP);
      end;
   end While_Statement;

   procedure If_Statement is
   begin
      Consume (Lox_Scanner.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
      Expression;
      Consume (Lox_Scanner.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

      declare
         Then_Jump : Natural;
         Else_Jump : Natural;
      begin
         Then_Jump := Emit_Jump (Lox_Chunk.OP_JUMP_IF_FALSE);
         Emit_Byte (Lox_Chunk.OP_POP);
         Statement;

         Else_Jump := Emit_Jump (Lox_Chunk.OP_JUMP);

         Patch_Jump (Then_Jump);
         Emit_Byte (Lox_Chunk.OP_POP);

         if Match (Lox_Scanner.TOKEN_ELSE) then
            Statement;
         end if;
         Patch_Jump (Else_Jump);
      end;
   end If_Statement;

   procedure Switch_Statement is
      Next_Case : Natural;
      Exit_Jump : Natural;
      End_Jump  : Natural;
   begin
      Consume (Lox_Scanner.TOKEN_LEFT_PAREN, "Expect '(' after 'switch'.");
      Expression;
      Consume
        (Lox_Scanner.TOKEN_RIGHT_PAREN, "Expect ')' after switch expression.");
      Consume (Lox_Scanner.TOKEN_LEFT_BRACE, "Expect '{'.");

      Emit_Byte (Lox_Chunk.OP_FALSE);
      Next_Case := Emit_Jump (Lox_Chunk.OP_JUMP);

      Exit_Jump := Natural (Current_Func.Chunk.Code.Length);
      End_Jump := Emit_Jump (Lox_Chunk.OP_JUMP);

      --  case
      while Match (Lox_Scanner.TOKEN_CASE) loop
         Patch_Jump (Next_Case);
         Emit_Byte (Lox_Chunk.OP_POP); --  OP_EQUAL result

         Emit_Byte (Lox_Chunk.OP_PUSH);
         Expression;
         Emit_Byte (Lox_Chunk.OP_EQUAL);
         Next_Case := Emit_Jump (Lox_Chunk.OP_JUMP_IF_FALSE);

         --  matching case
         Emit_Byte (Lox_Chunk.OP_POP); --  OP_EQUAL result
         Consume
           (Lox_Scanner.TOKEN_COLON, "Expect ':' after case expression.");

         while not Check (Lox_Scanner.TOKEN_RIGHT_BRACE)
           and then not Check (Lox_Scanner.TOKEN_CASE)
           and then not Check (Lox_Scanner.TOKEN_DEFAULT)
         loop
            Statement;
         end loop;

         Emit_Loop (Exit_Jump);
      end loop;

      --  no matching case
      Patch_Jump (Next_Case);
      Emit_Byte (Lox_Chunk.OP_POP); --  OP_EQUAL result

      --  default
      if Match (Lox_Scanner.TOKEN_DEFAULT) then
         Consume (Lox_Scanner.TOKEN_COLON, "Expect ':' after 'default'.");

         while not Check (Lox_Scanner.TOKEN_RIGHT_BRACE) loop
            Statement;
         end loop;
      end if;

      Patch_Jump (End_Jump);
      Consume (Lox_Scanner.TOKEN_RIGHT_BRACE, "Expect '}'.");

      Emit_Byte (Lox_Chunk.OP_POP); --  switch expression
   end Switch_Statement;

   procedure Declaration is
   begin
      if Match (Lox_Scanner.TOKEN_CLASS) then
         Class_Declaration;
      elsif Match (Lox_Scanner.TOKEN_FUN) then
         Function_Declaration;
      elsif Match (Lox_Scanner.TOKEN_VAR) then
         Variable_Declaration;
      else
         Statement;
      end if;

      if Context.Parser.Panic_Mode then
         Synchronize;
      end if;
   end Declaration;

   procedure Statement is
   begin
      if Match (Lox_Scanner.TOKEN_PRINT) then
         Print_Statement;
      elsif Match (Lox_Scanner.TOKEN_FOR) then
         For_Statement;
      elsif Match (Lox_Scanner.TOKEN_IF) then
         If_Statement;
      elsif Match (Lox_Scanner.TOKEN_RETURN) then
         Return_Statement;
      elsif Match (Lox_Scanner.TOKEN_WHILE) then
         While_Statement;
      elsif Match (Lox_Scanner.TOKEN_SWITCH) then
         Switch_Statement;
      elsif Match (Lox_Scanner.TOKEN_LEFT_BRACE) then
         Begin_Scope;
         Block;
         End_Scope;
      else
         Expression_Statement;
      end if;
   end Statement;

   procedure Parse_Precedence (Precedence : Precedence_Type) is
      Prefix_Rule : Parse_Fn := null;
      Can_Assign  : Boolean := False;
   begin
      Advance;
      Prefix_Rule := Get_Rule (Context.Parser.Previous.Kind).Prefix;
      if Prefix_Rule = null then
         Error ("Expect expression.");
         return;
      end if;

      Can_Assign := Precedence <= PREC_ASSIGNMENT;
      Prefix_Rule (Can_Assign);

      while Precedence <= Get_Rule (Context.Parser.Current.Kind).Precedence
      loop
         Advance;
         declare
            Infix_Rule : constant Parse_Fn :=
              Get_Rule (Context.Parser.Previous.Kind).Infix;
         begin
            Infix_Rule (Can_Assign);
         end;
      end loop;

      if Can_Assign and then Match (Lox_Scanner.TOKEN_EQUAL) then
         Error ("Invalid assignment target.");
      end if;
   end Parse_Precedence;

   function Identifier_Constant (Token : Lox_Scanner.Token) return Byte is
   begin
      return Make_Constant (Lox_Value.Make_String (Token.Lexeme));
   end Identifier_Constant;

   function Identifiers_Equal
     (A : Lox_Scanner.Token; B : Lox_Scanner.Token) return Boolean
   is
      use type Lox_Scanner.Unbounded_String;
   begin
      return A.Lexeme = B.Lexeme;
   end Identifiers_Equal;

   function Resolve_Local
     (Compiler : Compiler_Access;
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
               Error ("Can't read local variable in its own initializer.");
            end if;
            Index := I;
            return True;
         end if;
      end loop;

      return False;
   end Resolve_Local;

   procedure Add_Local (Name : Lox_Scanner.Token) is
   begin
      if Context.Current.Local_Count > Natural (Local_Index'Last) then
         Error ("Too many local variables in function.");
         return;
      end if;

      declare
         Local : Local_Type renames
           Context.Current.Locals (Local_Index (Context.Current.Local_Count));
      begin
         Context.Current.Local_Count :=
           Natural'Succ (Context.Current.Local_Count);
         Local.Name := Name;
         Local.Depth := None;
         Local.Is_Captured := False;
      end;
   end Add_Local;

   function Resolve_Upvalue
     (Compiler : Compiler_Access;
      Name     : Lox_Scanner.Token;
      Index    : out Upvalue_Index) return Boolean
   is
      Local   : Local_Index;
      Upvalue : Upvalue_Index;
   begin
      if Compiler.Enclosing = null then
         return False;
      end if;

      if Resolve_Local (Compiler.Enclosing, Name, Local) then
         Compiler.Enclosing.Locals (Local).Is_Captured := True;
         Index := Add_Upvalue (Compiler, Upvalue_Slot_Index (Local), True);
         return True;
      end if;

      if Resolve_Upvalue (Compiler.Enclosing, Name, Upvalue) then
         Index := Add_Upvalue (Compiler, Upvalue_Slot_Index (Upvalue), False);
         return True;
      end if;

      return False;
   end Resolve_Upvalue;

   function Add_Upvalue
     (Compiler : Compiler_Access;
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
         Error ("Too many closure variables in function.");
         return 0;
      end if;

      Compiler.Upvalues (Upvalue_Index (Upvalue_Count)).Is_Local := Is_Local;
      Compiler.Upvalues (Upvalue_Index (Upvalue_Count)).Index := Index;
      Compiler.Func.Upvalue_Count :=
        Natural'Succ (Compiler.Func.Upvalue_Count);

      return Upvalue_Index (Upvalue_Count);
   end Add_Upvalue;

   procedure Declare_Variable is
      procedure Check_Duplicate_In_Same_Scope (Name : Lox_Scanner.Token) is
      begin
         if Context.Current.Local_Count = 0 then
            return;
         end if;

         for I in reverse
           Local_Index'First
           .. Local_Index (Natural'Pred (Context.Current.Local_Count))
         loop
            declare
               Local : Local_Type renames Context.Current.Locals (I);
            begin
               exit when
                 Local.Depth.Has_Value
                 and then Local.Depth.Value < Context.Current.Scope_Depth;

               if Identifiers_Equal (Name, Local.Name) then
                  Error ("Already a variable with this name in this scope.");
               end if;
            end;
         end loop;
      end Check_Duplicate_In_Same_Scope;

   begin
      if Context.Current.Scope_Depth = 0 then
         return;
      end if;

      declare
         Name : constant Lox_Scanner.Token := Context.Parser.Previous;
      begin
         Check_Duplicate_In_Same_Scope (Name);

         Add_Local (Name);
      end;
   end Declare_Variable;

   function Parse_Variable (Error_Message : String) return Byte is
   begin
      Consume (Lox_Scanner.TOKEN_IDENTIFIER, Error_Message);

      Declare_Variable;
      if Context.Current.Scope_Depth > 0 then
         return 0;
      end if;

      return Identifier_Constant (Context.Parser.Previous);
   end Parse_Variable;

   procedure Mark_Initialized is
   begin
      if Context.Current.Scope_Depth = 0 then
         return;
      end if;

      declare
         Top_Index : constant Local_Index :=
           Local_Index (Natural'Pred (Context.Current.Local_Count));
      begin
         Context.Current.Locals (Top_Index).Depth :=
           Just (Context.Current.Scope_Depth);
      end;
   end Mark_Initialized;

   procedure Define_Variable (Global : Byte) is
   begin
      if Context.Current.Scope_Depth > 0 then
         Mark_Initialized;
         return;
      end if;

      Emit_Bytes (Lox_Chunk.OP_DEFINE_GLOBAL, Global);
   end Define_Variable;

   function Argument_List return Byte is
      Arg_Count : Byte := 0;
   begin
      if not Check (Lox_Scanner.TOKEN_RIGHT_PAREN) then
         loop
            Expression;
            if Arg_Count = 255 then
               Error ("Can't have more than 255 arguments.");
            end if;
            Arg_Count := Byte'Succ (Arg_Count);
            exit when not Match (Lox_Scanner.TOKEN_COMMA);
         end loop;
      end if;
      Consume (Lox_Scanner.TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
      return Arg_Count;
   end Argument_List;

   function Get_Rule (Kind : Lox_Scanner.TokenType) return Parse_Rule is
   begin
      return Rules (Kind);
   end Get_Rule;

   procedure Synchronize is
   begin
      Context.Parser.Panic_Mode := False;

      while Context.Parser.Current.Kind /= Lox_Scanner.TOKEN_EOF loop
         if Context.Parser.Previous.Kind = Lox_Scanner.TOKEN_SEMICOLON then
            return;
         end if;
         case Context.Parser.Current.Kind is
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

         Advance;
      end loop;
   end Synchronize;

end Lox_Compiler;
