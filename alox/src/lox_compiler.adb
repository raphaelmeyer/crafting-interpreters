with Debug;

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
      Lox_Scanner.TOKEN_BANG          => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_BANG_EQUAL    => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_EQUAL         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_EQUAL_EQUAL   => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_GREATER       => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_GREATER_EQUAL => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_LESS          => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_LESS_EQUAL    => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_IDENTIFIER    => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_STRING        => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_NUMBER        => (Number'Access, null, PREC_NONE),
      Lox_Scanner.TOKEN_AND           => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_CLASS         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_ELSE          => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_FALSE         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_FOR           => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_FUN           => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_IF            => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_NIL           => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_OR            => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_PRINT         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_RETURN        => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_SUPER         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_THIS          => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_TRUE          => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_VAR           => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_WHILE         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_ERROR         => (null, null, PREC_NONE),
      Lox_Scanner.TOKEN_EOF           => (null, null, PREC_NONE)];

   function Compile
     (Source : Lox_Scanner.Source_Code; Chunk : Lox_Chunk.Chunk_Access)
      return Boolean
   is
      C : Compiler_Context;
   begin
      C.Scanner := Lox_Scanner.Init (Source);
      C.Compiling_Chunk := Chunk;

      C.Parser.Had_Error := False;
      C.Parser.Panic_Mode := False;

      Advance (C);
      Expression (C);
      Consume (C, Lox_Scanner.TOKEN_EOF, "Expect end of expression.");

      End_Compiler (C);

      return not C.Parser.Had_Error;
   end Compile;

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

      Ada.Text_IO.Put
        (Ada.Text_IO.Standard_Error, "[line " & Token.Line'Image & "] Error");

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

   procedure Emit_Byte (C : in out Compiler_Context; Byte : Lox_Chunk.Byte) is
   begin
      Lox_Chunk.Write (Current_Chunk (C).all, Byte, C.Parser.Previous.Line);
   end Emit_Byte;

   procedure Emit_Byte
     (C : in out Compiler_Context; Op_Code : Lox_Chunk.Op_Code) is
   begin
      Lox_Chunk.Write (Current_Chunk (C).all, Op_Code, C.Parser.Previous.Line);
   end Emit_Byte;

   procedure Emit_Bytes
     (C      : in out Compiler_Context;
      Byte_1 : Lox_Chunk.Op_Code;
      Byte_2 : Lox_Chunk.Byte) is
   begin
      Emit_Byte (C, Byte_1);
      Emit_Byte (C, Byte_2);
   end Emit_Bytes;

   procedure Emit_Return (C : in out Compiler_Context) is
   begin
      Emit_Byte (C, Lox_Chunk.Op_Return);
   end Emit_Return;

   function Make_Constant
     (C : in out Compiler_Context; Value : Lox_Value.Value)
      return Lox_Chunk.Byte
   is
      Id : constant Natural :=
        Lox_Chunk.Add_Constant (Current_Chunk (C).all, Value);
   begin
      if Id > Natural (Lox_Chunk.Byte'Last) then
         Error (C, "Too many constants in one chunk.");
         return 0;
      end if;

      return Lox_Chunk.Byte (Id);
   end Make_Constant;

   procedure Emit_Constant
     (C : in out Compiler_Context; Value : Lox_Value.Value)
   is
      Id : constant Lox_Chunk.Byte := Make_Constant (C, Value);
   begin
      Emit_Bytes (C, Lox_Chunk.Op_Constant, Id);
   end Emit_Constant;

   procedure End_Compiler (C : in out Compiler_Context) is
   begin
      Emit_Return (C);

      if Debug.Print_Code_Enabled then
         if not C.Parser.Had_Error then
            Debug.DisassembleChunk
              (Lox_Chunk.Chunk_Read_Access (Current_Chunk (C)), "code");
         end if;
      end if;
   end End_Compiler;

   procedure Binary (C : in out Compiler_Context) is
      Operator_Type : constant Lox_Scanner.TokenType := C.Parser.Previous.Kind;
      Rule          : constant Parse_Rule := Get_Rule (Operator_Type);
   begin
      Parse_Precedence (C, Precedence_Type'Succ (Rule.Precedence));

      case Operator_Type is
         when Lox_Scanner.TOKEN_PLUS  =>
            Emit_Byte (C, Lox_Chunk.Op_Add);

         when Lox_Scanner.TOKEN_MINUS =>
            Emit_Byte (C, Lox_Chunk.Op_Subtract);

         when Lox_Scanner.TOKEN_STAR  =>
            Emit_Byte (C, Lox_Chunk.Op_Multiply);

         when Lox_Scanner.TOKEN_SLASH =>
            Emit_Byte (C, Lox_Chunk.Op_Divide);

         when others                  =>
            return;
      end case;
   end Binary;

   procedure Grouping (C : in out Compiler_Context) is
   begin
      Expression (C);
      Consume
        (C, Lox_Scanner.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
   end Grouping;

   procedure Number (C : in out Compiler_Context) is
      Lexeme : constant String :=
        Unbounded.To_String (C.Parser.Previous.Lexeme);
      Value  : constant Float := Float'Value (Lexeme);
   begin
      Emit_Constant (C, Lox_Value.Value (Value));
   end Number;

   procedure Unary (C : in out Compiler_Context) is
      Kind : constant Lox_Scanner.TokenType := C.Parser.Previous.Kind;
   begin
      Parse_Precedence (C, PREC_UNARY);

      case Kind is
         when Lox_Scanner.TOKEN_MINUS =>
            Emit_Byte (C, Lox_Chunk.Op_Negate);

         when others                  =>
            return;

      end case;

   end Unary;

   procedure Expression (C : in out Compiler_Context) is
   begin
      Parse_Precedence (C, PREC_ASSIGNMENT);
   end Expression;

   procedure Parse_Precedence
     (C : in out Compiler_Context; Precedence : Precedence_Type)
   is
      Prefix_Rule : Parse_Fn := null;
   begin
      Advance (C);
      Prefix_Rule := Get_Rule (C.Parser.Previous.Kind).Prefix;
      if Prefix_Rule = null then
         Error (C, "Expect expression.");
         return;
      end if;

      Prefix_Rule (C);

      while Precedence < Get_Rule (C.Parser.Current.Kind).Precedence loop
         Advance (C);
         declare
            Infix_Rule : constant Parse_Fn :=
              Get_Rule (C.Parser.Previous.Kind).Infix;
         begin
            Infix_Rule (C);
         end;
      end loop;

   end Parse_Precedence;

   function Get_Rule (Kind : Lox_Scanner.TokenType) return Parse_Rule is
   begin
      return Rules (Kind);
   end Get_Rule;

end Lox_Compiler;
