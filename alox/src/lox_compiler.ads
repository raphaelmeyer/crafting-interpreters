with Lox_Chunk;
with Lox_Scanner;
with Lox_Value;

package Lox_Compiler is
   function Compile
     (Source : Lox_Scanner.Source_Code; Chunk : Lox_Chunk.Chunk_Access)
      return Boolean;

private
   use type Lox_Scanner.TokenType;

   type Precedence_Type is
     (PREC_NONE,
      PREC_ASSIGNMENT,  -- =
      PREC_OR,          -- or
      PREC_AND,         -- and
      PREC_EQUALITY,    -- == !=
      PREC_COMPARISON,  -- < > <= >=
      PREC_TERM,        -- + -
      PREC_FACTOR,      -- * /
      PREC_UNARY,       -- ! -
      PREC_CALL,        -- . ()
      PREC_PRIMARY);

   type Parser_Context is limited record
      Current    : Lox_Scanner.Token;
      Previous   : Lox_Scanner.Token;
      Had_Error  : Boolean;
      Panic_Mode : Boolean;
   end record;

   type Compiler_Context is limited record
      Scanner         : Lox_Scanner.Scanner;
      Parser          : Parser_Context;
      Compiling_Chunk : Lox_Chunk.Chunk_Access;
   end record;

   type Parse_Fn is access procedure (C : in out Compiler_Context);

   type Parse_Rule is record
      Prefix     : Parse_Fn;
      Infix      : Parse_Fn;
      Precedence : Precedence_Type;
   end record;

   function Current_Chunk
     (C : in out Compiler_Context) return Lox_Chunk.Chunk_Access;

   procedure Error_At
     (C       : in out Compiler_Context;
      Token   : Lox_Scanner.Token;
      Message : String);
   procedure Error (C : in out Compiler_Context; Message : String);
   procedure Error_At_Current (C : in out Compiler_Context; Message : String);

   procedure Advance (C : in out Compiler_Context);
   procedure Consume
     (C       : in out Compiler_Context;
      Kind    : Lox_Scanner.TokenType;
      Message : String);
   procedure Emit_Byte (C : in out Compiler_Context; Byte : Lox_Chunk.Byte);
   procedure Emit_Byte
     (C : in out Compiler_Context; Op_Code : Lox_Chunk.Op_Code);
   procedure Emit_Bytes
     (C      : in out Compiler_Context;
      Byte_1 : Lox_Chunk.Op_Code;
      Byte_2 : Lox_Chunk.Byte);
   procedure Emit_Bytes
     (C      : in out Compiler_Context;
      Byte_1 : Lox_Chunk.Op_Code;
      Byte_2 : Lox_Chunk.Op_Code);
   procedure Emit_Return (C : in out Compiler_Context);
   function Make_Constant
     (C : in out Compiler_Context; Value : Lox_Value.Value)
      return Lox_Chunk.Byte;
   procedure Emit_Constant
     (C : in out Compiler_Context; Value : Lox_Value.Value);

   procedure End_Compiler (C : in out Compiler_Context);

   procedure Binary (C : in out Compiler_Context);
   procedure Literal (C : in out Compiler_Context);
   procedure Grouping (C : in out Compiler_Context);
   procedure Number (C : in out Compiler_Context);
   procedure Unary (C : in out Compiler_Context);
   procedure Expression (C : in out Compiler_Context);

   procedure Parse_Precedence
     (C : in out Compiler_Context; Precedence : Precedence_Type);
   function Get_Rule (Kind : Lox_Scanner.TokenType) return Parse_Rule;

end Lox_Compiler;
