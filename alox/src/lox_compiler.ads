with Lox_Chunk;
with Lox_Scanner;
with Lox_Value;

package Lox_Compiler is
   function Compile
     (Source : Lox_Scanner.Source_Code; Chunk : Lox_Chunk.Chunk_Access)
      return Boolean;

private
   use type Lox_Scanner.TokenType;

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
   procedure Emit_Return (C : in out Compiler_Context);
   function Make_Constant
     (C : in out Compiler_Context; Value : Lox_Value.Value)
      return Lox_Chunk.Byte;
   procedure Emit_Constant
     (C : in out Compiler_Context; Value : Lox_Value.Value);

   procedure End_Compiler (C : in out Compiler_Context);

   procedure Grouping (C : in out Compiler_Context);
   procedure Number (C : in out Compiler_Context);
   procedure Unary (C : in out Compiler_Context);
   procedure Expression (C : in out Compiler_Context);

end Lox_Compiler;
