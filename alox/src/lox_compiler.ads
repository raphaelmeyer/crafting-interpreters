with Lox_Chunk;
with Lox_Scanner;
with Lox_Value;

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

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

   type Maybe_Natural (Has_Value : Boolean := False) is record
      case Has_Value is
         when False =>
            null;

         when True =>
            Value : Natural;
      end case;
   end record;

   function Just (Value : Natural) return Maybe_Natural;
   function None return Maybe_Natural;

   type Local_Type is record
      Name  : Lox_Scanner.Token;
      Depth : Maybe_Natural;
   end record;

   type Local_Index is range 0 .. 255;
   type Locals_Array is array (Local_Index) of Local_Type;

   type Compiler_Type is limited record
      Locals      : Locals_Array;
      Local_Count : Natural;
      Scope_Depth : Natural;
   end record;

   type Compiler_Access is access Compiler_Type;

   type Compiler_Instance is new Ada.Finalization.Limited_Controlled
   with record
      Instance : Compiler_Access;
   end record;

   type Compiler_Context is limited record
      Scanner         : Lox_Scanner.Scanner;
      Parser          : Parser_Context;
      Current         : Compiler_Access;
      Compiling_Chunk : Lox_Chunk.Chunk_Access;
   end record;

   type Parse_Fn is
     access procedure (C : in out Compiler_Context; Can_Assign : Boolean);

   type Parse_Rule is record
      Prefix     : Parse_Fn;
      Infix      : Parse_Fn;
      Precedence : Precedence_Type;
   end record;

   overriding
   procedure Initialize (Compiler : in out Compiler_Instance);
   overriding
   procedure Finalize (Compiler : in out Compiler_Instance);
   procedure Free is new
     Ada.Unchecked_Deallocation
       (Object => Compiler_Type,
        Name   => Compiler_Access);

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
   function Check
     (C : in out Compiler_Context; Kind : Lox_Scanner.TokenType)
      return Boolean;
   function Match
     (C : in out Compiler_Context; Kind : Lox_Scanner.TokenType)
      return Boolean;

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
   function Emit_Jump
     (C : in out Compiler_Context; Instruction : Lox_Chunk.Op_Code)
      return Natural;
   procedure Emit_Return (C : in out Compiler_Context);
   function Make_Constant
     (C : in out Compiler_Context; Value : Lox_Value.Value)
      return Lox_Chunk.Byte;
   procedure Emit_Constant
     (C : in out Compiler_Context; Value : Lox_Value.Value);

   procedure Init_Compiler
     (C : in out Compiler_Context; Compiler : Compiler_Access);
   procedure End_Compiler (C : in out Compiler_Context);

   procedure Begin_Scope (C : in out Compiler_Context);
   procedure End_Scope (C : in out Compiler_Context);

   procedure Binary (C : in out Compiler_Context; Can_Assign : Boolean);
   procedure Literal (C : in out Compiler_Context; Can_Assign : Boolean);
   procedure Grouping (C : in out Compiler_Context; Can_Assign : Boolean);
   procedure Number (C : in out Compiler_Context; Can_Assign : Boolean);
   procedure String_Literal
     (C : in out Compiler_Context; Can_Assign : Boolean);
   procedure Named_Variable
     (C          : in out Compiler_Context;
      Name       : Lox_Scanner.Token;
      Can_Assign : Boolean);
   procedure Variable (C : in out Compiler_Context; Can_Assign : Boolean);
   procedure Unary (C : in out Compiler_Context; Can_Assign : Boolean);
   procedure Expression (C : in out Compiler_Context);
   procedure Block (C : in out Compiler_Context);
   procedure Variable_Declaration (C : in out Compiler_Context);
   procedure Expression_Statement (C : in out Compiler_Context);
   procedure If_Statement (C : in out Compiler_Context);
   procedure Print_Statement (C : in out Compiler_Context);
   procedure Declaration (C : in out Compiler_Context);
   procedure Statement (C : in out Compiler_Context);

   procedure Parse_Precedence
     (C : in out Compiler_Context; Precedence : Precedence_Type);
   function Identifier_Constant
     (C : in out Compiler_Context; Token : Lox_Scanner.Token)
      return Lox_Chunk.Byte;
   function Identifiers_Equal
     (A : Lox_Scanner.Token; B : Lox_Scanner.Token) return Boolean;
   function Resolve_Local
     (C        : in out Compiler_Context;
      Compiler : Compiler_Access;
      Name     : Lox_Scanner.Token;
      Index    : out Local_Index) return Boolean;
   procedure Add_Local (C : in out Compiler_Context; Name : Lox_Scanner.Token);
   procedure Declare_Variable (C : in out Compiler_Context);
   function Parse_Variable
     (C : in out Compiler_Context; Error_Message : String)
      return Lox_Chunk.Byte;
   procedure Mark_Initialized (C : in out Compiler_Context);
   procedure Define_Variable
     (C : in out Compiler_Context; Global : Lox_Chunk.Byte);
   function Get_Rule (Kind : Lox_Scanner.TokenType) return Parse_Rule;

   procedure Synchronize (C : in out Compiler_Context);
end Lox_Compiler;
