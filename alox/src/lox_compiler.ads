with Lox_Chunk;
with Lox_Object;
with Lox_Scanner;
with Lox_Value;
with Lox_Types; use Lox_Types;

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

package Lox_Compiler is
   function Compile
     (Source : Lox_Scanner.Source_Code) return Lox_Object.Object_Access;

   type Object_Action is access procedure (Obj : Lox_Object.Object_Access);
   procedure Iterate_Current_Functions (Action : not null Object_Action);

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
      Name        : Lox_Scanner.Token;
      Depth       : Maybe_Natural;
      Is_Captured : Boolean;
   end record;

   type Local_Index is range 0 .. 255;
   type Locals_Array is array (Local_Index) of Local_Type;

   type Upvalue_Slot_Index is range 0 .. 255;

   type Upvalue_Type is record
      Index    : Upvalue_Slot_Index;
      Is_Local : Boolean;
   end record;

   type Upvalue_Index is range 0 .. 255;
   type Upvalue_Array is array (Upvalue_Index) of Upvalue_Type;

   type Function_Kind is (TYPE_FUNCTION, TYPE_SCRIPT);

   type Compiler_Type is limited record
      Enclosing : access Compiler_Type;
      Func      : Lox_Object.Object_Access;
      Kind      : Function_Kind;

      Locals      : Locals_Array;
      Local_Count : Natural;
      Upvalues    : Upvalue_Array;
      Scope_Depth : Natural;
   end record;

   type Compiler_Access is access all Compiler_Type;

   type Compiler_Instance is new Ada.Finalization.Limited_Controlled
   with record
      Instance : Compiler_Access;
   end record;

   type Compiler_Context is limited record
      Scanner : Lox_Scanner.Scanner;
      Parser  : Parser_Context;
      Current : Compiler_Access;
   end record;

   type Parse_Fn is access procedure (Can_Assign : Boolean);

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

   function Current_Func return Lox_Object.Object_Access;

   procedure Error_At (Token : Lox_Scanner.Token; Message : String);
   procedure Error (Message : String);
   procedure Error_At_Current (Message : String);

   procedure Advance;
   procedure Consume (Kind : Lox_Scanner.TokenType; Message : String);
   function Check (Kind : Lox_Scanner.TokenType) return Boolean;
   function Match (Kind : Lox_Scanner.TokenType) return Boolean;

   procedure Emit_Byte (Value : Byte);
   procedure Emit_Byte (Op_Code : Lox_Chunk.Op_Code);
   procedure Emit_Bytes (Byte_1 : Lox_Chunk.Op_Code; Byte_2 : Byte);
   procedure Emit_Bytes
     (Byte_1 : Lox_Chunk.Op_Code; Byte_2 : Lox_Chunk.Op_Code);
   procedure Emit_Loop (Loop_Start : Natural);
   function Emit_Jump (Instruction : Lox_Chunk.Op_Code) return Natural;
   procedure Emit_Return;
   function Make_Constant (Value : Lox_Value.Value) return Byte;
   procedure Emit_Constant (Value : Lox_Value.Value);

   procedure Init_Compiler (Compiler : Compiler_Access; Kind : Function_Kind);
   function End_Compiler return Lox_Object.Object_Access;

   procedure Begin_Scope;
   procedure End_Scope;

   procedure Binary (Can_Assign : Boolean);
   procedure Call (Can_Assign : Boolean);
   procedure Literal (Can_Assign : Boolean);
   procedure Grouping (Can_Assign : Boolean);
   procedure Number (Can_Assign : Boolean);
   procedure String_Literal (Can_Assign : Boolean);
   procedure Logical_And (Can_Assign : Boolean);
   procedure Logical_Or (Can_Assign : Boolean);

   procedure Named_Variable (Name : Lox_Scanner.Token; Can_Assign : Boolean);
   procedure Variable (Can_Assign : Boolean);
   procedure Unary (Can_Assign : Boolean);
   procedure Expression;
   procedure Block;
   procedure Function_Declaration;
   procedure Function_Definition (Kind : Function_Kind);
   procedure Variable_Declaration;
   procedure Expression_Statement;
   procedure For_Statement;
   procedure If_Statement;
   procedure Print_Statement;
   procedure Return_Statement;
   procedure While_Statement;
   procedure Switch_Statement;
   procedure Declaration;
   procedure Statement;

   procedure Parse_Precedence (Precedence : Precedence_Type);
   function Identifier_Constant (Token : Lox_Scanner.Token) return Byte;
   function Identifiers_Equal
     (A : Lox_Scanner.Token; B : Lox_Scanner.Token) return Boolean;
   function Resolve_Local
     (Compiler : Compiler_Access;
      Name     : Lox_Scanner.Token;
      Index    : out Local_Index) return Boolean;
   procedure Add_Local (Name : Lox_Scanner.Token);
   function Resolve_Upvalue
     (Compiler : Compiler_Access;
      Name     : Lox_Scanner.Token;
      Index    : out Upvalue_Index) return Boolean;
   function Add_Upvalue
     (Compiler : Compiler_Access;
      Index    : Upvalue_Slot_Index;
      Is_Local : Boolean) return Upvalue_Index;
   procedure Declare_Variable;
   function Parse_Variable (Error_Message : String) return Byte;
   procedure Mark_Initialized;
   procedure Define_Variable (Global : Byte);
   function Argument_List return Byte;
   function Get_Rule (Kind : Lox_Scanner.TokenType) return Parse_Rule;

   procedure Synchronize;
end Lox_Compiler;
