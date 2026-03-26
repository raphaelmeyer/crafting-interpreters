with Lox_Chunk;
with Lox_Object;
with Lox_Scanner;
with Lox_Value;
with Lox_Types; use Lox_Types;

with Ada.Containers.Hashed_Maps;

package Lox_VM is
   type VM_Context is limited private;

   type Interpret_Result is
     (INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR);

   type Stack_Index is range 0 .. 16383;
   type Stack_Array is array (Stack_Index) of Lox_Value.Value;

   type Call_Frame is limited record
      Closure : Lox_Object.Obj_Closure_Access;
      IP      : Lox_Chunk.Byte_Vectors.Cursor;
      Slots   : Stack_Index;
   end record;

   type Call_Frame_Index is range 0 .. 63;
   type Call_Frame_Array is array (Call_Frame_Index) of Call_Frame;

   procedure Init_VM;
   procedure Free_VM;

   function Interpret
     (Source : Lox_Scanner.Source_Code) return Interpret_Result;

   function New_Function return Lox_Object.Obj_Function_Access;

private
   function Hash_String
     (Key : Lox_Value.Unbounded_String) return Ada.Containers.Hash_Type;

   package Hash_Table is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Lox_Value.Unbounded_String,
        Element_Type    => Lox_Value.Value,
        Hash            => Hash_String,
        Equivalent_Keys => Lox_Value.Unbounded."=",
        "="             => Lox_Value.Values_Equal);

   type VM_Context is limited record
      Frames      : Call_Frame_Array;
      Frame_Count : Natural;

      Stack     : Stack_Array;
      Stack_Top : Stack_Index;
      Globals   : Hash_Table.Map;

      Objects : Lox_Object.Objects;
   end record;

   procedure Push (Value : Lox_Value.Value);
   function Pop return Lox_Value.Value;
   function Peek (Distance : Integer) return Lox_Value.Value;

   function Arity_Error_Message
     (Arity : Natural; Arg_Count : Natural) return String;
   function Call
     (Closure : Lox_Object.Obj_Closure_Access; Arg_Count : Natural)
      return Boolean;
   function Call_Native
     (Native : Lox_Value.Native; Arg_Count : Natural) return Boolean;
   function Call_Value
     (Callee : Lox_Value.Value; Arg_Count : Natural) return Boolean;

   function Capture_Upvalue
     (Local : Stack_Index) return Lox_Object.Obj_Upvalue_Access;

   function Is_Falsey (Value : Lox_Value.Value) return Boolean;
   procedure Concatenate;

   procedure Reset_Stack;
   procedure Runtime_Error (Message : String);

   procedure Define_Native
     (Name : String; Arity : Natural; Func : Lox_Value.Native_Fn);
   function Clock_Native (First_Arg : Natural) return Lox_Value.Value;
   function Random_Native (First_Arg : Natural) return Lox_Value.Value;

   function Read_Byte (Frame : in out Call_Frame) return Byte;
   function Read_Short (Frame : in out Call_Frame) return Short;
   function Read_Constant (Frame : in out Call_Frame) return Lox_Value.Value;
   function Read_String
     (Frame : in out Call_Frame) return Lox_Value.Unbounded_String;

   function Get_Upvalue (Location : Natural) return Lox_Value.Value;
   procedure Set_Upvalue (Location : Natural; Value : Lox_Value.Value);

   function Run return Interpret_Result;

end Lox_VM;
