with Ada.Finalization;
with Value;

package Chunk is
   type Byte is mod 2**8 with Size => 8;
   type Op_Code is (Op_Constant, Op_Return) with Size => 8;

   type Byte_Array is array (Natural range <>) of Byte;
   type Byte_Array_Access is access all Byte_Array;

   type Chunk is new Ada.Finalization.Controlled with record
      Count     : Natural;
      Capacity  : Natural;
      Code      : Byte_Array_Access;
      Constants : Value.Value_Array;
   end record;

   procedure Init (C : in out Chunk);
   procedure Write (C : in out Chunk; B : Byte);
   procedure Write (C : in out Chunk; O : Op_Code);
   procedure Free (C : in out Chunk);

   function Add_Constant (C : in out Chunk; V : Value.Value) return Natural;

   overriding
   procedure Finalize (Obj : in out Chunk);

private
   procedure Grow (C : in out Chunk);

end Chunk;
