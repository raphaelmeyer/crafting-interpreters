with Ada.Finalization;
with Value;

package Chunk is
   type Op_Code is (Op_Constant, Op_Return) with Size => 8;

   type Code_Array is array (Natural range <>) of Op_Code;
   type Code_Array_Access is access all Code_Array;

   type Chunk is new Ada.Finalization.Controlled with record
      Count     : Natural;
      Capacity  : Natural;
      Code      : Code_Array_Access;
      Constants : Value.Value_Array;
   end record;

   procedure Init (C : in out Chunk);
   procedure Write (C : in out Chunk; B : Op_Code);
   procedure Free (C : in out Chunk);

   function Add_Constant (C : in out Chunk; V : Value.Value) return Natural;

   overriding
   procedure Finalize (Obj : in out Chunk);

private
   procedure Grow (C : in out Chunk);

end Chunk;
