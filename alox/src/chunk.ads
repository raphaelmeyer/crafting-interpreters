with Ada.Finalization;

package Chunk is
   type Op_Code is (Op_Return) with Size => 8;

   type Code_Array is array (Natural range <>) of Op_Code;
   type Code_Array_Access is access all Code_Array;

   type Chunk is new Ada.Finalization.Controlled with record
      Count    : Natural;
      Capacity : Natural;
      Code     : Code_Array_Access;
   end record;

   procedure Init (C : in out Chunk);
   procedure Write (C : in out Chunk; B : in Op_Code);
   procedure Free (C : in out Chunk);

   overriding
   procedure Finalize (Obj : in out Chunk);

end Chunk;
