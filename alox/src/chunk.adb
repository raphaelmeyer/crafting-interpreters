with Ada.Unchecked_Deallocation;

package body Chunk is
   procedure Free is new
     Ada.Unchecked_Deallocation (Code_Array, Code_Array_Access);

   procedure Grow (C : in out Chunk);

   procedure Init (C : in out Chunk) is
   begin
      C.Capacity := 0;
      C.Count := 0;
      C.Code := null;
   end Init;

   procedure Write (C : in out Chunk; B : Op_Code) is
   begin
      if (C.Capacity < C.Count + 1) then
         Grow (C);
      end if;
      C.Code (C.Count) := B;
      C.Count := C.Count + 1;
   end Write;

   procedure Free (C : in out Chunk) is
   begin
      if C.Code /= null then
         Free (C.Code);
      end if;
      Init (C);
   end Free;

   overriding
   procedure Finalize (Obj : in out Chunk) is
   begin
      Free (Obj);
   end Finalize;

   procedure Grow (C : in out Chunk) is
      Old_Code : Code_Array_Access := C.Code;
   begin
      C.Capacity := (if C.Capacity < 8 then 8 else C.Capacity * 2);
      C.Code := new Code_Array (0 .. C.Capacity - 1);
      if Old_Code /= null then
         C.Code (Old_Code'Range) := Old_Code.all (Old_Code'Range);
      end if;
      Free (Old_Code);
   end Grow;

end Chunk;
