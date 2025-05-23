with Ada.Unchecked_Deallocation;

package body Chunk is
   procedure Free is new
     Ada.Unchecked_Deallocation (Byte_Array, Byte_Array_Access);

   procedure Free is new
     Ada.Unchecked_Deallocation (Natural_Array, Natural_Array_Access);

   procedure Init (C : in out Chunk) is
   begin
      C.Capacity := 0;
      C.Count := 0;
      C.Code := null;
      C.Lines := null;
      Value.Init (C.Constants);
   end Init;

   procedure Write (C : in out Chunk; B : Byte; Line : Natural) is
   begin
      if C.Capacity < C.Count + 1 then
         Grow (C);
      end if;
      C.Code (C.Count) := B;
      C.Lines (C.Count) := Line;
      C.Count := C.Count + 1;
   end Write;

   procedure Write (C : in out Chunk; O : Op_Code; Line : Natural) is
   begin
      Write (C, O'Enum_Rep, Line);
   end Write;

   procedure Free (C : in out Chunk) is
   begin
      if C.Code /= null then
         Free (C.Code);
      end if;
      if C.Lines /= null then
         Free (C.Lines);
      end if;
      Value.Free (C.Constants);
      Init (C);
   end Free;

   function Add_Constant (C : in out Chunk; V : Value.Value) return Natural is
   begin
      Value.Write (C.Constants, V);
      return C.Constants.Count - 1;
   end Add_Constant;

   overriding
   procedure Finalize (Obj : in out Chunk) is
   begin
      Free (Obj);
   end Finalize;

   procedure Grow (C : in out Chunk) is
      Old_Code  : Byte_Array_Access := C.Code;
      Old_Lines : Natural_Array_Access := C.Lines;
   begin
      C.Capacity := (if C.Capacity < 8 then 8 else C.Capacity * 2);
      C.Code := new Byte_Array (0 .. C.Capacity - 1);
      C.Lines := new Natural_Array (0 .. C.Capacity - 1);
      if Old_Code /= null then
         C.Code (Old_Code'Range) := Old_Code.all (Old_Code'Range);
      end if;
      if Old_Lines /= null then
         C.Lines (Old_Lines'Range) := Old_Lines.all (Old_Lines'Range);
      end if;
      Free (Old_Code);
      Free (Old_Lines);
   end Grow;

end Chunk;
