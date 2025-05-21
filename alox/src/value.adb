with Ada.Unchecked_Deallocation;

package body Value is
   procedure Free is new
     Ada.Unchecked_Deallocation (Value_Array_Array, Value_Array_Array_Access);

   procedure Grow (VA : in out Value_Array);

   procedure Init (VA : in out Value_Array) is
   begin
      VA.Capacity := 0;
      VA.Count := 0;
      VA.Values := null;
   end Init;

   procedure Write (VA : in out Value_Array; V : Value) is
   begin
      if VA.Capacity < VA.Count + 1 then
         Grow (VA);
      end if;
      VA.Values (VA.Count) := V;
      VA.Count := VA.Count + 1;
   end Write;

   procedure Free (VA : in out Value_Array) is
   begin
      if VA.Values /= null then
         Free (VA.Values);
      end if;
      Init (VA);
   end Free;

   overriding
   procedure Finalize (Obj : in out Value_Array) is
   begin
      Free (Obj);
   end Finalize;

   procedure Grow (VA : in out Value_Array) is
      Old_Code : Value_Array_Array_Access := VA.Values;
   begin
      VA.Capacity := (if VA.Capacity < 8 then 8 else VA.Capacity * 2);
      VA.Values := new Value_Array_Array (0 .. VA.Capacity - 1);
      if Old_Code /= null then
         VA.Values (Old_Code'Range) := Old_Code.all (Old_Code'Range);
      end if;
      Free (Old_Code);
   end Grow;

end Value;
