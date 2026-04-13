package body Lox_Table is

   function Hash_String
     (Key : Lox_Value.Unbounded_String) return Ada.Containers.Hash_Type
   is
      use type Ada.Containers.Hash_Type;
      Hash : Ada.Containers.Hash_Type := 2166136261;
      Char : Character;
   begin
      for Index in 1 .. Lox_Value.Unbounded.Length (Key) loop
         Char := Lox_Value.Unbounded.Element (Key, Index);
         Hash := Hash xor Ada.Containers.Hash_Type (Character'Pos (Char));
         Hash := Hash * 16777619;
      end loop;
      return Hash;
   end Hash_String;

end Lox_Table;
