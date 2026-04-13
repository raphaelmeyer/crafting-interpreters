with Ada.Containers.Hashed_Maps;
with Lox_Value;

package Lox_Table is

   function Hash_String
     (Key : Lox_Value.Unbounded_String) return Ada.Containers.Hash_Type;

   package Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Lox_Value.Unbounded_String,
        Element_Type    => Lox_Value.Value,
        Hash            => Hash_String,
        Equivalent_Keys => Lox_Value.Unbounded."=",
        "="             => Lox_Value.Values_Equal);

   subtype Table  is Maps.Map;
   subtype Cursor is Maps.Cursor;

   No_Element : Maps.Cursor renames Maps.No_Element;

   function Element (Position : Maps.Cursor) return Lox_Value.Value
     renames Maps.Element;

end Lox_Table;
