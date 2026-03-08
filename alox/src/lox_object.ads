with Lox_Chunk;

with Ada.Strings.Unbounded;

package Lox_Object is

   package Unbounded renames Ada.Strings.Unbounded;
   subtype Unbounded_String is Unbounded.Unbounded_String;

   type Obj_Function is record
      Arity : Natural;
      Name  : Unbounded_String;
      Chunk : aliased Lox_Chunk.Chunk;
   end record;

   type Obj_Function_Access is access Obj_Function;

   function New_Function return Obj_Function_Access;

   function To_String (Function_Object : Obj_Function) return String;

end Lox_Object;
