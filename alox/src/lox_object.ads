with Lox_Chunk;

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package Lox_Object is
   package Unbounded renames Ada.Strings.Unbounded;
   subtype Unbounded_String is Unbounded.Unbounded_String;

   type Obj_Function;
   type Obj_Function_Access is access Obj_Function;

   type Obj_Function is record
      Arity : Natural;
      Name  : Unbounded_String;
      Chunk : Lox_Chunk.Chunk;

      Next : Obj_Function_Access;
   end record;

   type Objects is private;

   function New_Function (O : in out Objects) return Obj_Function_Access;
   procedure Free_Objects (O : in out Objects);

   function To_String (Function_Object : Obj_Function) return String;

private
   procedure Free is new
     Ada.Unchecked_Deallocation (Obj_Function, Obj_Function_Access);

   type Objects is record
      Functions : Obj_Function_Access;
   end record;

end Lox_Object;
