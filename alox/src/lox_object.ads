with Lox_Chunk;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package Lox_Object is
   package Unbounded renames Ada.Strings.Unbounded;
   subtype Unbounded_String is Unbounded.Unbounded_String;

   type Obj_Function;
   type Obj_Function_Access is access all Obj_Function;

   type Obj_Closure;
   type Obj_Closure_Access is access all Obj_Closure;

   type Obj_Upvalue;
   type Obj_Upvalue_Access is access all Obj_Upvalue;

   type Obj_Function is record
      Arity         : Natural;
      Upvalue_Count : Natural;
      Name          : Unbounded_String;
      Chunk         : Lox_Chunk.Chunk;

      Next : Obj_Function_Access;
   end record;

   type Obj_Upvalue is record
      Location : Natural;

      Next : Obj_Upvalue_Access;
   end record;

   package Upvalue_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Obj_Upvalue_Access);

   type Obj_Closure is record
      Func     : Obj_Function_Access;
      Upvalues : Upvalue_Vectors.Vector;

      Next : Obj_Closure_Access;
   end record;

   type Objects is private;

   function New_Function (O : in out Objects) return Obj_Function_Access;
   function New_Closure
     (O : in out Objects; Func : Obj_Function_Access)
      return Obj_Closure_Access;
   function New_Upvalue
     (O : in out Objects; Slot : Natural) return Obj_Upvalue_Access;

   procedure Free_Objects (O : in out Objects);

   function To_String (Func : Obj_Function) return String;
   function To_String (Closure : Obj_Closure) return String;

private
   procedure Free is new
     Ada.Unchecked_Deallocation (Obj_Function, Obj_Function_Access);
   procedure Free is new
     Ada.Unchecked_Deallocation (Obj_Closure, Obj_Closure_Access);
   procedure Free is new
     Ada.Unchecked_Deallocation (Obj_Upvalue, Obj_Upvalue_Access);

   type Objects is record
      Functions : Obj_Function_Access;
      Closures  : Obj_Closure_Access;
      Upvalues  : Obj_Upvalue_Access;
   end record;

end Lox_Object;
