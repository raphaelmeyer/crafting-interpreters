with Lox_Chunk;
with Lox_Table;
with Lox_Value;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package Lox_Object is
   package Unbounded renames Ada.Strings.Unbounded;
   subtype Unbounded_String is Unbounded.Unbounded_String;

   type Object_Kind is
     (OBJ_KIND_CLASS,
      OBJ_KIND_FUNCTION,
      OBJ_KIND_CLOSURE,
      OBJ_KIND_INSTANCE,
      OBJ_KIND_UPVALUE);

   type Object (Kind : Object_Kind);
   type Object_Access is access all Object;

   type Upvalue (Closed : Boolean := False) is record
      case Closed is
         when True =>
            Value : Lox_Value.Value;

         when False =>
            Location  : Natural;
            Next_Open : Object_Access;
      end case;
   end record;

   package Upvalue_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Object_Access);

   type Object (Kind : Object_Kind) is record
      Is_Marked : Boolean;

      Next : Object_Access;
      case Kind is
         when OBJ_KIND_CLASS =>
            Class_Name : Unbounded_String;

         when OBJ_KIND_INSTANCE =>
            Class  : Object_Access;
            Fields : Lox_Table.Table;

         when OBJ_KIND_FUNCTION =>
            Arity         : Natural;
            Upvalue_Count : Natural;
            Name          : Unbounded_String;
            Chunk         : Lox_Chunk.Chunk;

         when OBJ_KIND_CLOSURE =>
            Func     : Object_Access;
            Upvalues : Upvalue_Vectors.Vector;

         when OBJ_KIND_UPVALUE =>
            Instance : Upvalue;
      end case;
   end record;

   function New_Class
     (Objs : in out Object_Access; Name : String) return Object_Access;
   function New_Function (Objs : in out Object_Access) return Object_Access;
   function New_Instance
     (Objs : in out Object_Access; Class : Object_Access) return Object_Access;
   function New_Closure
     (Objs : in out Object_Access; Func : Object_Access) return Object_Access;
   function New_Upvalue
     (Objs : in out Object_Access; Slot : Natural) return Object_Access;

   procedure Free_Objects (Objs : in out Object_Access);

   function To_String (Obj : Object_Access) return String;

private
   procedure Free is new Ada.Unchecked_Deallocation (Object, Object_Access);

   procedure Manage_Object (Objs : in out Object_Access; Obj : Object_Access);
   procedure Release_Object (Obj : in out Object_Access);

   procedure Collect_Garbage (Objs : in out Object_Access);
   procedure Trigger_Garbage_Collection_On_Threshold
     (Objs : in out Object_Access);
   procedure Mark_Roots;
   procedure Mark_Value (Value : in out Lox_Value.Value);
   procedure Mark_Table (Table : in out Lox_Table.Table);
   procedure Mark_Object (Obj : Object_Access);
   procedure Trace_References (Obj : Object_Access);
   procedure Sweep (Objs : in out Object_Access);

end Lox_Object;
