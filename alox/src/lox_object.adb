with Debug;
with Lox_Compiler;
with Lox_VM;

with Ada.Text_IO;
with System.Address_Image;

package body Lox_Object is
   function New_Function (Objs : in out Object_Access) return Object_Access is
   begin
      Trigger_Garbage_Collection_On_Threshold;

      return Func : Object_Access do
         Func :=
           new Object'
             (Kind          => OBJ_KIND_FUNCTION,
              Is_Marked     => <>,
              Next          => <>,
              Arity         => 0,
              Upvalue_Count => 0,
              Name          => Unbounded.Null_Unbounded_String,
              Chunk         => <>);
         Manage_Object (Objs, Func);

         Lox_Chunk.Init (Func.Chunk);

      end return;
   end New_Function;

   function New_Closure
     (Objs : in out Object_Access; Func : Object_Access) return Object_Access
   is
   begin
      Trigger_Garbage_Collection_On_Threshold;

      return Closure : Object_Access do
         Closure :=
           new Object'
             (Kind      => OBJ_KIND_CLOSURE,
              Is_Marked => <>,
              Next      => <>,
              Func      => Func,
              Upvalues  => <>);
         Manage_Object (Objs, Closure);

         for I in 1 .. Func.Upvalue_Count loop
            Closure.Upvalues.Append (null);
         end loop;

      end return;
   end New_Closure;

   function New_Upvalue
     (Objs : in out Object_Access; Slot : Natural) return Object_Access is
   begin
      Trigger_Garbage_Collection_On_Threshold;

      return Upvalue : Object_Access do
         Upvalue :=
           new Object'
             (Kind      => OBJ_KIND_UPVALUE,
              Is_Marked => <>,
              Next      => <>,
              Instance  =>
                (Closed => False, Location => Slot, Next_Open => null));
         Manage_Object (Objs, Upvalue);

      end return;
   end New_Upvalue;

   procedure Free_Objects (Objs : in out Object_Access) is
      Next : Object_Access;
   begin
      while Objs /= null loop
         Next := Objs.Next;
         Release_Object (Objs);
         Objs := Next;
      end loop;
   end Free_Objects;

   function To_String (Obj : Object_Access) return String is
      use type Unbounded.Unbounded_String;
   begin
      case Obj.Kind is
         when OBJ_KIND_FUNCTION =>
            if Obj.Name = Unbounded.Null_Unbounded_String then
               return "<script>";
            end if;
            return "<fn " & Unbounded.To_String (Obj.Name) & ">";

         when OBJ_KIND_CLOSURE  =>
            return To_String (Obj.Func);

         when OBJ_KIND_UPVALUE  =>
            return "<upvalue>";
      end case;
   end To_String;

   procedure Manage_Object (Objs : in out Object_Access; Obj : Object_Access)
   is
   begin
      Obj.Is_Marked := False;

      Obj.Next := Objs;
      Objs := Obj;

      if Debug.Log_GC_Enabled then
         Ada.Text_IO.Put (System.Address_Image (Obj.all'Address));
         Ada.Text_IO.Put_Line (" allocate " & Obj.Kind'Image);
      end if;
   end Manage_Object;

   procedure Release_Object (Obj : in out Object_Access) is
   begin
      if Debug.Log_GC_Enabled then
         Ada.Text_IO.Put (System.Address_Image (Obj.all'Address));
         Ada.Text_IO.Put_Line (" free " & Obj.Kind'Image);
      end if;

      Free (Obj);
   end Release_Object;

   procedure Collect_Garbage is
   begin
      if Debug.Log_GC_Enabled then
         Ada.Text_IO.Put_Line ("-- gc begin");
      end if;

      Mark_Roots;

      if Debug.Log_GC_Enabled then
         Ada.Text_IO.Put_Line ("-- gc end");
      end if;
   end Collect_Garbage;

   procedure Trigger_Garbage_Collection_On_Threshold is
   begin
      if Debug.Stress_GC_Enabled then
         Collect_Garbage;
      end if;
   end Trigger_Garbage_Collection_On_Threshold;

   procedure Mark_Roots is
   begin
      Lox_VM.Iterate_Stack (Mark_Value'Access);
      Lox_VM.Iterate_Closures (Mark_Object'Access);
      Lox_VM.Iterate_Open_Upvalues (Mark_Object'Access);
      Lox_VM.Iterate_Globals (Mark_Value'Access);
      Lox_Compiler.Iterate_Current_Functions (Mark_Object'Access);
   end Mark_Roots;

   procedure Mark_Value (Value : in out Lox_Value.Value) is
   begin
      if Lox_Value.Is_Object (Value) then
         Mark_Object (Value.Object_Value);
      end if;
   end Mark_Value;

   procedure Mark_Object (Obj : Object_Access) is
   begin
      if Obj = null or else Obj.Is_Marked then
         return;
      end if;

      if Debug.Log_GC_Enabled then
         Ada.Text_IO.Put (System.Address_Image (Obj.all'Address));
         Ada.Text_IO.Put_Line (" mark " & To_String (Obj));
      end if;

      Obj.Is_Marked := True;
      Trace_References (Obj);
   end Mark_Object;

   procedure Trace_References (Obj : Object_Access) is
   begin
      case Obj.Kind is
         when OBJ_KIND_UPVALUE =>
            if Obj.Instance.Closed then
               Mark_Value (Obj.Instance.Value);
            end if;

         when OBJ_KIND_FUNCTION =>
            for Value of Obj.Chunk.Constants loop
               Mark_Value (Value);
            end loop;

         when OBJ_KIND_CLOSURE =>
            Mark_Object (Obj.Func);
            for Upvalue of Obj.Upvalues loop
               Mark_Object (Upvalue);
            end loop;

      end case;
   end Trace_References;

end Lox_Object;
