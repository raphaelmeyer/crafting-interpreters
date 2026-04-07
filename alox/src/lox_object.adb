package body Lox_Object is
   function New_Function (Objs : in out Object_Access) return Object_Access is
      Func : constant Object_Access :=
        new Object'
          (Kind          => OBJ_KIND_FUNCTION,
           Next          => Objs,
           Arity         => 0,
           Upvalue_Count => 0,
           Name          => Unbounded.Null_Unbounded_String,
           Chunk         => <>);
   begin
      Lox_Chunk.Init (Func.Chunk);
      Objs := Func;
      return Func;
   end New_Function;

   function New_Closure
     (Objs : in out Object_Access; Func : Object_Access) return Object_Access
   is
      Closure : constant Object_Access :=
        new Object'
          (Kind     => OBJ_KIND_CLOSURE,
           Next     => Objs,
           Func     => Func,
           Upvalues => <>);
   begin
      Objs := Closure;

      for I in 1 .. Func.Upvalue_Count loop
         Closure.Upvalues.Append (null);
      end loop;

      return Closure;
   end New_Closure;

   function New_Upvalue
     (Objs : in out Object_Access; Slot : Natural) return Object_Access
   is
      Upvalue : constant Object_Access :=
        new Object'
          (Kind     => OBJ_KIND_UPVALUE,
           Next     => Objs,
           Instance => (Closed => False, Location => Slot, Next_Open => null));
   begin
      Objs := Upvalue;
      return Upvalue;
   end New_Upvalue;

   procedure Free_Objects (Objs : in out Object_Access) is
      Next : Object_Access;
   begin
      while Objs /= null loop
         Next := Objs.Next;
         Free (Objs);
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

end Lox_Object;
