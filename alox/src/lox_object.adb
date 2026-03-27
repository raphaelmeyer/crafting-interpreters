package body Lox_Object is
   function New_Function (O : in out Objects) return Obj_Function_Access is
      Func : constant Obj_Function_Access :=
        new Obj_Function'
          (Arity         => 0,
           Upvalue_Count => 0,
           Name          => Unbounded.Null_Unbounded_String,
           Chunk         => <>,
           Next          => <>);
   begin
      Func.Next := O.Functions;
      O.Functions := Func;

      Lox_Chunk.Init (Func.Chunk);
      return Func;
   end New_Function;

   function New_Closure
     (O : in out Objects; Func : Obj_Function_Access) return Obj_Closure_Access
   is
      Closure : constant Obj_Closure_Access :=
        new Obj_Closure'(Func => Func, Upvalues => <>, Next => <>);
   begin
      Closure.Next := O.Closures;
      O.Closures := Closure;

      for I in 1 .. Func.Upvalue_Count loop
         Closure.Upvalues.Append (null);
      end loop;

      return Closure;
   end New_Closure;

   function New_Upvalue
     (O : in out Objects; Slot : Natural) return Obj_Upvalue_Access
   is
      Upvalue : constant Obj_Upvalue_Access :=
        new Obj_Upvalue'
          (Instance => (Closed => False, Location => Slot, Next_Open => <>),
           Next     => <>);
   begin
      Upvalue.Next := O.Upvalues;
      O.Upvalues := Upvalue;

      return Upvalue;
   end New_Upvalue;

   procedure Free_Objects (O : in out Objects) is
      procedure Free_Functions (Func : in out Obj_Function_Access) is
         Next : Obj_Function_Access := null;
      begin
         while Func /= null loop
            Next := Func.Next;
            Free (Func);
            Func := Next;
         end loop;
      end Free_Functions;

      procedure Free_Closures (Closure : in out Obj_Closure_Access) is
         Next : Obj_Closure_Access := null;
      begin
         while Closure /= null loop
            Next := Closure.Next;
            Free (Closure);
            Closure := Next;
         end loop;
      end Free_Closures;

      procedure Free_Upvalues (Upvalue : in out Obj_Upvalue_Access) is
         Next : Obj_Upvalue_Access := null;
      begin
         while Upvalue /= null loop
            Next := Upvalue.Next;
            Free (Upvalue);
            Upvalue := Next;
         end loop;
      end Free_Upvalues;

   begin
      Free_Functions (O.Functions);
      Free_Closures (O.Closures);
      Free_Upvalues (O.Upvalues);
   end Free_Objects;

   function To_String (Func : Obj_Function) return String is
      use type Unbounded.Unbounded_String;
   begin
      if Func.Name = Unbounded.Null_Unbounded_String then
         return "<script>";
      end if;
      return "<fn " & Unbounded.To_String (Func.Name) & ">";
   end To_String;

   function To_String (Closure : Obj_Closure) return String is
   begin
      return To_String (Closure.Func.all);
   end To_String;

end Lox_Object;
