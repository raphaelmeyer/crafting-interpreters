package body Lox_Object is
   function New_Function (O : in out Objects) return Obj_Function_Access is
      Func : constant Obj_Function_Access :=
        new Obj_Function'
          (Arity => 0,
           Name  => Unbounded.Null_Unbounded_String,
           Chunk => <>,
           Next  => <>);
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
        new Obj_Closure'(Func => Func, Next => <>);
   begin
      Closure.Next := O.Closures;
      O.Closures := Closure;

      return Closure;
   end New_Closure;

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
   begin
      Free_Functions (O.Functions);
      Free_Closures (O.Closures);
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
