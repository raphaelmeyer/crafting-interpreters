package body Lox_Object is
   function New_Function return Obj_Function_Access is
      Func : constant Obj_Function_Access :=
        new Obj_Function'
          (Arity => 0, Name => Unbounded.Null_Unbounded_String, Chunk => <>);
   begin
      Lox_Chunk.Init (Func.Chunk);
      return Func;
   end New_Function;

   function To_String (Function_Object : Obj_Function) return String is
   begin
      return "<fn " & Unbounded.To_String (Function_Object.Name) & ">";
   end To_String;

end Lox_Object;
