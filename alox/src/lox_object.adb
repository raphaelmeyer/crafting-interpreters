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

   procedure Free_Objects (O : in out Objects) is
      Object : Obj_Function_Access := O.Functions;
      Next   : Obj_Function_Access := null;
   begin
      while Object /= null loop
         Next := Object.Next;
         Free (Object);
         Object := Next;
      end loop;
   end Free_Objects;

   function To_String (Function_Object : Obj_Function) return String is
      use type Unbounded.Unbounded_String;
   begin
      if Function_Object.Name = Unbounded.Null_Unbounded_String then
         return "<script>";
      end if;
      return "<fn " & Unbounded.To_String (Function_Object.Name) & ">";
   end To_String;

end Lox_Object;
