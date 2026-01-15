package body Lox_Chunk is

   procedure Write (C : in out Chunk; B : Byte; Line : Natural) is
   begin
      C.Code.Append (B);
      C.Lines.Append (Line);
   end Write;

   procedure Write (C : in out Chunk; O : Op_Code; Line : Natural) is
   begin
      C.Code.Append (O'Enum_Rep);
      C.Lines.Append (Line);
   end Write;

   function Add_Constant
     (C : in out Chunk; Value : Lox_Value.Value) return Natural is
   begin
      Lox_Value.Write (C.Constants, Value);
      return C.Constants.Last_Index;
   end Add_Constant;

end Lox_Chunk;
