with Exceptions;
with Main;

with Ada.Command_Line;

procedure ALox is
begin
   Main.Main;

exception
   when Exceptions.Usage_Error =>
      Ada.Command_Line.Set_Exit_Status (64);

   when Exceptions.Compile_Error =>
      Ada.Command_Line.Set_Exit_Status (65);

   when Exceptions.Runtime_Error =>
      Ada.Command_Line.Set_Exit_Status (70);

   when Exceptions.File_Not_Found =>
      Ada.Command_Line.Set_Exit_Status (74);

end ALox;
