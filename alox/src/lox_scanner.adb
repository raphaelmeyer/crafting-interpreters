package body Lox_Scanner is

   procedure Init (S : in out Scanner; Source : String) is
   begin
      S.Start := Source'First;
      S.Current := Source'First;
      S.Line := 1;
   end;

end Lox_Scanner;
