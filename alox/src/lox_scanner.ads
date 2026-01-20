package Lox_Scanner is

   type Scanner is record
      Start   : Positive;
      Current : Positive;
      Line    : Natural;
   end record;

   procedure Init (S : in out Scanner; Source : String);

end Lox_Scanner;
