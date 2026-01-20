with Lox_Scanner;

package body Lox_Compiler is

   procedure Compile (Source : String) is
      Scanner : Lox_Scanner.Scanner;
   begin
      Lox_Scanner.Init (Scanner, Source);
   end Compile;

end Lox_Compiler;
