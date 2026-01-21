with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Lox_Compiler is

   procedure Compile (Source : Lox_Scanner.Source_Code) is
      Scanner : Lox_Scanner.Scanner;
      Token   : Lox_Scanner.Token;
      Line    : Natural;
      use type Lox_Scanner.TokenType;
   begin
      Scanner := Lox_Scanner.Init (Source);
      Line := 0;
      loop
         Token := Lox_Scanner.Scan_Token (Scanner);
         if Token.Line /= Line then
            Ada.Integer_Text_IO.Put (Token.Line, Width => 4);
            Ada.Text_IO.Put (" ");
            Line := Token.Line;
         else
            Ada.Text_IO.Put ("   | ");
         end if;
         Ada.Text_IO.Put_Line
           (Ada.Strings.Fixed.Head (Token.Kind'Image, 20)
            & " '"
            & Lox_Scanner.Unbounded.To_String (Token.Lexeme)
            & "'");

         exit when Token.Kind = Lox_Scanner.TOKEN_EOF;
      end loop;
   end Compile;

end Lox_Compiler;
