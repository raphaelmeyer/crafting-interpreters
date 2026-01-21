package body Lox_Scanner is

   function Init (Source : Source_Code) return Scanner is
   begin
      return
        Scanner'
          (Source  => Source,
           Start   => Source'First,
           Current => Source'First,
           Line    => 1);
   end Init;

   function Scan_Token (S : in out Scanner) return Token is
   begin
      S.Start := S.Current;

      if Is_At_End (S) then
         return Make_Token (S, TOKEN_EOF);
      end if;

      return Error_Token (S, "Unexpected character.");
   end Scan_Token;

   function Is_At_End (S : in out Scanner) return Boolean is
   begin
      return S.Current > S.Source'Last;
   end Is_At_End;

   function Make_Token (S : in out Scanner; Kind : TokenType) return Token is
      To : constant Natural := Natural'Pred (S.Current);
   begin
      if S.Source'Last < To then
         return
           (Kind,
            Lexeme => Unbounded.To_Unbounded_String (""),
            Line   => S.Line);
      end if;

      return
        (Kind,
         Lexeme => Unbounded.To_Unbounded_String (S.Source (S.Start .. To)),
         Line   => S.Line);
   end Make_Token;

   function Error_Token (S : in out Scanner; Message : String) return Token is
   begin
      return
        (TOKEN_ERROR,
         Lexeme => Unbounded.To_Unbounded_String (Message),
         Line   => S.Line);
   end Error_Token;

end Lox_Scanner;
