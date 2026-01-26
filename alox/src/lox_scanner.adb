with Ada.Characters.Latin_1;

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
      Skip_Whitespace (S);

      S.Start := S.Current;

      if Is_At_End (S) then
         return Make_Token (S, TOKEN_EOF);
      end if;

      declare
         C : constant Character := Advance (S);
      begin
         case C is
            when '('    =>
               return Make_Token (S, TOKEN_LEFT_PAREN);

            when ')'    =>
               return Make_Token (S, TOKEN_RIGHT_PAREN);

            when '{'    =>
               return Make_Token (S, TOKEN_LEFT_BRACE);

            when '}'    =>
               return Make_Token (S, TOKEN_RIGHT_BRACE);

            when ';'    =>
               return Make_Token (S, TOKEN_SEMICOLON);

            when ','    =>
               return Make_Token (S, TOKEN_COMMA);

            when '.'    =>
               return Make_Token (S, TOKEN_DOT);

            when '-'    =>
               return Make_Token (S, TOKEN_MINUS);

            when '+'    =>
               return Make_Token (S, TOKEN_PLUS);

            when '/'    =>
               return Make_Token (S, TOKEN_SLASH);

            when '*'    =>
               return Make_Token (S, TOKEN_STAR);

            when '!'    =>
               return
                 Make_Token
                   (S,
                    (if Match (S, '=') then TOKEN_BANG_EQUAL else TOKEN_BANG));

            when '='    =>
               return
                 Make_Token
                   (S,
                    (if Match (S, '=')
                     then TOKEN_EQUAL_EQUAL
                     else TOKEN_EQUAL));

            when '<'    =>
               return
                 Make_Token
                   (S,
                    (if Match (S, '=') then TOKEN_LESS_EQUAL else TOKEN_LESS));

            when '>'    =>
               return
                 Make_Token
                   (S,
                    (if Match (S, '=')
                     then TOKEN_GREATER_EQUAL
                     else TOKEN_GREATER));

            when others =>
               null;
         end case;
      end;

      return Error_Token (S, "Unexpected character.");
   end Scan_Token;

   function Is_At_End (S : in out Scanner) return Boolean is
   begin
      return S.Current > S.Source'Last;
   end Is_At_End;

   function Advance (S : in out Scanner) return Character is
      C : constant Character := S.Source (S.Current);
   begin
      S.Current := Positive'Succ (S.Current);
      return C;
   end Advance;

   function Peek (S : in out Scanner) return Character is
   begin
      if S.Current > S.Source'Last then
         return Ada.Characters.Latin_1.NUL;
      end if;
      return S.Source (S.Current);
   end Peek;

   function Match (S : in out Scanner; Expected : Character) return Boolean is
   begin
      if Is_At_End (S) then
         return False;
      end if;

      if S.Source (S.Current) /= Expected then
         return False;
      end if;

      S.Current := Positive'Succ (S.Current);
      return True;
   end Match;

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

   procedure Skip_Whitespace (S : in out Scanner) is
      Unused : Character;
   begin
      loop
         declare
            C : constant Character := Peek (S);
            use Ada.Characters.Latin_1;
         begin
            case C is
               when ' ' | CR | HT =>
                  Unused := Advance (S);

               when LF            =>
                  S.Line := Natural'Succ (S.Line);
                  Unused := Advance (S);

               when others        =>
                  return;
            end case;

         end;
      end loop;
   end Skip_Whitespace;

end Lox_Scanner;
