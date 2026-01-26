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
         if Is_Alpha (C) then
            return Identifier (S);
         end if;

         if Is_Digit (C) then
            return Number_Literal (S);
         end if;

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

            when '"'    =>
               return String_Literal (S);

            when others =>
               null;
         end case;
      end;

      return Error_Token (S, "Unexpected character.");
   end Scan_Token;

   function Is_Alpha (C : Character) return Boolean is
   begin
      case C is
         when 'a' .. 'z' | 'A' .. 'Z' | '_' =>
            return True;

         when others                        =>
            return False;
      end case;
   end Is_Alpha;

   function Is_Digit (C : Character) return Boolean is
   begin
      case C is
         when '0' .. '9' =>
            return True;

         when others     =>
            return False;
      end case;
   end Is_Digit;

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

   function Peek_Next (S : in out Scanner) return Character is
   begin
      if Is_At_End (S) then
         return Ada.Characters.Latin_1.NUL;
      end if;
      return S.Source (Positive'Succ (S.Current));
   end Peek_Next;

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

               when '/'           =>
                  if Peek_Next (S) = '/' then
                     while Peek (S) /= LF and then not Is_At_End (S) loop
                        Unused := Advance (S);
                     end loop;
                  else
                     return;
                  end if;

               when others        =>
                  return;
            end case;

         end;
      end loop;
   end Skip_Whitespace;

   function Check_Keyword
     (S : in out Scanner; Keyword : String; Kind : TokenType) return TokenType
   is
      To : constant Natural := Natural'Pred (S.Current);
   begin
      if S.Source (S.Start .. To) = Keyword then
         return Kind;
      end if;

      return TOKEN_IDENTIFIER;
   end Check_Keyword;

   function Identifier_Type (S : in out Scanner) return TokenType is
   begin
      case S.Source (S.Start) is
         when 'a'    =>
            return Check_Keyword (S, "and", TOKEN_AND);

         when 'c'    =>
            return Check_Keyword (S, "class", TOKEN_CLASS);

         when 'e'    =>
            return Check_Keyword (S, "else", TOKEN_ELSE);

         when 'f'    =>
            if Positive'Succ (S.Start) < S.Current then
               case S.Source (Positive'Succ (S.Start)) is
                  when 'a'    =>
                     return Check_Keyword (S, "false", TOKEN_FALSE);

                  when 'o'    =>
                     return Check_Keyword (S, "for", TOKEN_FOR);

                  when 'u'    =>
                     return Check_Keyword (S, "fun", TOKEN_FUN);

                  when others =>
                     null;
               end case;
            end if;

         when 'i'    =>
            return Check_Keyword (S, "if", TOKEN_IF);

         when 'n'    =>
            return Check_Keyword (S, "nil", TOKEN_NIL);

         when 'o'    =>
            return Check_Keyword (S, "or", TOKEN_OR);

         when 'p'    =>
            return Check_Keyword (S, "print", TOKEN_PRINT);

         when 'r'    =>
            return Check_Keyword (S, "return", TOKEN_RETURN);

         when 's'    =>
            return Check_Keyword (S, "super", TOKEN_SUPER);

         when 't'    =>
            if Positive'Succ (S.Start) < S.Current then
               case S.Source (Positive'Succ (S.Start)) is
                  when 'h'    =>
                     return Check_Keyword (S, "this", TOKEN_THIS);

                  when 'r'    =>
                     return Check_Keyword (S, "true", TOKEN_TRUE);

                  when others =>
                     null;
               end case;
            end if;

         when 'v'    =>
            return Check_Keyword (S, "var", TOKEN_VAR);

         when 'w'    =>
            return Check_Keyword (S, "while", TOKEN_WHILE);

         when others =>
            null;
      end case;

      return TOKEN_IDENTIFIER;
   end Identifier_Type;

   function Identifier (S : in out Scanner) return Token is
      Unused : Character;
   begin
      while Is_Alpha (Peek (S)) or else Is_Digit (Peek (S)) loop
         Unused := Advance (S);
      end loop;
      return Make_Token (S, Identifier_Type (S));
   end Identifier;

   function Number_Literal (S : in out Scanner) return Token is
      Unused : Character;
   begin
      while Is_Digit (Peek (S)) loop
         Unused := Advance (S);
      end loop;

      if Peek (S) = '.' and then Is_Digit (Peek_Next (S)) then
         --  consume the '.'
         Unused := Advance (S);

         while Is_Digit (Peek (S)) loop
            Unused := Advance (S);
         end loop;
      end if;

      return Make_Token (S, TOKEN_NUMBER);
   end Number_Literal;

   function String_Literal (S : in out Scanner) return Token is
      Unused : Character;
      use Ada.Characters.Latin_1;
   begin
      while Peek (S) /= '"' and then not Is_At_End (S) loop
         if Peek (S) = LF then
            S.Line := Natural'Succ (S.Line);
         end if;
         Unused := Advance (S);
      end loop;

      if Is_At_End (S) then
         return Error_Token (S, "Unterminated string.");
      end if;

      Unused := Advance (S);
      return Make_Token (S, TOKEN_STRING);
   end String_Literal;

end Lox_Scanner;
