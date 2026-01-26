with Ada.Strings.Unbounded;

package Lox_Scanner is

   type Source_Code is access constant String;

   package Unbounded renames Ada.Strings.Unbounded;
   subtype Unbounded_String is Unbounded.Unbounded_String;

   type Scanner is record
      Source  : Source_Code;
      Start   : Positive;
      Current : Positive;
      Line    : Natural;
   end record;

   type TokenType is
     (
     --  Single-character tokens.
     TOKEN_LEFT_PAREN,
      TOKEN_RIGHT_PAREN,
      TOKEN_LEFT_BRACE,
      TOKEN_RIGHT_BRACE,
      TOKEN_COMMA,
      TOKEN_DOT,
      TOKEN_MINUS,
      TOKEN_PLUS,
      TOKEN_SEMICOLON,
      TOKEN_SLASH,
      TOKEN_STAR,

      --  One or two character tokens.
      TOKEN_BANG,
      TOKEN_BANG_EQUAL,
      TOKEN_EQUAL,
      TOKEN_EQUAL_EQUAL,
      TOKEN_GREATER,
      TOKEN_GREATER_EQUAL,
      TOKEN_LESS,
      TOKEN_LESS_EQUAL,

      --  Literals.
      TOKEN_IDENTIFIER,
      TOKEN_STRING,
      TOKEN_NUMBER,

      --  Keywords.
      TOKEN_AND,
      TOKEN_CLASS,
      TOKEN_ELSE,
      TOKEN_FALSE,
      TOKEN_FOR,
      TOKEN_FUN,
      TOKEN_IF,
      TOKEN_NIL,
      TOKEN_OR,
      TOKEN_PRINT,
      TOKEN_RETURN,
      TOKEN_SUPER,
      TOKEN_THIS,
      TOKEN_TRUE,
      TOKEN_VAR,
      TOKEN_WHILE,

      TOKEN_ERROR,
      TOKEN_EOF);

   type Token is record
      Kind   : TokenType;
      Lexeme : Unbounded_String;
      Line   : Natural;
   end record;

   function Init (Source : Source_Code) return Scanner;

   function Scan_Token (S : in out Scanner) return Token;

private
   function Is_At_End (S : in out Scanner) return Boolean;
   function Advance (S : in out Scanner) return Character;
   function Peek (S : in out Scanner) return Character;
   function Match (S : in out Scanner; Expected : Character) return Boolean;

   function Make_Token (S : in out Scanner; Kind : TokenType) return Token;
   function Error_Token (S : in out Scanner; Message : String) return Token;
   procedure Skip_Whitespace (S : in out Scanner);

end Lox_Scanner;
