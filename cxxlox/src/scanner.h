#pragma once

#include <memory>
#include <string_view>

enum class TokenType {
  // Single-character tokens.
  LEFT_PAREN,
  RIGHT_PAREN,
  LEFT_BRACE,
  RIGHT_BRACE,
  COMMA,
  DOT,
  MINUS,
  PLUS,
  SEMICOLON,
  SLASH,
  STAR,

  // One or two character tokens.
  BANG,
  BANG_EQUAL,
  EQUAL,
  EQUAL_EQUAL,
  GREATER,
  GREATER_EQUAL,
  LESS,
  LESS_EQUAL,

  // Literals.
  IDENTIFIER,
  STRING,
  NUMBER,

  // Keywords.
  AND,
  CLASS,
  ELSE,
  FALSE,
  FOR,
  FUN,
  IF,
  NIL,
  OR,
  PRINT,
  RETURN,
  SUPER,
  THIS,
  TRUE,
  VAR,
  WHILE,

  ERROR,
  END
};

struct Token {
  TokenType type;
  std::string_view::const_iterator start;
  std::size_t length;
  std::size_t line;
};

class Scanner {
public:
  virtual ~Scanner() = default;

  virtual void init_scanner(std::string_view source) = 0;
  virtual Token scan_token() = 0;

  static std::unique_ptr<Scanner> create();
};
