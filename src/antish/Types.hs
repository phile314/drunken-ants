module Types where


data Token
  = LBRACE
  | RBRACE
  | NEWLINE
  | IF
  | THEN
  | ELSE
  | FOR
  | IN
  | TRY
  | WITH
  | LET
  | PROBABILITY
  | DO
  | OTHERWISE
  | EQ
  | LBRACKET
  | RBRACKET
  | LPAREN
  | RPAREN
  | COMMA
  | OPERATOR String  -- should be generated for these strings: "..", "&&", "||"
  | IDENTIFIER String
  | CDOUBLE Double
  | CINT Int
  
