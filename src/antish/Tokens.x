{
module Lexer where
}

%wrapper "posn"

$newline	= [\r\n]
$digit 		= 0-9
$alpha 		= [a-zA-Z]
$point		= [\.]
$comma		= [\,]

tokens :-
	$newline+		{ tok1 NewLine			}
	$white+			;
	"--".*			;
	
	$comma			{ tok1 Comma			}

	"if"			{ tok1 TokenIF 		}
	"then"			{ tok1 TokenTHEN 		}
	"else"			{ tok1 TokenELSE		}
	"for"			{ tok1 TokenFOR		}
	"in"			{ tok1 TokenIN			}
	"try"			{ tok1 TokenTRY		}
	"with"			{ tok1 TokenWITH		}
	"catch"			{ tok1 TokenCATCH		}
	"let"			{ tok1 TokenLET		}
	"probability"   { tok1 TokenPROBABILITY	}
	"do"			{ tok1 TokenDO			}
	"otherwise"		{ tok1 TokenOTHERWISE		}
	"="             { tok1 TokenDEF		}

--    [\[]] $digit+ ".." $digit+ [\]            { \s -> TokenFromTo  }
    "&&"			{ tok1 TokenAND		}
    "||"			{ tok1 TokenOR			}
    "!"             { tok1 TokenNOT    }

	[\{]			{ tok1 TokenLBRACE		}
	[\}]			{ tok1 TokenRBRACE		}
	[\[]			{ tok1 TokenLBRACKET		}
	[\]]			{ tok1 TokenRBRACKET		}
	[\(]			{ tok1 TokenLPAREN		}
	[\)]			{ tok1 TokenRPAREN		}

    [A-Z][$alpha $digit]*   { tok $ \s -> TokenConst s }
	[a-z][$alpha $digit]*	{ tok $ \s -> TokenIdent s }
	$digit+			        { tok $ \s -> TokenInt (read s)	}
	[$digit]*$point[$digit]+ { tok $ \s -> TokenDouble (read s)}
{
-- Each action has type :: String -> Token
-- The token type:
data Token
  = NewLine
  | Comma
  | TokenNEWLINE
  | TokenIF
  | TokenTHEN
  | TokenELSE
  | TokenFOR
  | TokenIN
  | TokenTRY
  | TokenWITH
  | TokenCATCH
  | TokenLET
  | TokenPROBABILITY
  | TokenDO
  | TokenOTHERWISE
  | TokenDEF
--  | TokenFROM_TO
  | TokenAND
  | TokenOR
  | TokenNOT
  | TokenLBRACE
  | TokenRBRACE
  | TokenLBRACKET
  | TokenRBRACKET
  | TokenLPAREN
  | TokenRPAREN
  | TokenConst String
  | TokenIdent String
  | TokenInt Int
  | TokenDouble Double
  deriving (Eq, Show)
	

type TokenPos = (Int, Int)

tok :: (String -> Token) -> AlexPosn -> String -> (TokenPos, Token)
tok f a s = (a2tp a, f s)

tok1 :: Token -> AlexPosn -> String -> (TokenPos, Token)
tok1 t a s = (a2tp a, t)

a2tp :: AlexPosn -> TokenPos
a2tp (AlexPn _ l c) = (l, c)

main = do
	s <- getContents
	print (alexScanTokens s)

{- A lexer/scanner for the language using Alex. -}
lexer :: String -> [(TokenPos, Token)]
lexer = alexScanTokens

}
