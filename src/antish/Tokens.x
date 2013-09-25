{
module Tokens where
}

%wrapper "basic"

$newline	= [\r\n]
$digit 		= 0-9
$alpha 		= [a-zA-Z]
$point		= [\.]
$comma		= [\,]

tokens :-
	$newline+		{ \s -> NewLine			}
	$white+			;
	"--".*			;
	
	$comma			{ \s -> Comma			}

	"if"			{ \s -> TokenIF 		}
	"then"			{ \s -> TokenTHEN 		}
	"else"			{ \s -> TokenELSE		}
	"for"			{ \s -> TokenFOR		}
	"in"			{ \s -> TokenIN			}
	"try"			{ \s -> TokenTRY		}
	"with"			{ \s -> TokenWITH		}
	"catch"			{ \s -> TokenCATCH		}
	"let"			{ \s -> TokenLET		}
	"probability"		{ \s -> TokenPROBABILITY	}
	"do"			{ \s -> TokenDO			}
	"otherwise"		{ \s -> TokenOTHERWISE		}
	"="			{ \s -> TokenDEF		}

	"&&"			{ \s -> TokenAND		}
	"||"			{ \s -> TokenOR			}

	[\{]			{ \s -> TokenLBRACE		}
	[\}]			{ \s -> TokenRBRACE		}
	[\[]			{ \s -> TokenLBRACKET		}
	[\]]			{ \s -> TokenRBRACKET		}
	[\(]			{ \s -> TokenLPAREN		}
	[\)]			{ \s -> TokenRPAREN		}

	[a-z][$alpha $digit]*	{ \s -> TokenIdent s		}
	$digit+			{ \s -> TokenInt i		}
	[$digit]*$point[$digit]+ { \s -> TokenDouble d		}
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
  | TokenAND
  | TokenOR
  | TokenLBRACE
  | TokenRBRACE
  | TokenLBRACKET
  | TokenRBRACKET
  | TokenLPAREN
  | TokenRPAREN
  | TokenIdent String
  | TokenInt Int
  | TokenDouble Double
  deriving (Eq, Show)
	
main = do
	s <- getContents
	print (alexScanTokens s)

{- A lexer/scanner for the language using Alex. -}
lexer :: String -> [Token]
lexer = alexScanTokens

}
