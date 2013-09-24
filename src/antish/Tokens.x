{
module Tokens where
}

%wrapper "basic"

$digit 		= 0-9
$alpha 		= [a-zA-Z]
$dot		= [\.]
$comma		= [\,]

tokens :-
	$white+			;
	"--".*			;
	
	$dot			{ \s -> Dot		}
	$comma			{ \s -> Comma 		}

	"if"			{ \s -> TokenIF 		}
	"else"			{ \s -> TokenELSE		}
	"then"			{ \s -> TokenTHEN 		}
	"for"			{ \s -> TokenFOR		}
	"in"			{ \s -> TokenIN			}
	"with"			{ \s -> TokenWITH		}
	"let"			{ \s -> TokenLET		}
	"probability"		{ \s -> TokenPROBABILITY	}
	"do"			{ \s -> TokenDO			}
	"otherwise"		{ \s -> TokenOTHERWISE		}
	"eq"			{ \s -> TokenEQ			}

	"{"			{ \s -> TokenLBRACE		}
	"}"			{ \s -> TokenRBRACE		}
	"["			{ \s -> TokenLBRACKET		}
	"]"			{ \s -> TokenRBRACKET		}
	"("			{ \s -> TokenLPAREN		}
	")"			{ \s -> TokenRPAREN		}

	[\_]			{ \s -> TokenUnderscore	}

	[$alpha $digit \+ \-]+	{ \s -> TokenIdent s		}
{
-- Each action has type :: String -> Token
-- The token type:
data Token
  = Dot
  | Comma
  | TokenNEWLINE
  | TokenIF
  | TokenTHEN
  | TokenELSE
  | TokenFOR
  | TokenIN
  | TokenTRY
  | TokenWITH
  | TokenLET
  | TokenPROBABILITY
  | TokenDO
  | TokenOTHERWISE
  | TokenEQ
  | TokenLBRACE
  | TokenRBRACE
  | TokenLBRACKET
  | TokenRBRACKET
  | TokenLPAREN
  | TokenRPAREN
  | TokenIdent String
  deriving (Eq, Show)
	
main = do
	s <- getContents
	print (alexScanTokens s)

{- A lexer/scanner for the language using Alex. -}
lexer :: String -> [Token]
lexer = alexScanTokens

}
