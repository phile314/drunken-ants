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

--    [\[]] $digit+ ".." $digit+ [\]            { \s -> TokenFromTo  }
    "&&"			{ \s -> TokenAND		}
    "||"			{ \s -> TokenOR			}
    "!"       { \_ -> TokenNOT    }

	[\{]			{ \s -> TokenLBRACE		}
	[\}]			{ \s -> TokenRBRACE		}
	[\[]			{ \s -> TokenLBRACKET		}
	[\]]			{ \s -> TokenRBRACKET		}
	[\(]			{ \s -> TokenLPAREN		}
	[\)]			{ \s -> TokenRPAREN		}

    [A-Z][$alpha $digit]*   { \s -> TokenConst s }
	[a-z][$alpha $digit]*	{ \s -> TokenIdent s }
	$digit+			        { \s -> TokenInt (read s)	}
	[$digit]*$point[$digit]+ { \s -> TokenDouble (read s)}
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
	
main = do
	s <- getContents
	print (alexScanTokens s)

{- A lexer/scanner for the language using Alex. -}
lexer :: String -> [Token]
lexer = alexScanTokens

}
