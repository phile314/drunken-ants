{
module Parser where

import Tokens
}

%name arrow
%tokentype { Token }
%error { parseError }

%token
	'.' 				{ Dot			}
	','				{ Comma 		}
	if				{ TokenIF 		}
	else				{ TokenTHEN		}
	then				{ TokenELSE 		}
	for				{ TokenFOR		}
	in				{ TokenIN		}
	try				{ TokenTRY		}
	with				{ TokenWITH		}
	let				{ TokenLET		}
	probability			{ TokenPROBABILITY	}
	do				{ TokenDO		}
	otherwise			{ TokenOTHERWISE	}
	eq				{ TokenEQ		}

	'{'				{ TokenLBRACE 		}
	'}'				{ TokenRBRACE 		}
	'['				{ TokenLBRACKET 	}
	']'				{ TokenRBRACKET 	}
	'('				{ TokenLPAREN 		}
	')'				{ TokenRPAREN 		}
	'_'				{ TokenUnderscore	}
	
	ident				{ TokenIdent $$		}

%%
	
Program	: {- empty -}          			{ [] }
	| StmBlock				{ $1 }


StmBlock : StmBlock ',' Stm			{ $3 : $1 }
	| StmBlock ','				{ $1 }
	| Stm					{ [$1] }
	| {- empty -}				{ [] }


Stm	: if Expr then StmBlock else StmBlock			{ If $2 $4 $6 }
	| if Expr then StmBlock 				{ If $2 $4 }
	| for Var in Expr StmBlock				{ For $2 $4 $5 }
	| for Expr StmBlock					{ For $2 $3 }
	| try StmBlock with StmBlock catch StmBlock 		{ Try $2 $4 $6 }
	| try StmBlock with StmBlock 		 		{ Try $2 $4 }
	| let Bindings in StmBlock				{ Let $2 $4 }
	| with probability ident do StmBlock otherwise StmBlock	{ Prop $3 $5 $7 }

Bindings : Bindings ',' Binding			{ $3 : $1 }
	| Bindings ','				{ $1 }
	| Binding				{ [$1] }
	| {- empty -}				{ [] }

Binding	: ident eq Expr 			{ Bind $1 $3 }
	| ident eq StmBlock			{ Bind $1 $3 } 

funCall =
    identifier [expr {expr}]

bexpr =
    scond sdir
  | expr "&&" expr
  | expr "||" expr

-- We could also implement this by defining all these values as built-in functions?
scond =
    Friend
  | Foe
  | FriendWithFood
  | FoeWithFood
  | Food
  | Rock
  | Marker iexpr
  | FoeMarker
  | Home
  | FoeHome

sdir =
    Here
  | Ahead
  | LeftAhead
  | RightAhead

expr =
  | lexpr
  | iexpr
  | bexpr
  | turnDir
  | funCall
  | "(" expr ")"


turnDir = Left | Right

lexpr =
    "[" expr ".." expr "]"
  | "[" expr { "," expr} "]"


iexpr =
    INTEGER
  | var

var =
    identifier



Rule	: ident '->' Cmds '.'			{ Rule $1 $3 }

Cmds 	: Cmds ',' Cmd				{ $3 : $1 }
	| Cmds ','				{ $1 }
	| Cmd					{ [$1] }
	| {- empty -}				{ [] }

Cmd	: go					{ Go }
	| take 					{ Take }
	| mark					{ Mark }
	| nothing				{ None }
	| turn Dir				{ Turn $2 }
	| case Dir of Alts end			{ Case $2 $4 }
	| ident 				{ Var $1 }

Dir	: left					{ LeftTurn }
	| right					{ RightTurn }
	| front					{ Front }


Alts 	: Alts ';' Alt				{ $3 : $1 }
	| Alts ';'				{ $1 }
	| Alt					{ [$1] }
	| {- empty -}				{ [] }

Alt	: Pat '->' Cmds				{ Alt $1 $3 }

Pat 	: empty					{ Empty }
	| lambda				{ Lambda }
	| debris				{ Debris }
	| asteroid				{ Asteroid }
	| boundary				{ Boundary }
	| '_'					{ Underscore }
	
{

{- 2. Define a suitable abstract syntax for the Arrow language. -}
type Program = [Rule]
data Rule 	= Rule Ident Cmds 	
			deriving Show

type Cmds 	= [Cmd]
data Cmd 	= Go | Take | Mark | None
			| Turn Dir
			| Case Dir Alts
			| Var Ident
			deriving Show

data Dir	= LeftTurn 
			| RightTurn 
			| Front	
			deriving Show

type Alts	= [Alt]
data Alt 	= Alt Pat Cmds			
			deriving Show

data Pat 	= Empty 
			| Lambda 
			| Debris 
			| Asteroid 
			| Boundary 
			| Underscore
			deriving (Show,Eq,Ord)

type Ident = String

parseError :: [Token] -> a
parseError xs = error ("Parse error "  ++ (foldr (\x y -> show x ++ "," ++ y) "" xs))

{- 3. Write a parser for the language using Happy. -}
parseString :: String -> Program
parseString = arrow . lexer

lexString = lexer

{- 4. What can you find out from the Happy documentation over Happys handling of left-recursive and right-recursive grammars.

Happy wordt veelal left-recursive gebruikt, 2 puntjes uit de user-guide.

-The only reason we used left recursion is that Happy is more ef?cient at parsing left-recursive rules
-Use left recursion rather than right recursion wherever possible. While not strictly a performance issue, this affects the size of
       the parser stack, which is kept on the heap and thus needs to be garbage collected
       
Bij parser-combinators word left-recursive juist vermeden zodat er geen sprake kan zijn van eventuele cyclische structuren.
-}


}
