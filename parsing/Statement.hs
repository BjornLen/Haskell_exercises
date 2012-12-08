module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip | --    Begin [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T 
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept ";" buildSkip
buildSkip = Skip

if_statement = accept "if" -# Expr.parse  #- require "then" # stmt #- require "else" # stmt >-> buildIf
buildIf((a, s1), s2) = If a s1 s2 

while_statement = accept "while" -# Expr.parse #- require "do" # stmt  >-> build_while
build_while(e, s) = While e s

read = accept "read" -# word #- require ";" >-> build_read
build_read(v) = Read v

write = accept "write" -# word #- require ";" >-> build_write
build_write(v) = Write v

stmt = error "not implemented" 


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = stmt -- error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"
