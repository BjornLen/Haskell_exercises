module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip | 
    Begin [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T 
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" -# require ";" >-> buildSkip 
buildSkip(v) = Skip

if_statement = accept "if" -# Expr.parse  #- require "then" # stmt #- require "else" # stmt >-> buildIf
buildIf((a, s1), s2) = If a s1 s2 

while_statement = accept "while" -# Expr.parse #- require "do" # stmt  >-> build_while
build_while(e, s) = While e s

read = accept "read" -# word #- require ";" >-> build_read
build_read(v) = Read v

write = accept "write" -# Expr.parse  #- require ";" >-> build_write
build_write(v) = Write v

begin_stmt = accept "begin" -# iter stmt  #- require "end" >-> build_begin
build_begin(s) = Begin s


stmt = assignment ! Statement.read ! write ! if_statement ! while_statement ! skip ! begin_stmt


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Assignment v e : stmts ) dict input = 
    (exec stmts (Dictionary.insert (v, Expr.value e dict) dict) input)
exec (Skip :stmts ) dict input = exec stmts dict input
exec (Begin (s:ss) : stmts) dict input =
   ( exec (s : (concat [ss,stmts])) dict input)
exec (While cond todo : stmts) dict input = 
    if (Expr.value cond dict) > 0  
    then exec (todo: (While cond todo:stmts)) dict input
    else exec stmts dict input
exec (Read v :stmts) dict input =
  exec stmts (Dictionary.insert (v, input !! 0 ) dict) (tail input) 
exec (Write e : stmts) dict input = 
    (Expr.value  e dict):(exec stmts dict input)
exec [] _ _ = [] 


shw :: Statement -> String
shw (If conf thenS elseS) = 
    "if "++(toString conf)++">0 then\n"++(toString thenS)++"else\n"++(toString elseS)++"endIf\n"
shw (Skip) = "skip;\n"
shw (Assignment v e ) = v++" := "++(toString e)++"\n"
shw (While cond todo) = "While "++(toString cond)++">0 do \n"++(toString todo)
shw (Read v) = "read "++v++";\n"
shw (Write e) = "write "++(toString e)++"\n"
shw (Begin ss) = "begin\n"++(concat $ map toString ss)++"end;\n"
  
instance Parse Statement where
  parse = stmt
  toString = shw
