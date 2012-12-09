module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program  [Statement.T] 
instance Parse T where
  parse = parse_stmts
  toString =  shw

parse_stmts s

shw::T->String
shw ss = toString ss



exec (Program [stmts]) ins  = Statement.exec [stmts] Dictionary.empty ins
