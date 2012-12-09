module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program  [Statement.T] deriving Show 
instance Parse T where
  parse = (iter Statement.parse) >-> getP  
  toString =  shw

getP ss = Program ss


shw::T->String
shw (Program ss) = concat [toString s_i | s_i <-ss]



exec (Program s) ins  = Statement.exec s Dictionary.empty ins
