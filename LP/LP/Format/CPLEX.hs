module LP.Format.CPLEX where 
import Helpers.Common
import qualified Data.Bimap as BM

data Var = Var String
data Operator = OEQ | OLT | OLTE | OGT | OGTE 
data Constraint = Constraint String Formula Operator Coef
data Objective = Objective Dir Formula
data Dir = Max | Min
data Formula = Formula [(Coef, Var)]
type Coef = Double
data Bound = Bound Coef Var Coef
data LP = LP Objective [Constraint] [Bound] 

writeDir Max = text "Maximize" 
writeDir Min = text "Minimize" 
    
writeVar (Var v) = text $ clean v  

writeOperator op = text $ case op of 
                     OEQ -> "="
                     OLTE-> "<="
                     OLT -> "<"
                     OGT -> ">"
                     OGTE -> ">="

writeFormula (Formula formula) = 
    vcat $ do
      (c, v) <- formula
      return $  
             if c >= 0 then text "+" <+> (text $ show c) <+> (writeVar v)
             else  text "-" <+> (text $ show (-c)) <+> (writeVar v)

writeObjective (Objective dir formula) =
    (writeDir dir) $$
    nest 2 (text "optimize:" <+> writeFormula formula)
    
writeConstraint (Constraint name formula operator n) = 
    text (name ++ ":") <+>
    writeFormula formula <+> 
    writeOperator operator <+>
    (text $ show n)

writeBound (Bound a x b) = 
    text (show a) <+> text "<=" <+> writeVar x <+> text "<=" <+> text (show b)
                

writeCplex (LP obj cons bounds) =
    writeObjective obj $$
    (text "Subject To") $$ 
    nest 2 (vcat $ map writeConstraint cons) $$
    (text "Bounds") $$ 
    nest 2 (vcat $ map writeBound bounds) $$
    (text "End") 
                     
testLP = LP (Objective Max (Formula [(1, Var "A")])) [Constraint "c1" (Formula [(1, Var "A"),(2, Var "C")]) OEQ 10] [Bound 0 (Var "A") 100]

trans = BM.fromList                 
        [(',' , "_COMMA_"), 
         ( '$' , "_DOLLAR_"),
         ( '`' , "_BQUOTE_"),
         ( '\'' , "_QUOTE_"),
         ( '&' , "_AMP_"),
         ( ':' , "_COLON_"),
         ( '\\' , "_SLASH_"),
         ( '/' , "_FSLASH_"),
         ( '%' , "_PERCENT_"),
         ( '@' , "_AT_"),
         ( '=' , "_EQ_"),
         ( '|' , "_PIPE_"),
         ( '^' , "_CARAT_"),
         ( ';' , "_SEMI_"),
         ( '#' , "_POUND_")]

clean = 
    concatMap (\c -> case BM.lookup c trans of
                                   Just n -> n
                                   Nothing -> [c]) 
