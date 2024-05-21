import ExpParser (Expr(..), exprParser)
import Text.Parsec (parse)
import Data.Tree (Tree(..), drawTree)
import Data.List (nub)

-- Função para remover espaços e hífens de uma string
removeInconveniences :: String -> String
removeInconveniences = filter (`notElem` [' ', '-'])

-- Função para aplicar regras de prova e construir a árvore
applyRules :: (Bool, Expr) -> [(Bool, Expr)]
applyRules (True, Atom _) = []
applyRules (False, Atom _) = []
applyRules (v, Not f) = [(not v, f)]
applyRules (True, And f1 f2) = [(True, f1), (True, f2)]
applyRules (False, And f1 f2) = [(False, f1), (False, f2)]
applyRules (True, Or f1 f2) = [(True, f1), (True, f2)]
applyRules (False, Or f1 f2) = [(False, f1)]
applyRules (True, Imply f1 f2) = [(False, f1), (True, f2)]
applyRules (False, Imply f1 f2) = [(True, f1), (False, f2)]

-- Função para construir a árvore de prova recursivamente
buildProofTree :: [(Bool, Expr)] -> Tree (Bool, Expr)
buildProofTree [] = error "No formulas to process"
buildProofTree ((v, f):fs) = Node (v, f) (map buildProofTree branches)
  where
    newFormulas = applyRules (v, f) ++ fs
    branches = case applyRules (v, f) of
                 [] -> [newFormulas]
                 [x] -> [newFormulas]
                 xs -> map (\x -> nub (x:fs)) xs

-- Função principal para construir a árvore de prova a partir de uma fórmula
proofTree :: Expr -> Tree (Bool, Expr)
proofTree formula = buildProofTree [(True, formula)]

main :: IO ()
main = do
    putStrLn "Insira uma fórmula lógica: "
    formula <- getLine
    let formulaWithoutInconveniences = removeInconveniences formula
    let parsedFormula = parse exprParser "" formulaWithoutInconveniences
    
    let tree = case parsedFormula of
                    Left _ -> error "Invalid formula"
                    Right expr -> proofTree expr
    putStrLn $ drawTree $ fmap showNode tree
  where
    showNode (v, f) = (if v then "v: " else "f: ") ++ show f
