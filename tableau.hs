import ExpParser (Expr(..), exprParser)
import Text.Parsec (parse)
import Data.Tree (Tree(..), drawTree)
import Data.List (nub)

-- Função para remover espaços e hífens de uma string
removeInconveniences :: String -> String
removeInconveniences = filter (`notElem` [' ', '-'])

-- Função para aplicar regras de prova e construir a árvore
applyRules :: (Bool, Expr) -> [[(Bool, Expr)]]
applyRules (True, Atom _) = [[]]
applyRules (False, Atom _) = [[]]
applyRules (v, Not f) = [[(not v, f)]]
applyRules (True, And f1 f2) = [[(True, f1), (True, f2)]]
applyRules (False, And f1 f2) = [[(False, f1)], [(False, f2)]]
applyRules (True, Or f1 f2) = [[(True, f1)], [(True, f2)]]
applyRules (False, Or f1 f2) = [[(False, f1), (False, f2)]]
applyRules (True, Imply f1 f2) = [[(False, f1)], [(True, f2)]]
applyRules (False, Imply f1 f2) = [[(True, f1), (False, f2)]]

-- Função para construir a árvore de prova recursivamente
buildProofTree :: [(Bool, Expr)] -> Tree (Bool, Expr)
buildProofTree [] = Node (True, Atom ' ') []
buildProofTree ((v, f):fs) =
  let branches = applyRules (v, f)
  in Node (v, f) (map (buildProofTree . (++ fs)) branches)

-- Função principal para construir a árvore de prova a partir de uma fórmula
proofTree :: Expr -> Tree (Bool, Expr)
proofTree formula = buildProofTree [(False, formula)]

-- Verifica se uma árvore de refutação é válida
isValidRefutationTree :: Tree (Bool, Expr) -> Bool
isValidRefutationTree (Node (val, expr) children)
  | null children = not val && isAtomic expr
  | val = False
  | otherwise = all isValidRefutationTree children

-- Verifica se uma expressão é um átomo
isAtomic :: Expr -> Bool
isAtomic (Atom _) = True
isAtomic _ = False

-- Função principal
main :: IO ()
main = do
    putStrLn "Insira uma fórmula lógica: "
    formula <- getLine
    let formulaWithoutInconveniences = removeInconveniences formula
    let parsedFormula = parse exprParser "" formulaWithoutInconveniences

    case parsedFormula of
        Left _ -> error "Invalid formula"
        Right expr -> do
            let tree = proofTree expr
            putStrLn $ drawTree $ fmap showNode tree
            if isValidRefutationTree tree
                then putStrLn "A árvore de refutação é válida."
                else putStrLn "A árvore de refutação não é válida."
  where
    showNode (v, f) = (if v then "v: " else "f: ") ++ show f
