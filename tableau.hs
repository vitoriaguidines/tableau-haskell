import ExpParser (Expr(..), exprParser)
import Text.Parsec (parse)
import Data.Tree (Tree(..), drawTree, flatten)
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
applyRules (False, Or f1 f2) = [(False, f1), (False, f2)]
applyRules (True, Imply f1 f2) = [(False, f1), (True, f2)]
applyRules (False, Imply f1 f2) = [(True, f1), (False, f2)]

-- Função para aplicar regras de prova com ramificação
applyRulesWithBranching :: (Bool, Expr) -> (Bool, [(Bool, Expr)])
applyRulesWithBranching (False, Imply f1 f2) = (False, [(True, f1), (False, f2)])
applyRulesWithBranching (True, And f1 f2) = (False, [(True, f1), (True, f2)])
applyRulesWithBranching (False, Or f1 f2) = (False, [(False, f1), (False, f2)])
applyRulesWithBranching (v, f) = (True, applyRules (v, f))

-- Função para construir a árvore de prova a partir de uma lista de fórmulas
buildProofTree :: [(Bool, Expr)] -> Tree (Bool, Expr)
buildProofTree [] = Node (True, Atom ' ') []
buildProofTree ((v, f):fs) =
  let (newBranch, newFormulas) = applyRulesWithBranching (v, f)
      newFormulas' = newFormulas ++ fs
      branches = if newBranch
                 then map (\x -> nub (x:fs)) newFormulas'
                 else [newFormulas']
  in Node (v, f) (map buildProofTree (filter (not . null) branches))


-- Função principal para construir a árvore de prova a partir de uma fórmula
proofTree :: Expr -> Tree (Bool, Expr)
proofTree formula = buildProofTree [(True, Not formula)]


printTreeAsLists :: Tree (Bool, Expr) -> [[(Bool, Expr)]]
printTreeAsLists (Node (v, f) []) = [[(v, f)]]
printTreeAsLists (Node (v, f) branches) = 
  map ((v, f) :) $ concatMap printTreeAsLists branches

main :: IO ()
main = do
    putStrLn "Insira uma fórmula lógica: "
    formula <- getLine
    let formulaWithoutInconveniences = removeInconveniences formula
    let parsedFormula = parse exprParser "" formulaWithoutInconveniences
    
    case parsedFormula of
        Left _ -> putStrLn "Fórmula inválida"
        Right expr -> do
            let tree = proofTree expr
            putStrLn $ drawTree $ fmap showNode tree
            let treeAsLists = printTreeAsLists tree
            mapM_ print treeAsLists

    
  where
    showNode (v, f) = (if v then "V: " else "F: ") ++ show f
