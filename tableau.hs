import ExpParser (Expr(..), exprParser)
import Text.Parsec (parse)
import Data.Tree (Tree(..), drawTree)
import Data.List (nub)


-- Função para remover espaços e hífens de uma string
removeInconveniences :: String -> String
removeInconveniences = filter (`notElem` [' ', '-'])


-- Função para aplicar regras de prova e construir a árvore
applyRulesWithBranching :: (Bool, Expr) -> [(Bool, Expr)]
applyRulesWithBranching (False, And f1 f2) = [(False, f1), (False, f2)]
applyRulesWithBranching (True, Or f1 f2) = [(True, f1), (True, f2)]
applyRulesWithBranching (True, Imply f1 f2) = [(False, f1), (True, f2)]


-- Função para aplicar regras de prova sem ramificação
applyRulesWithNoBranching :: (Bool, Expr) -> (Bool, [(Bool, Expr)])
applyRulesWithNoBranching (True, Atom _) = (False, [])
applyRulesWithNoBranching (False, Atom _) = (False, [])
applyRulesWithNoBranching (v, Not f) = (False, [(not v, f)])
applyRulesWithNoBranching (False, Imply f1 f2) = (False, [(True, f1), (False, f2)])
applyRulesWithNoBranching (True, And f1 f2) = (False, [(True, f1), (True, f2)])
applyRulesWithNoBranching (False, Or f1 f2) = (False, [(False, f1), (False, f2)])
applyRulesWithNoBranching (v, f) = (True, applyRulesWithBranching (v, f))


-- Função para construir a árvore de prova a partir de uma lista de fórmulas
buildProofTree :: [(Bool, Expr)] -> Tree (Bool, Expr)
buildProofTree [] = Node (True, Atom ' ') []
buildProofTree ((v, f):fs) =
  let (newBranch, newFormulas) = applyRulesWithNoBranching (v, f)
      newFormulas' = newFormulas ++ fs
      branches = if newBranch
                 then [nub (x:fs) | x <- newFormulas]
                 else [newFormulas']
  in Node (v, f) (map buildProofTree (filter (not . null) branches))


-- Função principal para construir a árvore de prova a partir de uma fórmula
proofTree :: Expr -> Tree (Bool, Expr)
proofTree formula = buildProofTree [(True, Not formula)]


-- Função para transformar cada caminho da árvore em uma lista
branchTreeAsLists :: Tree (Bool, Expr) -> [[(Bool, Expr)]]
branchTreeAsLists (Node (v, f) []) = [[(v, f)]]
branchTreeAsLists (Node (v, f) branches) = concatMap (map ((v, f) :) . branchTreeAsLists) branches


-- Função para verificar se uma fórmula é válida
checkValidate :: [[(Bool, Expr)]] -> IO ()
checkValidate branches = do
  let numberedBranches = zip [1..] branches
  allClosed <- mapM printBranch numberedBranches
  if and allClosed
    then putStrLn "A fórmula é válida"
    else putStrLn "A fórmula é inválida"
  where
    printBranch (branchNum, branch) = do
      putStrLn $ "Branch " ++ show branchNum ++ ":"
      let atoms = [(v, f) | (v, Atom f) <- branch]
      putStrLn $ "Atoms: " ++ show atoms
      let closed = checkContradiction atoms
      if closed
        then putStrLn "O ramo está fechado"
        else putStrLn "O ramo está aberto"
      putStrLn ""
      return closed
  

-- Função para verificar se uma lista de átomos contém uma contradição
checkContradiction :: [(Bool, Char)] -> Bool
checkContradiction atoms = any checkAtom ['a' .. 'z']
  where
    checkAtom a = elem (True, a) atoms && elem (False, a) atoms

main :: IO ()
main = do
  -- Exemplos:
  -- b>(a&(b|a)) Inválida
  -- a>(a>(b>a)) Válida
  -- (p|(q&r))>((p|q)&(p|r)) Válida
  -- ((p|q)&(p|r))>(p|(q&r)) Válida
    putStrLn "Insira uma fórmula lógica: "
    formula <- getLine
    let formulaWithoutInconveniences = removeInconveniences formula
    let parsedFormula = parse exprParser "" formulaWithoutInconveniences

    case parsedFormula of
        Left _ -> putStrLn "Fórmula inválida"
        Right expr -> do
            let tree = proofTree expr
            putStrLn $ drawTree $ fmap showNode tree

            let treeAsLists = branchTreeAsLists tree
            checkValidate treeAsLists
  where
    showNode (v, f) = (if v then "V: " else "F: ") ++ show f
