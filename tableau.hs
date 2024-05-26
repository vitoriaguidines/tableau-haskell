import Data.List (nub)
import Data.Tree (Tree (..), drawTree, flatten)
import ExpParser (Expr (..), exprParser)
import Text.Parsec (parse)

-- Função para remover espaços e hífens de uma string
removeInconveniences :: String -> String
removeInconveniences = filter (`notElem` [' ', '-'])

-- Função para verificar se uma expressão é composta
isCompound :: Expr -> Bool
isCompound (Atom _) = False
isCompound _ = True

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

-- Função para aplicar regras de prova com ramificação e verificar se é composta
-- applyRulesWithBranching :: (Bool, Expr) -> (Bool, [(Bool, Expr)])
-- applyRulesWithBranching (v, f)
--   | isCompound f = (True, applyRules (v, f)) -- verifica se a função f é composta, e se for, aplica as regras de v e f
--   | otherwise = case f of -- se não, verifica o tipo de f usando correspondência
--       Imply f1 f2 -> (False, [(True, f1), (False, f2)])
--       And f1 f2 -> (True, [(True, f1), (True, f2)])
--       Or f1 f2 -> (False, [(False, f1), (False, f2)])
--       _ -> (True, [(v, f)])

-- ta imprimindo um monte de coisa eternamente e não sai da recursão

-- Função para construir a árvore de prova a partir de uma lista de fórmulas
buildProofTree :: [(Bool, Expr)] -> Tree (Bool, Expr)
buildProofTree [] = Node (True, Atom ' ') []
buildProofTree ((v, f) : fs) =
  let (newBranch, newFormulas) = applyRulesWithBranching (v, f)
      newFormulas' = newFormulas ++ fs
      branches =
        if newBranch
          then map (\x -> nub (x : fs)) newFormulas'
          else [newFormulas']
   in Node (v, f) (map buildProofTree (filter (not . null) branches))

-- Função principal para construir a árvore de prova a partir de uma fórmula
proofTree :: Expr -> Tree (Bool, Expr)
proofTree formula = buildProofTree [(True, Not formula)]

-- Função para transformar cada caminho da árvore em uma lista
branchTreeAsLists :: Tree (Bool, Expr) -> [[(Bool, Expr)]]
branchTreeAsLists (Node (v, f) []) = [[(v, f)]]
branchTreeAsLists (Node (v, f) branches) = map ((v, f) :) $ concatMap branchTreeAsLists branches

-- Função para verificar se uma fórmula é válida
checkValidate :: [[(Bool, Expr)]] -> IO ()
checkValidate branches = do
  let numberedBranches = zip [1 ..] branches
  allClosed <- mapM printBranch numberedBranches
  if and allClosed
    then putStrLn "A função é válida"
    else putStrLn "A função não é válida"
  where
    printBranch (branchNum, branch) = do
      putStrLn $ "Branch " ++ show branchNum ++ ":"
      let atoms = [(if v then "V: " else "F: ") ++ show f | (v, Atom f) <- branch]
      putStrLn $ "Atoms: " ++ show atoms
      let closed = checkContradiction atoms
      if closed
        then putStrLn "The branch is closed"
        else putStrLn "The branch is open"
      putStrLn ""
      return closed

-- Função para verificar se uma lista de átomos contém uma contradição
checkContradiction :: [String] -> Bool
checkContradiction atoms = any checkAtom ['a' .. 'z']
  where
    checkAtom a = elem ("V: '" ++ [a] ++ "'") atoms && elem ("F: '" ++ [a] ++ "'") atoms

main :: IO ()
main = do
  -- b>(a&(b|a)) Inválida
  -- a>(a>(b>a)) Válida
  -- (p|(q&r))>((p|q)&(p|r)) Válida
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
