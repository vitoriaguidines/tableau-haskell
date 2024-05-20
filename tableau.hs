import ExpParser (Expr(..), exprParser)
import Data.List (elemIndex)
import Text.Parsec (parse)
import Data.Tree (Tree(..), drawTree)


-- Função para remover espaços e hífens de uma string
removeInconveniences :: String -> String
removeInconveniences " " = ""
removeInconveniences a = [e | e<-a, e /= ' ', e /= '-']

proofTree :: Expr -> Tree Expr
proofTree formula = case formula of
    Atom _ -> Node formula []
    Not f -> Node formula [proofTree f]
    And f1 f2 -> Node formula [proofTree f1, proofTree f2]
    Or f1 f2 -> Node formula [proofTree f1, proofTree f2]
    Imply f1 f2 -> Node formula [proofTree f1, proofTree f2]


main :: IO ()
main = do
    --b>(a&(b|a)) Inválida
    --a>(a>(b>a))
    --(p|(q&r))>((p|q)&(p|r))
    --(a&b)>a
    --(a&b)>b
    --a>(a|b)
    --b>(a|b)
    
    putStrLn "Insira uma fórmula lógica: "
    formula <- getLine

    let formulaWithoutInconveniences = removeInconveniences formula
    let formattedFormula = parse exprParser "" formulaWithoutInconveniences
    
    let tree = case formattedFormula of
                    Left _ -> error "Invalid formula"
                    Right expr -> proofTree expr
    putStrLn $ drawTree $ fmap show tree
