import Control.Monad

dictionary = [ "hi", "my", "name", "is", "am", "munition", "ammunition"]

tokenize :: String -> [[String]]
tokenize "" = [[]]
tokenize str = do
    (prefix, suffix) <- [(take i str, drop i str) | i <- [1..length str]]
    guard $ elem prefix dictionary
    others <- tokenize suffix
    return $ prefix : others

