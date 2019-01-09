module Main where
import Prelude hiding (and, or, succ, pred)

import Lambda.Untyped.Lam
import Lambda.Untyped.Reduce

main :: IO ()
main = do
    putStrLn ident
    putStrLn true
    putStrLn false
    putStrLn $ to $ redterm (app2 plus two two)
    putStrLn $ to $ redterm (app2 times two two)
    putStrLn $ to $ redterm (app2 power two two)
    let three = redterm $ app2 plus one two
        four  = redterm $ app2 times two two
        five  = redterm $ app2 plus two three
    putStrLn $ to five
    putStrLn $ to $ redterm (app2 eq five four)
    putStrLn $ to $ redterm (app2 eq five five)
    putStrLn succ
