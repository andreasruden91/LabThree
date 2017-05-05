{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}

import AATree

--------------------------------------------------------------------------------

main :: IO ()
main = do
    contents <- getContents

    let tree = foldl (flip insert) emptyTree (words contents)
    let n = size tree
    let h = height tree
    let h' = ceiling $ (logBase 2 (fromIntegral (n) + 1)) - 1

    putStrLn $ "Size: " ++ show n
    putStrLn $ "Height: " ++ show h
    putStrLn $ "Optimal Height: " ++ show h'
    putStrLn $ "Height / Optimal Height: " ++ show (fromIntegral (h) / fromIntegral(h'))
    putStrLn $ "checkTree: " ++ show (checkTree tree)
    putStrLn $ "First 20 words: " ++ unwords (take 20 (inorder tree))

--------------------------------------------------------------------------------

