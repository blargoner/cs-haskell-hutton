-- exercise 1
init' :: [a] -> [a]
init' [] = []
init' xs = init xs

back :: Int -> IO ()
back n | n >= 0 = do putStr (concat (replicate n "\ESC[1D \ESC[1D"))

readLineBuf :: String -> IO String
readLineBuf xs = do x <- getChar
                    case x of
                        '\n' -> return xs
                        '\DEL' -> do back 3
                                     readLineBuf (init' xs)
                        _ -> readLineBuf (xs ++ [x])

readLine :: IO String
readLine = readLineBuf ""
