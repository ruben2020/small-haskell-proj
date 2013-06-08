import System.IO  
import System.Environment 

converteol [] = []
converteol (x:[])
   | (x == '\r')   = ['\n']
   | otherwise     = [x]
converteol (x:y:xs)
   | ((x == '\r')&&(y == '\n'))       = ['\n'] ++ (converteol xs) -- Windows EOL to Linux EOL
   | ((x == '\r')&&(not (y == '\n'))) = ['\n'] ++ (converteol ([y] ++ xs)) -- Mac EOL to Linux EOL
   | otherwise                        = [x]    ++ (converteol ([y] ++ xs))

countlines_linux = countlines . converteol
countsloc_linux  = countsloc . converteol

countlines t = countlines' t 0
countlines' [] n = n
countlines' (x:[]) n = n + 1
countlines' (x:xs) n
   | (x == '\n') = countlines' xs (n + 1)
   | otherwise   = countlines' xs n

countsloc t  = countsloc' t 0 1
countsloc' [] loc _ = loc
countsloc' (x:[]) loc notcnt
   | (x == '\n')                      = countsloc' [] loc 1
   | (not ((x == ' ')||(x == '\t')))  = countsloc' [] (loc + notcnt) 0
   | otherwise                        = countsloc' [] loc notcnt
countsloc' (x:y:xs) loc notcnt
   | ((x == '/')&&(y == '/'))         = cppcomment xs loc notcnt
   | ((x == '/')&&(y == '*'))         = ccomment xs loc notcnt
   | (x == '\n')                      = countsloc' ([y] ++ xs) loc 1
   | (not ((x == ' ')||(x == '\t')))  = countsloc' ([y] ++ xs) (loc + notcnt) 0
   | otherwise                        = countsloc' ([y] ++ xs) loc notcnt

cppcomment [] loc _ = loc
cppcomment (x:xs) loc notcnt
   | (x == '\n')      = countsloc' xs loc 1
   | otherwise        = cppcomment xs loc notcnt

ccomment [] loc _ = loc
ccomment (x:[]) loc notcnt = ccomment [] loc notcnt
ccomment (x:y:xs) loc notcnt
   | ((x == '*')&&(y == '/'))   = countsloc' xs loc notcnt
   | (x == '\n')                = ccomment ([y] ++ xs) loc 1
   | otherwise                  = ccomment ([y] ++ xs) loc notcnt

getFNFromArgs [] = ""
getFNFromArgs (x:xs) = x

process_contents "" = do
    putStrLn "Provide filename as argument"

process_contents fn = do
    handle <- openBinaryFile fn ReadMode
    contents <- hGetContents handle
    putStrLn $ fn ++ ";" ++ (show (countsloc_linux contents)) ++ ";" ++ (show (countlines_linux contents))
    hClose handle                          

main = do
    args <- getArgs
    process_contents (getFNFromArgs args)

