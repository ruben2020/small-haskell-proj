
import System.IO
import System.Exit
import System.Environment(getArgs)
import Data.List
import Data.Char

-- General functions

genBigPuzzle :: [Int] -> [[Int]]
genBigPuzzle [] = []
genBigPuzzle lst
    | ((head lst) == 0)   = [[0..9]] ++ (genBigPuzzle (tail lst))
    | otherwise           = [  [head lst]  ] ++ (genBigPuzzle (tail lst))

genSmallPuzzle :: [[Int]] -> [Int]
genSmallPuzzle [] = []
genSmallPuzzle lst = [head (head lst)] ++ genSmallPuzzle (tail lst)

accessCell row col = (row*9) + col
rowAccess accnum =  accnum `quot` 9
colAccess accnum =  (accnum - (9*(rowAccess accnum)))
startnum n = ((n `quot` 3) * 3)
endnum   n = (startnum n) + 2

reduceSolved (0:x:[]) = [x]
reduceSolved xs = xs

countPossib [ ] = 0
countPossib pzl = (length (tail (head pzl))) + (countPossib (tail pzl))

countUnknown pzl = countUnknown' pzl 0
countUnknown' [] n = n
countUnknown' pzl n
    | ((head (head pzl)) == 0) = countUnknown' (tail pzl) (n+1)
    | otherwise                = countUnknown' (tail pzl) n

countMatches lst x = countMatches' lst x 0
countMatches' [] _ n = n
countMatches' (x:xs) y n
    | (x == y)  = countMatches' xs y (n+1)
    | otherwise = countMatches' xs y n

onlyUnique lst = onlyUnique' lst (nub lst)
onlyUnique' lst [] = []
onlyUnique' lst (x:xs)
    | ((countMatches lst x) > 1) = onlyUnique' lst xs
    | otherwise                  = [x] ++ onlyUnique' lst xs

transRowCol :: [[Int]] -> [[Int]]
transRowCol pzl = transRowCol' pzl 0
transRowCol' pzl 81 = []
transRowCol' pzl n  = [( pzl !! (accessCell (colAccess n) (rowAccess n)) )] ++ transRowCol' pzl (n+1)

trans3x3Row :: [[Int]] -> [[Int]]
trans3x3Row pzl = trans3x3Row' pzl [0..2] [0..2]
trans3x3Row' pzl [    ]   _    = []
trans3x3Row' pzl (x:xs) [    ] = trans3x3Row' pzl xs [0..2]
trans3x3Row' pzl (x:xs) (y:ys) = trans3x3Row'' pzl [(x*3)..((x*3)+2)] [(y*3)..((y*3)+2)] (y*3) ++ trans3x3Row' pzl (x:xs) ys
trans3x3Row'' pzl [    ]   _    _ = []
trans3x3Row'' pzl (x:xs) [    ] m = trans3x3Row'' pzl (xs) [m..(m+2)] m
trans3x3Row'' pzl (x:xs) (y:ys) m = [(pzl !! (accessCell x y))] ++ trans3x3Row'' pzl (x:xs) (ys) m


-- Rule 1

rule1Row  pzl row = nub $ filter (>0) $ rule1Row' pzl row 0 8
rule1Row' pzl row n maxx
    | (n > maxx)   = []
    | otherwise = (head (pzl !! (accessCell row n))) : (rule1Row' pzl row (n + 1) maxx)

checkRule1' pzl = checkRule1'' pzl pzl 0
checkRule1'' _ [] _ = []
checkRule1'' ppzl pzl n
    | ((head (head pzl)) == 0) = [(reduceSolved . filter f) (head pzl)] ++ (checkRule1'' ppzl (tail pzl) (n+1))
    | otherwise                = [head pzl] ++ (checkRule1'' ppzl (tail pzl) (n+1))
             where
                f x = x `notElem` (rule1Row ppzl (rowAccess n))

checkRule1 pzl = do
    n    <- return $ countUnknown pzl
    pzl1 <- return $ trans3x3Row $ checkRule1' $ trans3x3Row $ transRowCol $ checkRule1' $ transRowCol $ checkRule1' pzl
    m    <- return $ countUnknown pzl1
    putStrLn $ "checkRule1, unknowns = " ++ (show m)
    if (n == m)
    then
        do
        return $ pzl1
    else
        do
        checkRule1 pzl1

-- Rule 2

rule2RowReduce  pzl row lst              = rule2RowReduce' pzl [0..8] [0..8] lst row
rule2RowReduce' pzl [] _ _ _ = []
rule2RowReduce' pzl (x:xs) [] lst row    = rule2RowReduce' pzl   xs   [0..8] lst row
rule2RowReduce' pzl (x:xs) (y:ys) [] row = [(pzl !! (accessCell x y))]  ++ rule2RowReduce' pzl (x:xs) ys [] row
rule2RowReduce' pzl (x:xs) (y:ys) (z:zs) row
    | (x == row)&&(z `elem` (tail (pzl !! (accessCell x y)))) = [[z]] ++ rule2RowReduce' pzl (x:xs) ys [] row
    | otherwise                                               = [(pzl !! (accessCell x y))]  ++ rule2RowReduce' pzl (x:xs) ys (z:zs) row

rule2Row  pzl = rule2Row' pzl [0..8] [0..8] []
rule2Row' pzl [    ]   _    _   = pzl
rule2Row' pzl (x:xs) [    ] [ ] = rule2Row' pzl xs [0..8] []
rule2Row' pzl (x:xs) [    ] lst
    | (uniqlst == [])  = rule2Row' pzl xs [0..8] []
    | otherwise        = rule2RowReduce pzl x uniqlst
               where
                 uniqlst = onlyUnique lst
rule2Row' pzl (x:xs) (y:ys) lst = rule2Row' pzl (x:xs) ys (lst ++ tail (pzl !! (accessCell x y) ))

checkRule2 pzl = do
    n    <- return $ countUnknown pzl
    pzl1 <- return $ rule2Row pzl
    m    <- return $ countUnknown pzl1
    putStrLn $ "checkRule2 rows, unknowns = " ++ (show m)
    if (m == n)
    then
        do
        pzl2 <- return $ transRowCol $ rule2Row $ transRowCol pzl1
        k    <- return $ countUnknown pzl2
        putStrLn $ "checkRule2 cols, unknowns = " ++ (show k)
        if (k == n)
        then
            do
            pzl3 <- return $ trans3x3Row $ rule2Row $ trans3x3Row pzl2
            p <- return $ countUnknown pzl3
            putStrLn $ "checkRule2 3x3s, unknowns = " ++ (show p)
            return $ pzl3
        else
            do
            return $ pzl2
    else
        do
        return $ pzl1

-- Rule 3

singleNumIn3sGroup pzl row = onlyUnique $ singleNumIn3sGroup' pzl row [0..8] []
singleNumIn3sGroup'  _   _    []    _  = []
singleNumIn3sGroup' pzl row (x:xs) lst
    | ((x+1) `mod` 3 == 0) = (nub addTailToLst) ++ singleNumIn3sGroup' pzl row xs []
    | otherwise            = singleNumIn3sGroup' pzl row xs addTailToLst
          where
              addTailToLst = lst ++ (tail $ pzl !! (accessCell row x))

findColWithElem  pzl row  elm = findColWithElem' pzl row [0..8] elm
-- should not reach xs == []
findColWithElem' pzl row (x:xs) elm
    | (elm `elem` (cell pzl row x)) = x
    | otherwise                 = findColWithElem' pzl row xs elm
          where
             cell pzl a b
                 | (length (pzl !! (accessCell a b))) > 1 = tail (pzl !! (accessCell a b))
                 | otherwise                              = [0]

reduceElements elm (x:[]) = [x]
reduceElements elm (x:xs) = [x] ++ (filter (/= elm) xs)

removeFromOthers pzl row col elm = removeFromOthers' pzl row col elm 0
removeFromOthers'  _   _   _   _  81 = []
removeFromOthers' pzl row col elm n
    | (r == row)                         = [(pzl !! n)] ++ nxt
    | ((checkx r row) && (checkx c col)) = [reduceElements elm (pzl !! n)] ++ nxt
    | otherwise                          = [(pzl !! n)] ++ nxt
          where
             r = rowAccess n
             c = colAccess n
             checkx x y = (x >= startnum y) && (x <= endnum y)
             nxt = removeFromOthers' pzl row col elm (n+1)

head'' [] = 19
head'' (x:xs) = x

chkChanged p1 p2 xs
   | (countPossib p1) == (countPossib p2)  = xs
   | otherwise                             = []

checkRule3' pzl = checkRule3'' pzl [0..8]
checkRule3'' pzl [] = pzl
checkRule3'' pzl (row:xs)
   | elme /= 19      = checkRule3'' rfo (chkChanged rfo pzl xs)
   | otherwise       = checkRule3'' pzl xs
         where
            elme = head'' $ singleNumIn3sGroup pzl row
            rfo  = (removeFromOthers pzl row (findColWithElem pzl row elme) elme)

checkRule3 pzl = do
    n    <- return $ countPossib pzl
    pzl1 <- return $ checkRule3' pzl
    m    <- return $ countPossib pzl1
    putStrLn $ "checkRule3 rows, possib = " ++ (show n) ++ " --> " ++ (show m)
    if (m == n)
    then
        do
        pzl2 <- return $ transRowCol $ checkRule3' $ transRowCol pzl1
        k    <- return $ countPossib pzl2
        putStrLn $ "checkRule3 cols, possib = " ++ (show m) ++ " --> " ++ (show k)
        if (k == n)
        then
            do
            pzl3 <- return $ trans3x3Row $ checkRule3' $ trans3x3Row pzl2
            p <- return $ countPossib pzl3
            putStrLn $ "checkRule3 3x3s, possib = " ++ (show k) ++ " --> " ++ (show p)
            return $ pzl3
        else
            do
            return $ pzl2
    else
        do
        return $ pzl1

-- I/O and main

solve pzl = do
    n <- return $ countUnknown pzl
    putStrLn $ "Solving ... " ++ (show n) ++ " unknowns remaining"
    pzl1 <- checkRule1 pzl
    pzl2 <- checkRule2 pzl1
    m <- return $ countUnknown pzl2
    if (m /= 0)
        then
        do
        if (m == n)
        then
            do
            k1 <- return $ countPossib pzl2
            pzl3 <- checkRule3 pzl2
            k2 <- return $ countPossib pzl3
            if (k1 == k2)
                then
                    do
                    return $ pzl3
                else
                    do
                    solve pzl3
        else
            do
            solve pzl2
    else
        do
        return $ pzl2

prettyprint chs = prettyprint' chs 0
prettyprint' [] _ = do
    putStrLn ""
prettyprint' chs n = do
    putStr $ [intToDigit (head chs)]
    m <- return $ n + 1
    if (m `mod` 3 == 0)
    then
        do
        putStr " "
    else
        do
        return ()
    if (m `mod` 9 == 0)
    then
        do
        putStrLn ""
    else
        do
        return ()
    if (m `mod` 27 == 0)
    then
        do
        putStrLn ""
        prettyprint' (tail chs) m
    else
        do
        prettyprint' (tail chs) m

checkSolved 0 = "Success!"
checkSolved _ = "Failed!"

main = do
    args <- getArgs
    puzzle <- return $ map (digitToInt) (head args)
    if ((length puzzle) == 81)
    then
        do
        a <- return $ genBigPuzzle $ puzzle
        prettyprint $ genSmallPuzzle a
        b <- solve a
        n <- return $ countUnknown b
        putStrLn ""
        prettyprint $ genSmallPuzzle b
        putStrLn $ checkSolved n
        putStrLn ""
        return ()
    else
        do
        putStrLn "Number of integers entered was not 81"
        return ()


