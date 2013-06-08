
testln = "for (int i; i<0; i++) {test();} /*first line*/\n /*yo*/ test2(); //this is one comment\n//test3();\na = b/2;\n/* /// the end *** */\n/***//***///hello\n////woohoo////\nprintf(\"\\\"What kind of comment //// /****/ is this?\\\"\");\n//test4();\n//last commented line\n/**/;"

converteol [] = []
converteol (x:[])
   | (x == '\r')   = ['\n']
   | otherwise     = [x]
converteol (x:y:xs)
   | ((x == '\r')&&(y == '\n'))       = ['\n'] ++ (converteol xs) -- Windows EOL to Linux EOL
   | ((x == '\r')&&(not (y == '\n'))) = ['\n'] ++ (converteol ([y] ++ xs)) -- Mac EOL to Linux EOL
   | otherwise                        = [x]    ++ (converteol ([y] ++ xs))

ftop [] = []
ftop (x:[]) = [x]
ftop (x:y:xs)
    | ((x == '/') && (y == '/'))   = "/*" ++ convcomment xs     -- start converting // to /* */
    | ((x == '/') && (y == '*'))   = "/*" ++ ccomment xs        -- to ignore // within /* // */
    |   x == '"'                   = "\"" ++ quotez ([y] ++ xs) -- to ignore // within " // "
    | otherwise                    = [x]  ++ ftop ([y] ++ xs)

convcomment [] = "*/"   -- what if last line with // is not terminated with newline?
convcomment (x:xs)
    | x == '\n'                    = "*/\n" ++ ftop xs
    | otherwise                    = [x]    ++ convcomment xs

ccomment [] = []
ccomment (x:[]) = [x]
ccomment (x:y:xs)
    | ((x == '*') && (y == '/'))   = "*/" ++ ftop xs
    | otherwise                    = [x] ++ ccomment ([y] ++ xs)

quotez [] = []
quotez (x:[]) = [x]
quotez (x:y:xs)
    | ((x == '\\') && (y == '"'))  = "\\\"" ++ quotez xs  -- to consider the case of "\"", escape of "
    | x == '"'                     = "\""   ++ ftop ([y] ++ xs)
    | otherwise                    = [x] ++ quotez ([y] ++ xs)

main = do
        putStr $ "\n------------Input------------------\n"
        putStr $ testln
        putStr $ "\n------------Output------------------\n"
        putStr $ ftop $ converteol (testln)
        putStr $ "\n------------The End-----------------\n"
 
