import System.IO

-- Main Routine
main :: IO ()
main = do
    putStrLn "Welcome to IO monad! Enter the username to login, or 'quit' to exit the program. "
    -- always flush the buffer! (use the hFLush stdout)
    hFlush stdout
    login <- getLine
    if login == "myconejo" then do
        putStrLn ("Welcome! " ++ login)
        putStrLn "Enter the password: "
        hFlush stdout
        pw <- getLine
        if pw == "1234" then do
            putStrLn ("Login successful, username: " ++ login)
            hFlush stdout
            shell
        else do
            putStrLn ("Invalid Password")
            hFlush stdout
            main
    else 
        if (login == "quit") || (login == "q") || (login == "exit") then do
            return()
        else do 
            putStrLn ("Unregistered ID")
            hFlush stdout
            main

-- Shell
shell :: IO()  
shell = do
    putStr "haskell> "
    hFlush stdout
    command <- getLine
    if (command == "quit") || (command == "q") || (command == "exit") then do
        putStrLn "haskell> exiting..."
        return ()
    else 
        if (command == "cal") || (command == "calculator") then do
            putStrLn "haskell> loading calculator"
            hFlush stdout
            calc
            shell
        else 
            if command == "logout" then do 
                main
            else do 
                putStrLn $ "haskell> " ++ command ++ ": unknown command"
                hFlush stdout
                shell        

-- Calculator
calc :: IO ()
calc = do
    putStr "Enter the first argument: "
    hFlush stdout
    x1 <- getLine
    if (x1 == "quit") || (x1 == "q") || (x1 == "exit") then do return()
    else do
        putStr("Enter the operand: ")
        hFlush stdout
        op <- getLine
        if not(elem op ["+", "-", "*", "/"]) then do
            putStrLn("Invalid operator. Exiting program")
            return()
        else do 
            putStr("Enter the second argument: ")
            hFlush stdout
            x2 <- getLine
            if op == "+" then do
                putStr "Result: " 
                putStrLn (show((read x1 :: Integer) + (read x2 :: Integer)))
                return()
            else 
                if op == "-" then do
                    putStr "Result: "
                    putStrLn (show((read x1 :: Integer) - (read x2 :: Integer)))
                    return()
                else 
                    if op == "*" then do
                    putStr "Result: "
                    putStrLn (show((read x1 :: Integer) * (read x2 :: Integer)))
                    return()
                    else do 
                        putStr "Result: "
                        putStrLn (printSafe(Just(read x1 :: Integer) >>= (\a -> Just(read x2 :: Integer) >>= (\b -> safediv a b))))
                        return()

-- Safe division
safediv :: Integer -> (Integer -> Maybe Integer)
safediv x y 
    | y == 0 = Nothing
    | otherwise = Just(div x y)

-- Print results
printSafe :: Maybe Integer -> String
printSafe mx = 
    case mx of 
        Nothing -> "divide by 0 Error"
        Just n -> show(n)
