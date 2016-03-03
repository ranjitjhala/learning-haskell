

myPrint:: String -> IO ()
myPrint = putStrLn

anotherPrint :: Maybe String -> IO ()
anotherPrint Nothing = return ()
anotherPrint (Just s) = putStrLn s

main::IO ()
main =
      myPrint "Hello World\n"
              
