import System.IO hiding (hPutStr, hPutStrLn, hGetLine, hGetContents, putStrLn)

import qualified Data.Text as T
import System.IO( Handle, FilePath, IOMode( ReadMode ), 
                  openFile, hGetLine, hPutStr, hClose, hIsEOF, stderr );
import Data.Char;
import Control.Monad( when );
lowerString str = [ toLower loweredString | loweredString <- str];
toUpperCase str = map toUpper str;
dumpFile :: Handle -> FilePath -> Integer -> IO ()
dumpFile handle filename lineNumber   = do      -- show file contents line by line
    end <- hIsEOF handle
    when ( not end ) $ do
        line <- hGetLine handle
        let line1 line ch1 = if ("'"++ch1++"'")==show(head(line::[Char])) then line else  "net";
        putStrLn $ filename ++ ":" ++ show lineNumber ++ ": " ++"  " ++line
        dumpFile (handle) (filename) ( lineNumber + 1) ;

dumpFile1 :: Handle -> FilePath -> Integer ->Integer-> IO ()
dumpFile1 handle filename lineNumber k   = do      -- show file contents line by line
    end <- hIsEOF handle
    if (end) then putStrLn("Практик = "++show(k)) else putStr"";
    when ( not end ) $ do
        line <- hGetLine handle
        let line1 line = if (dropWhile (/=' ') line)==" (практ.)" then do{dumpFile1 (handle) (filename) ( lineNumber + 1) (k+1); putStr("")} else do{dumpFile1 (handle) (filename) ( lineNumber + 1) (k); putStr"";}
        line1 (line);

dumpFile2 :: Handle -> FilePath -> Integer ->Integer-> IO ()
dumpFile2 handle filename lineNumber k   = do      -- show file contents line by line
    end <- hIsEOF handle
    if (end) then putStrLn("Лекций = "++show(k)) else putStr"";
    when ( not end ) $ do
        line <- hGetLine handle
        let line1 line = if (dropWhile (/=' ') line)==" (лекц.)" then do{dumpFile2 (handle) (filename) ( lineNumber + 1) (k+1); putStr""} else do{dumpFile2 (handle) (filename) ( lineNumber + 1) (k); putStr"";}
        line1 (line);


dumpFile3 :: Handle -> FilePath -> Integer ->Integer-> IO ()
dumpFile3 handle filename lineNumber k   = do      -- show file contents line by line
    end <- hIsEOF handle
    if (end) then putStrLn("Лабораторных = "++show(k)) else putStr"";
    when ( not end ) $ do
        line <- hGetLine handle
        let line1 line = if (dropWhile (/=' ') line)==" (лаб.)" then do{dumpFile3 (handle) (filename) ( lineNumber + 1) (k+1); putStr("")} else do{dumpFile3 (handle) (filename) ( lineNumber + 1) (k); putStr"";}
        line1 (line);

main = do{
    hPutStr stderr "Введите имя файла: ";
    filename <- getLine;
    handle <- openFile filename ReadMode;     

    dumpFile handle filename 1;
    hClose handle;

    handle <- openFile filename ReadMode; 
    dumpFile1 handle filename 1 0;
    hClose handle;

    handle <- openFile filename ReadMode; 
    dumpFile2 handle filename 1 0;
    hClose handle;

    handle <- openFile filename ReadMode; 
    dumpFile3 handle filename 1 0;
    hClose handle;
}