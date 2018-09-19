module Main where


import System.Environment
import Data.Char

main :: IO ()
main 
  = do args <- getArgs
       latex <- readFile "TMP.tex"
       let haskell = extractHaskell (modname args) $ lines latex
       writeFile "TMP.hs" $ unlines haskell
      
modname [] = "TMP"
modname (nm:_) = nm       
       
extractHaskell nm lline
  = ( "module "++nm++" where\n" ) :  extract lline
    
           
extract [] = []
extract (ln:lns)
 | trim ln == "\\begin{haskell}"  =  keep lns
 | otherwise                      =  extract lns
 
 
keep [] = []
keep (ln:lns)
 | trim ln == "\\end{haskell}"  =  "" : extract lns
 | otherwise                    =  ln : keep lns

trim = reverse . ltrim . reverse . ltrim

ltrim "" = ""
ltrim str@(c:cs)
  | isSpace c  =  ltrim cs
  | otherwise  =  str