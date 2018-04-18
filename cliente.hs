import System.IO
import Data.List.Split

type Nome = String
type Idade = Int
type Pessoa = (Nome, Idade)

retp :: Pessoa
retp = ("junior", 12)

retchar :: Pessoa -> String
retchar (x, y) = "" ++ x++ show y

rettupla :: String -> Pessoa
re
salva = do
  let string = retchar(retp)
  writeFile "pessoa.txt" string
  putStr "ecreveu"

ler = do
  contents <- readFile "pessoa.txt"
  putStr contents

main:: IO ()
main = do
  handle <- openFile "cliente.db" ReadMode
  contents <- hGetLine handle
  putStr contents
  contents <- hGetLine handle
  putStr contents
