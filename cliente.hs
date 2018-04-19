import System.IO
import Data.List.Split

type Codigo = Integer
type Nome = String
type Cidade = String
type Idade = Integer
type Sexo = Char
type Cliente = (Codigo, Nome, Cidade, Idade, Sexo)

cli_arquivo = "cliente.db"

cli = (5,"junior", "floripa", 12, 'M')

string_conv :: Cliente -> String
string_conv (x, y, z, w, k) = "" ++ show x ++ "," ++ y ++ "," ++ z ++ "," ++ show w ++ "," ++ show k ++ "\n"

cli_add = do
  appendFile cli_arquivo (string_conv cli)


--retchar :: Pessoa -> String
--retchar (x, y) = "" ++ x++ show y

--ret_pessoa :: [String] -> Pessoa
--ret_pessoa x = (get_pos x 0, get_pos x 1)

--get_pos :: [String] -> Int -> String
--get_pos [] _ = []
--get_pos (x:xs) i | i == 0 = x
--                 | otherwise = get_pos xs (i-1)


main:: IO ()
main = do
  handle <- openFile "pessoa.txt" ReadMode
  contents <- hGetLine handle
  contents <- hGetLine handle
  putStrLn contents
  putStrLn (show(splitOn "," contents))
