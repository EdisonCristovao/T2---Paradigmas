import System.Process
import Cliente
import Produto
import Venda

main :: IO ()
main = do
  system "clear"
  putStrLn "-----------------Gerenciador de Vendas----------------"
  putStrLn "\nDigite 1 para gerenciar clientes"
  putStrLn "Digite 2 para gerenciar produtos"
  putStrLn "Digite 3 para gerenciar vendas"
  putStrLn "Digite 9 para sair"
  putStr "Opção: "
  op <- getChar
  getChar
  tratarOpcao op
  if op == '9'
    then return ()
    else main

tratarOpcao :: Char -> IO ()
tratarOpcao '1' = do
  menu_cli
  return ()
tratarOpcao '2' = do
  menu_prod
  return ()
tratarOpcao '3' = do
  menu_vend
  return ()
tratarOpcao '9' = do
  return ()
tratarOpcao _ = do
  putStrLn "Opção invalida"
