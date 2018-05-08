module Venda where

  import Data.Time.Clock
  import Data.Time.Calendar
  import System.Process
  import System.IO

  import ItemVenda
  import Cliente
  import Produto
  import DataType


  menu_vend :: IO ()
  menu_vend = do
    system "clear"
    putStrLn "---------------Menu Vendas------------"
    putStrLn "\nDigite 1 para listar Vendas"
    putStrLn "Digite 2 para cadastrar Vendas"
    --putStrLn "Digite 3 para alterar Vendas"
    --putStrLn "Digite 4 para remover Vendas"
    --outras opçoes em breve
    putStr "Opção: "
    op <- getChar
    getChar
    vend_trata_menu op

  vend_trata_menu :: Char -> IO ()
  vend_trata_menu '1' = do
    dados <- vend_read_arq -- pega vendas
    system "clear"
    putStrLn "----------- Listar Vendas ------------"
    vend_list dados
    return ()
  vend_trata_menu '2' = do
    id_cli <- get_cliente -- pega o id do cliente
    clientes <- cli_read_arq
    if (is_cliente id_cli clientes)
      then do
        dados <- vend_read_arq -- pega vendas
        system "clear"
        realizar_venda dados id_cli
      else do
        putStrLn "Cliente nao existe (ENTER PARA SAIR)"
        getLine
        return ()
    return ()

  vend_get_cod_atual :: Vendas -> IO Integer
  vend_get_cod_atual [] = return 0
  vend_get_cod_atual ((Venda co co_c dia mes ano):xs) = return co

  realizar_venda :: Vendas -> Integer -> IO ()
  realizar_venda dados id_cli = do
    cod <- vend_get_cod_atual dados
    now <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay now
    -- chamar cadastro de produtos
    menu_item (cod+1) []
    handle <- openFile vend_arquivo WriteMode
    hPutStrLn handle (show((Venda (cod+1) id_cli (read (show day):: Integer) (read (show month):: Integer) (read (show year):: Integer)):dados))
    hClose handle
    return ()

  vend_list  :: Vendas -> IO ()
  vend_list [] = do
    putStr "\nAperte ENTER para continuar"
    getLine
    return ()
  vend_list (x:xs) = do
    print x
    vend_list xs

  get_cliente :: IO Integer
  get_cliente = do
    system "clear"
    putStrLn "Selecionar Cliente para venda"
    cli_dados <- cli_read_arq
    cli_list cli_dados
    putStr "Digite o Id do cliente: "
    id_cliente <- getLine
    return (read id_cliente:: Integer)

  -- cli_possui_venda :: Integer -> Vendas -> Bool
  -- cli_possui_venda _ [] = False
  -- cli_possui_venda cod_cli ((Venda cod idc day month year):xs) | cod_cli == idc = True
  --                                                              | otherwise = cli_possui_venda cod_cli xs
