module Produto where

  import System.IO
  import System.Process

  type Codigo = Integer
  type Nome = String
  type Quantidade = Integer
  type Preco = Float
  type Produtos = [Produto]
  data Produto = Produto Codigo Nome Quantidade Preco
                deriving (Show, Read)

  prod_arquivo = "db/produto.db"

  getString :: String -> IO String
  getString str = do
    putStr str
    ret <- getLine
    return ret

  prod_read_arq :: IO Produtos
  prod_read_arq = do
    handle <- openFile prod_arquivo ReadMode
    dados <- hGetLine handle
    hClose handle
    return (read dados)

  menu_prod :: IO ()
  menu_prod = do
    system "clear"
    putStrLn "---------------Menu Produtos------------"
    putStrLn "\nDigite 1 para listar Produtos"
    putStrLn "Digite 2 para cadastrar Produtos"
    putStrLn "Digite 3 para alterar Produtos"
    putStrLn "Digite 4 para remover Produtos"
    --outras opçoes em breve
    putStr "Opção: "
    op <- getChar
    getChar
    trata_menu op

  trata_menu :: Char -> IO ()
  trata_menu '1' = do
    dados <- prod_read_arq
    system "clear"
    putStrLn "---------------Listar produtos ------------\n"
    prod_list dados
    return ()
  trata_menu '2' = do
    dados <- prod_read_arq
    prod_add dados
    return ()
  trata_menu '3' = do
    dados <- prod_read_arq
    prod_edit dados
    return ()
  trata_menu '4' = do
    dados <- prod_read_arq
    prod_remove dados
    return ()
  trata_menu _ = do
    return ()

  prod_list  :: Produtos -> IO ()
  prod_list [] = do
    putStr "\nAperte ENTER para continuar"
    getLine
    return ()
  prod_list (x:xs) = do
    print x
    prod_list xs

  prod_edit :: Produtos -> IO ()
  prod_edit dados = do
    system "clear"
    putStrLn "---------------Alterar Produtos------------"
    prod_list dados
    putStr "Digite o id para alterar: "
    indice <- getLine
    nome <- getString "\nDigite o Nome: "
    quantid <- getString "\nDigite a Cidade: "
    preco <- getString "\nDigite a Idade: "
    handle <- openFile prod_arquivo WriteMode
    hPutStrLn handle (show (editar dados (Produto (read indice :: Integer) nome (read quantid :: Integer) (read preco:: Float))))
    hClose handle
    return ()

  editar :: Produtos -> Produto -> Produtos
  editar [] _ = []
  editar ((Produto co no qt pr):xs) (Produto coA noA qtA prA) | co == coA = ((Produto co noA qtA prA):xs)
                                                                       | otherwise = ((Produto co no qt pr) : (editar xs (Produto coA noA qtA prA)))

  prod_remove :: Produtos -> IO ()
  prod_remove dados = do
    system "clear"
    putStrLn "---------------Remover Produto------------"
    prod_list dados
    putStr "Digite o id para remover: "
    indice <- getLine
    handle <- openFile prod_arquivo WriteMode
    hPutStrLn handle (show (remove dados (read indice :: Integer)))
    hClose handle
    return ()

  remove :: Produtos -> Integer -> Produtos
  remove [] _ = []
  remove ((Produto co no qt pr):xs) indice | co == indice = xs
                                           | otherwise = ((Produto co no qt pr) : (remove xs indice))


  get_cod_atual :: Produtos -> IO Integer
  get_cod_atual [] = return 0
  get_cod_atual ((Produto co no qt pr):xs) = return co

  prod_add :: Produtos -> IO ()
  prod_add dados = do
    system "clear"
    putStrLn "---------------Adicionar Produto------------"
    cod <- get_cod_atual dados -- pega o cod do ultima
    nome <- getString "\nDigite o Nome: "
    putStr "Digite a quantidade: "
    quantid <- getLine
    putStr "Digite o preco: "
    preco <- getLine
    --- abrindo arquivo e adicionando Prod -----
    handle <- openFile prod_arquivo WriteMode
    hPutStrLn handle (show ((Produto (cod + 1) nome (read quantid :: Integer) (read preco:: Float)):dados))
    hClose handle
    return ()
