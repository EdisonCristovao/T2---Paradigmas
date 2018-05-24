module Produto where

  import System.IO
  import System.Process
  import DataType


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
    quantid <- getString "\nDigite a quantidade: "
    preco <- getString "\nDigite o preco: "
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
    items <- item_read_arq
    possuiItem <- prod_possui_venda (read indice :: Integer) items
    if possuiItem
      then do
        putStrLn "Produto possui venda"
        getLine
        return ()
      else do
        handle <- openFile prod_arquivo WriteMode
        hPutStrLn handle (show (remove dados (read indice :: Integer)))
        hClose handle
        return ()
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

  get_preco :: Integer -> Produtos -> IO Preco
  get_preco _ [] = return 0
  get_preco cod_pro ((Produto co no qt pr):xs) | cod_pro == co = return pr
                                              | otherwise = get_preco cod_pro xs

  get_qtd :: Integer -> Produtos -> IO Quantidade
  get_qtd _ [] = return 0
  get_qtd cod_pro ((Produto co no qt pr):xs) | cod_pro == co = return qt
                                              | otherwise = get_qtd cod_pro xs

  -- prepara_baixa_estoque :: ItemVendas -> Produtos -> IO Produtos
  -- prepara_baixa_estoque [] _ = return []
  -- prepara_baixa_estoque (item:ws) produtos = baixa_qtd item
  --
  -- baixa_qtd :: ItemVenda -> Produtos -> Produtos
  -- baixa_qtd (ItemVenda codV codiV codpV precV descV qtdV totV) ((Produto cod nome qtd preco):xs) | codpV == cod = ((Produto cod nome qtd preco):xs)
  --                                                                                                | otherwise = (Produto cod nome qtd preco) : (baixa_qtd (ItemVenda codV codiV codpV precV descV qtdV totV) xs)
