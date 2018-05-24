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
    putStrLn "Digite 5 para relatorio por periodo"
    putStrLn "Digite 6 para relatorio por cliente"
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
  vend_trata_menu '5' = do
    relatorio_periodo
    return ()
  vend_trata_menu '6' = do
    relatorio_cliente
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

  vend_list_client  :: Vendas -> IO ()
  vend_list_client [] = do
    putStr "\nAperte ENTER para continuar"
    getLine
    return ()
  vend_list_client (x:xs) = do
    total <- getTotal x
    print x
    print ("Total: " ++ show total)
    vend_list_client xs

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


  relatorio_periodo :: IO ()
  relatorio_periodo = do
    putStrLn "\nDigite o dia inicial: "
    dia_inicial <- getLine
    putStrLn "\nDigite o mes inicial:"
    mes_inicial <- getLine
    putStrLn "\nDigite o ano inicial:"
    ano_inicial <- getLine
    putStrLn "\nDigite o dia final: "
    dia_final <- getLine
    putStrLn "\nDigite o mes final:"
    mes_final <- getLine
    putStrLn "\nDigite o ano final:"
    ano_final <- getLine
    vendas <- vend_read_arq
    let vendas_periodo = get_vendas_periodo (read dia_inicial :: Integer) (read mes_inicial :: Integer) (read ano_inicial :: Integer) (read dia_final :: Integer) (read mes_final :: Integer) (read ano_final :: Integer) vendas
    vend_list_cli vendas_periodo
    return ()

  -- Dia Inicial -> Mes Inicial -> Ano inicial -> Dia Final -> Mes Final -> Ano Final
  get_vendas_periodo :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Vendas -> Vendas
  get_vendas_periodo _ _ _ _ _ _ [] = []
  get_vendas_periodo dia_inicial mes_inicial ano_inicial dia_final mes_final ano_final ((Venda co co_c dia mes ano):xs)
    | dia_inicial <= dia && mes_inicial <= mes && ano_inicial <= ano && dia_final >= dia && mes_final >= mes && ano_final >= ano = ((Venda co co_c dia mes ano) : (get_vendas_periodo dia_inicial mes_inicial ano_inicial dia_final mes_final ano_final xs))
    | otherwise = get_vendas_periodo dia_inicial mes_inicial ano_inicial dia_final mes_final ano_final xs

  relatorio_cliente :: IO ()
  relatorio_cliente = do
    putStrLn "Digite o ID do cliente: "
    id_cli <- getLine
    vendas <- vend_read_arq
    let vendas_cliente = get_vendas_cliente (read id_cli :: Integer) vendas
    vend_list vendas_cliente
    return ()

  get_vendas_cliente :: Integer -> Vendas -> Vendas
  get_vendas_cliente _ [] = []
  get_vendas_cliente id ((Venda co co_c dia mes ano):xs)
    | id == co_c = ((Venda co co_c dia mes ano) : (get_vendas_cliente id xs))
    | otherwise = get_vendas_cliente id xs

  coerencia_de_vendas :: Vendas -> IO ()
  coerencia_de_vendas [] = do
    getLine
    return ()
  coerencia_de_vendas ((Venda co co_c dia mes ano):xs) = do
    dados_cli <- cli_read_arq
    possuiCli <- venda_possui_cli co_c dados_cli
    if possuiCli
      then do
        putStrLn ("Venda :" ++ (show co) ++ " possui um cliente na base")
        return()
      else do
        putStrLn ("Venda :" ++ (show co) ++ " não possui um cliente na base")
        return()
    items <- item_read_arq
    get_itens_venda co items
    coerencia_de_vendas xs

  get_itens_venda :: Integer -> ItemVendas -> IO ()
  get_itens_venda _ [] = return ()
  get_itens_venda cod_v ((ItemVenda cod_vend cod_i cod_prod preco desc_prod qtd_prod total):xs) = do
    if cod_v == cod_vend then do
      let tot = (preco * (read (show qtd_prod) :: Float)*((read (show desc_prod) :: Float)/(-100.0)+1.0))
      if tot == total then do
        putStrLn ("Total da venda :" ++ (show cod_vend) ++ " está igual ao banco")
      else do
        putStrLn ("Total da venda :" ++ (show cod_vend) ++ " não está igual ao banco")
    else do
      get_itens_venda cod_v xs
      return ()
