module Cliente where

  import System.IO
  import System.Process
  --import Data.List

  type Codigo = Integer
  type Nome = String
  type Cidade = String
  type Idade = Integer
  type Sexo = Char
  type Clientes = [Cliente]
  data Cliente = Cliente Codigo Nome Cidade Idade Sexo
                  deriving (Show, Read)

  cli_arquivo = "db/cliente.db"

  getString :: String -> IO String
  getString str = do
    putStr str
    ret <- getLine
    return ret

  cli_read_arq :: IO Clientes
  cli_read_arq = do
    handle <- openFile cli_arquivo ReadMode
    dados <- hGetLine handle
    hClose handle
    return (read dados)

  menu_cli :: IO ()
  menu_cli = do
    system "clear"
    putStrLn "---------------Menu Clientes------------"
    putStrLn "\nDigite 1 para listar Clientes"
    putStrLn "Digite 2 para cadastrar Cliente"
    putStrLn "Digite 3 para alterar Cliente"
    putStrLn "Digite 4 para remover Cliente"
    --outras opçoes em breve
    putStr "Opção: "
    op <- getChar
    getChar
    cli_trata_menu op

  cli_trata_menu :: Char -> IO ()
  cli_trata_menu '1' = do
    dados <- cli_read_arq
    system "clear"
    putStrLn "---------------Listar Clientes------------\n"
    cli_list dados
    return ()
  cli_trata_menu '2' = do
    dados <- cli_read_arq
    cli_add dados
    return ()
  cli_trata_menu '3' = do
    dados <- cli_read_arq
    cli_edit dados
    return ()
  cli_trata_menu '4' = do
    dados <- cli_read_arq
    cli_remove dados
    return ()
  cli_trata_menu _ = do
    return ()

  cli_edit :: Clientes -> IO ()
  cli_edit dados = do
    system "clear"
    putStrLn "---------------Alterar Cliente------------"
    cli_list dados
    putStr "Digite o id para alterar: "
    indice <- getLine
    nome <- getString "\nDigite o Nome: "
    cidade <- getString "\nDigite a Cidade: "
    idade <- getString "\nDigite a Idade: "
    putStr "\nDigite o Sexo 'M' ou 'F': "
    sexo <- getChar
    handle <- openFile cli_arquivo WriteMode
    hPutStrLn handle (show (editar dados (Cliente (read indice :: Integer) nome cidade (read idade:: Integer) sexo)))
    hClose handle
    return ()

  editar :: Clientes -> Cliente -> Clientes
  editar [] _ = []
  editar ((Cliente co no ci ida se):xs) (Cliente coA noA ciA idaA seA) | co == coA = ((Cliente co noA ciA idaA seA):xs)
                                                                       | otherwise = ((Cliente co no ci ida se) : (editar xs (Cliente coA noA ciA idaA seA)))
  cli_remove :: Clientes -> IO ()
  cli_remove dados = do
    system "clear"
    putStrLn "---------------Remover Cliente------------"
    cli_list dados
    putStr "Digite o id para remover: "
    indice <- getLine
    handle <- openFile cli_arquivo WriteMode
    hPutStrLn handle (show (remove dados (read indice :: Integer)))
    hClose handle
    return ()

  remove :: Clientes -> Integer -> Clientes
  remove [] _ = []
  remove ((Cliente co no ci ida se):xs) indice | co == indice = xs
                                               | otherwise = ((Cliente co no ci ida se) : (remove xs indice))
  cli_get_cod_atual :: Clientes -> IO Integer
  cli_get_cod_atual [] = return 0
  cli_get_cod_atual ((Cliente co no ci ida se):xs) = return co

  cli_list  :: Clientes -> IO ()
  cli_list [] = do
    putStr "\nAperte ENTER para continuar"
    getLine
    return ()
  cli_list (x:xs) = do
    print x
    cli_list xs

  cli_add :: Clientes -> IO ()
  cli_add dados = do
    system "clear"
    putStrLn "---------------Adicionar Cliente------------"
    cod <- cli_get_cod_atual dados -- pega o cod do ultima
    nome <- getString "\nDigite o Nome: "
    cidade <- getString "\nDigite a Cidade: "
    idade <- getString "\nDigite a Idade: "
    putStr "\nDigite o Sexo 'M' ou 'F': "
    sexo <- getChar
    --- abrindo arquivo e adicionando cli -----
    handle <- openFile cli_arquivo WriteMode
    hPutStrLn handle (show ((Cliente (cod + 1) nome cidade (read idade:: Integer) sexo):dados))
    hClose handle
    return ()
  -------------------------------------------------------
