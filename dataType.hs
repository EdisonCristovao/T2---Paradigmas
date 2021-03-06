module DataType where

  import System.IO

  type Codigo = Integer
  type Cidade = String
  type Idade = Integer
  type Sexo = Char
  type Clientes = [Cliente]
  data Cliente = Cliente Codigo Nome Cidade Idade Sexo
                  deriving (Show, Read)

  type Codigo_venda = Integer
  type Codigo_item = Integer
  type Codigo_produto = Integer
  type Preco_unitario = Float
  type Percentual_desconto = Integer
  type Total_item = Float
  type ItemVendas = [ItemVenda]
  data ItemVenda = ItemVenda Codigo_venda Codigo_item Codigo_produto Preco_unitario Percentual_desconto Quantidade Total_item
                    deriving (Show, Read, Eq)

  type Nome = String
  type Quantidade = Integer
  type Preco = Float
  type Produtos = [Produto]
  data Produto = Produto Codigo Nome Quantidade Preco
                  deriving (Show, Read)

  type Codigo_cliente = Integer
  type Dia = Integer
  type Mes = Integer
  type Ano = Integer
  type Vendas = [Venda]
  data Venda = Venda Codigo_venda Codigo_cliente Dia Mes Ano
                deriving (Show, Read)

  item_arquivo = "db/itemvenda.db"
  prod_arquivo = "db/produto.db"
  cli_arquivo = "db/cliente.db"
  vend_arquivo = "db/venda.db"

  vend_read_arq :: IO Vendas
  vend_read_arq = do
    handle <- openFile vend_arquivo ReadMode
    dados <- hGetLine handle
    hClose handle
    return (read dados)


  item_read_arq :: IO ItemVendas
  item_read_arq = do
    handle <- openFile item_arquivo ReadMode
    dados <- hGetLine handle
    hClose handle
    return (read dados)

  getTotal :: Venda -> IO Float
  getTotal (Venda co co_c dia mes ano) = do
    itens <- item_read_arq
    let total = getTotalPorIdVenda itens co
    return total


  getTotalPorIdVenda :: ItemVendas -> Integer -> Float
  getTotalPorIdVenda [] _ = 0
  getTotalPorIdVenda ((ItemVenda cod_v cod_i cod_p pre desc_p qtd total):xs) cod | cod == cod_v = (total + getTotalPorIdVenda xs cod_v)
                                                                                 | otherwise = getTotalPorIdVenda xs cod_v
  prod_possui_venda :: Integer -> ItemVendas -> IO Bool
  prod_possui_venda _ [] = return False
  prod_possui_venda id ((ItemVenda cod_v cod_i cod_p prec desc qtd to):xs) | id == cod_p = return True
                                                                           | otherwise = prod_possui_venda id xs

  cli_possui_venda :: Integer -> Vendas -> IO Bool
  cli_possui_venda _ [] = return False
  cli_possui_venda indice ((Venda co co_c dia mes ano):xs) | indice == co_c = return  True
                                                    | otherwise = cli_possui_venda indice xs

  venda_possui_cli :: Integer -> Clientes -> IO Bool
  venda_possui_cli _ [] = return False
  venda_possui_cli co_c ((Cliente cod nome cidade idade sexo):xs) | co_c == cod = return True
                                                                  | otherwise = venda_possui_cli co_c xs
