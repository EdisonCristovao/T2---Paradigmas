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
