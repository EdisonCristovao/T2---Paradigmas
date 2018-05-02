module ItemVenda where

  import Produto
  import System.Process

  type Codigo_venda = Integer
  type Codigo_item = Integer
  type Codigo_produto = Integer
  type Preco_unitario = Float
  type Percentual_desconto = Integer
  type Total_item = Float

  type ItemVendas = [ItemVenda]
  data ItemVenda = ItemVenda Codigo_venda Codigo_item Codigo_produto Preco_unitario Percentual_desconto Quantidade Total_item
                  deriving (Show, Read)

  item_arquivo = "db/itemvenda.db"

  menu_item :: Integer -> IO ()
  menu_item cod_venda = do
    system "clear"
    putStrLn "---------------Menu Items de Venda------------"
    putStrLn "\nDigite 1 para cadastrar Produto"
    putStrLn "Digite 9 para fechar Venda"
    --putStrLn "Digite 3 para alterar Vendas"
    --putStrLn "Digite 4 para remover Vendas"
    --outras opçoes em breve
    putStr "Opção: "
    op <- getChar
    getChar
    item_trata_menu op cod_venda

  item_trata_menu :: Char -> Integer -> IO ()
  item_trata_menu op cod_vend = do
    produtos <- prod_read_arq
    prod_list produtos
    return ()
