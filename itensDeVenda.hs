module ItemVenda where

  import System.Process
  import System.IO
  import DataType
  import Produto



  menu_item :: Integer -> ItemVendas -> IO ()
  menu_item cod_venda items = do
    system "clear"
    putStrLn "---------------Menu Items de Venda------------"
    putStrLn "Carrinho de compra: "
    if items == []
      then putStrLn "Carrinho Vazil"
      else items_list items
    putStrLn "\nDigite 1 para cadastrar Produto"
    putStrLn "Digite 9 para fechar Venda"
    --putStrLn "Digite 3 para alterar Vendas"
    --putStrLn "Digite 4 para remover Vendas"
    --outras opçoes em breve
    putStr "Opção: "
    op <- getChar
    getChar
    item_trata_menu op cod_venda items

  item_trata_menu :: Char -> Integer -> ItemVendas -> IO ()
  item_trata_menu '1' cod_vend items = do
    putStrLn "\nProdutos a venda"
    produtos <- prod_read_arq
    prod_list produtos
    items_add cod_vend items
    return ()
  item_trata_menu '9' cod_vend items = do
    dados <- item_read_arq
    handle <- openFile item_arquivo WriteMode
    hPutStrLn handle (show ((items)++dados))
    hClose handle
    return ()

  is_produto :: Integer -> Produtos -> Bool
  is_produto _ [] = False
  is_produto idProd ((Produto cod nome qtd preco):xs) | cod == idProd = True
                                                      | otherwise = is_produto idProd xs

  items_add :: Integer -> ItemVendas -> IO ()
  items_add cod_vend items = do
    produtos <- prod_read_arq
    putStr "Digite o cod do produto: "
    cod_prod <- getLine
    putStr "Digite a quantidade do produto: "
    qtd_prod <- getLine
    qtd_existente <- (get_qtd (read cod_prod :: Integer) produtos)
    if ((is_produto (read cod_prod :: Integer) produtos) && (qtd_existente >= (read qtd_prod:: Integer)))
      then do
        preco <- get_preco (read cod_prod :: Integer) produtos
        putStr "Digite o percentual de desconto do produto: "
        desc_prod <- getLine
        if ((read desc_prod :: Integer) <= 10) && ((read desc_prod :: Integer) >= 0)
          then do
            let total = (preco * (read qtd_prod :: Float)*((read desc_prod :: Float)/(-100.0)+1.0))
            menu_item cod_vend ((ItemVenda cod_vend 2 (read cod_prod :: Integer) preco (read desc_prod :: Integer) (read qtd_prod :: Integer) total) :items)
          else do
            putStrLn "Desconto invalido (ENTER PARA CONTINUAR)"
            getLine
            menu_item cod_vend items
      else do
        putStrLn "Produto nao existe ou quantidade invalida (ENTER PARA CONTINUAR)"
        getLine
        menu_item cod_vend items
    return ()

  items_list  :: ItemVendas -> IO ()
  items_list [] = do
    putStr "\nAperte ENTER para continuar"
    getLine
    return ()
  items_list (x:xs) = do
    print x
    items_list xs
