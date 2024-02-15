---- Link para replit.com: https://replit.com/@christiandoramo/Projeto-Paradigmas-1va#Main.hs

---- Equipe --
-- CAIO FONTES DE MELO
-- CARLOS EDUARDO RIBEIRO
-- CHRISTIAN OLIVEIRA DO RAMO
-- GIOVANNI LIMA DO NASCIMENTO

-- para rodar no vs code - usar ghci, depois :load Main.hs, depois main

module Main where

import Data.List
import System.IO
import Text.Read

-- TIPOS PRINCIPAIS
data Cliente = Cliente {getIdC :: Int, getNome :: String, getTelefone :: String, getEndereco :: String, getCpf :: String} deriving (Read, Show)

data Jogo = Jogo {getIdJ :: Int, getTitulo :: String, getPlataforma :: String, getCategoria :: String, getPreco :: Float, getQtdDisponivel :: Int} deriving (Read, Show)

data Locacao = Locacao {getIdL :: Int, getCliente :: Cliente, getJogo :: Jogo} deriving (Read, Show)

-- instance Eq Cliente where
--   Cliente i1 t1 n1 e1 == Cliente i2 t2 n2 e2 = i1 == i2 && t1 == t2 && n1 == n2 && e1 == e2

type Banco = ([Jogo], [Cliente], [Locacao])

banco :: Banco -- Banco de itens Gerais (Jogos, Clientes, Locações)
banco = ([], [], []) -- inicializando banco vazio

-- FUNÇOES PRINCIPAIS

-- CREATE
-- ADICIONAR JOGO
adicionarJogo :: Jogo -> Banco -> IO Banco
adicionarJogo jogo@(Jogo _ titulo _ _ _ _) (jogos, clientes, locacoes)
  -- Verifica se já existe um jogo com o mesmo título no banco
  | any (\j -> getTitulo j == titulo) jogos = do
      putStrLn "Erro: Jogo com mesmo titulo já cadastrado!"
      return (jogos, clientes, locacoes)
  -- Se não houver jogo com o mesmo título, adiciona o novo jogo ao banco
  | otherwise = do
      let id = if null jogos then 1 else getIdJ (head jogos) + 1
          novoJogo = jogo {getIdJ = id}
      return (novoJogo : jogos, clientes, locacoes)

-- ADICIONAR Cliente
adicionarCliente :: Cliente -> Banco -> IO Banco
adicionarCliente cliente@(Cliente _ _ _ _ cpf) (jogos, clientes, locacoes)
  -- Verifica se já existe um cliente com o mesmo CPF no banco
  | any (\c -> getCpf c == cpf) clientes = do
      putStrLn "Erro: Cliente com mesmo CPF já cadastrado!"
      return (jogos, clientes, locacoes)
  -- Se não houver cliente com o mesmo CPF, adiciona o novo cliente ao banco
  | otherwise = do
      let id = if null clientes then 1 else getIdC (head clientes) + 1
          novoCliente = cliente {getIdC = id}
      return (jogos, novoCliente : clientes, locacoes)

-- ADICIONAR LOCACAO
-- Esta função adiciona uma nova locação ao banco de dados. Ela gera um novo ID para a locação e retorna o banco de dados atualizado.
adicionarLocacao :: Locacao -> Banco -> Banco
adicionarLocacao locacao (jogos, clientes, locacoes) =
  let id = if null locacoes then 1 else getIdL (head locacoes) + 1 -- Calcula o novo ID para a locação
      novaLocacao = locacao {getIdL = id}
   in (jogos, clientes, novaLocacao : locacoes)

-- ALUGAR JOGO
-- Esta função tenta alugar um jogo para um cliente. Se o cliente e o jogo existirem e o jogo estiver disponível, a função atualiza a quantidade disponível do jogo, adiciona uma nova locação ao banco de dados e retorna o banco de dados atualizado. Caso contrário, retorna uma mensagem de erro.
alugarJogo :: Int -> Int -> Banco -> Either String Banco
alugarJogo idCliente idJogo banco@(jogos, clientes, locacoes) =
  case (buscarClientePorId idCliente banco, buscarJogoPorId idJogo banco) of
    (Just cliente, Just jogo) ->
      if getQtdDisponivel jogo > 0
        then
          let novoJogo = jogo {getQtdDisponivel = getQtdDisponivel jogo - 1}
              novosJogos = novoJogo : filter (\j -> getIdJ j /= idJogo) jogos
           in Right $ adicionarLocacao (Locacao {getIdL = 0, getCliente = cliente, getJogo = novoJogo}) (novosJogos, clientes, locacoes)
        else Left "\nO jogo não está disponível.\n"
    (_, Nothing) -> Left "\nO jogo não existe.\n"
    (Nothing, _) -> Left "\nO cliente não existe.\n"

-- READ
-- LISTAR JOGOS
-- Esta função retorna uma string com a lista de todos os jogos no banco de dados. Se não houver jogos, retorna uma mensagem informando que não existem jogos salvos.
listarJogos :: Banco -> String
listarJogos (jogos, _, _) = if null jogos then "Não existem jogos salvos.\n\n" else unlines $ map formatarJogo jogos
  where 
    -- Verifica se a lista de jogos está vazia
    formatarJogo jogo =
      "Id: "
        ++ show (getIdJ jogo)
        ++ "\n"
        ++ "Título: "
        ++ getTitulo jogo
        ++ "\n"
        ++ "Plataforma: "
        ++ getPlataforma jogo
        ++ "\n"
        ++ "Categoria: "
        ++ getCategoria jogo
        ++ "\n"
        ++ "Preço: "
        ++ show (getPreco jogo)
        ++ "\n"
        ++ "Quantidade Disponível: "
        ++ show (getQtdDisponivel jogo)
        ++ "\n"

-- LISTAR CLIENTES
-- Esta função retorna uma string com a lista de todos os clientes no banco de dados. Se não houver clientes, retorna uma mensagem informando que não existem clientes salvos.
listarClientes :: Banco -> String
listarClientes (_, clientes, _) = if null clientes then "Não existem clientes salvos.\n\n" else unlines $ map formatarCliente clientes
  where
    formatarCliente cliente =
      "Id: "
        ++ show (getIdC cliente)
        ++ "\n"
        ++ "Nome: "
        ++ getNome cliente
        ++ "\n"
        ++ "Telefone: "
        ++ getTelefone cliente
        ++ "\n"
        ++ "Endereço: "
        ++ getEndereco cliente
        ++ "\n"
        ++ "Cpf:"
        ++ getCpf cliente
        ++ "\n"

-- LISTAR LOCAÇÕES
-- Esta função retorna uma string com a lista de todas as locações no banco de dados. Se não houver locações, retorna uma mensagem informando que não existem locações salvas.
listarLocacoes :: Banco -> String
listarLocacoes (_, _, locacoes) = if null locacoes then "Não existem locações salvas.\n\n" else unlines $ map formatarLocacao locacoes
  where
    formatarLocacao locacao =
      "Id da Locação: "
        ++ show (getIdL locacao)
        ++ "\n"
        ++ "Id do Cliente: "
        ++ show (getIdC (getCliente locacao))
        ++ "\n"
        ++ "Nome do Cliente: "
        ++ getNome (getCliente locacao)
        ++ "\n"
        ++ "Id do Jogo: "
        ++ show (getIdJ (getJogo locacao))
        ++ "\n"
        ++ "Nome do Jogo: "
        ++ getTitulo (getJogo locacao)
        ++ "\n"
        ++ "Preço do Jogo: "
        ++ show (getPreco (getJogo locacao))
        ++ "\n"

-- BUSCAR CLIENTE POR CPF
-- Esta função busca um cliente no banco de dados pelo CPF. Se o cliente for encontrado, retorna uma string formatada com as informações do cliente. Se o cliente não for encontrado, retorna uma mensagem de erro.
buscarClientePorCpf :: String -> Banco -> String
buscarClientePorCpf cpf banco
  | null clientesFiltrados = "Erro: Cliente com o cpf: " ++ cpf ++ " não encontrado."
  | otherwise = unlines $ map formatarCliente clientesFiltrados
  where
    filtrarClientes cpf (_, clientes, _) = filter (\cliente -> getCpf cliente == cpf) clientes
    formatarCliente cliente =
      "\nId: "
        ++ show (getIdC cliente)
        ++ "\n"
        ++ "Nome: "
        ++ getNome cliente
        ++ "\n"
        ++ "Telefone: "
        ++ getTelefone cliente
        ++ "\n"
        ++ "Endereço: "
        ++ getEndereco cliente
        ++ "\n"
        ++ "Cpf: "
        ++ getCpf cliente
    clientesFiltrados = filtrarClientes cpf banco

-- BUSCAR JOGO POR TITULO
-- Esta função busca um jogo no banco de dados pelo título. Se o jogo for encontrado, retorna uma string formatada com as informações do jogo. Se o jogo não for encontrado, retorna uma mensagem de erro.
buscarJogoPorTitulo :: String -> Banco -> String
buscarJogoPorTitulo titulo banco
  | null jogosFiltrados = "Erro: jogo com o título: " ++ titulo ++ " não encontrado."
  | otherwise = unlines $ map formatarJogo jogosFiltrados
  where
    filtrarJogos titulo (jogos, _, _) = filter (\jogo -> getTitulo jogo == titulo) jogos
    formatarJogo jogo =
      "Id: "
        ++ show (getIdJ jogo)
        ++ "\n"
        ++ "Título: "
        ++ getTitulo jogo
        ++ "\n"
        ++ "Plataforma: "
        ++ getPlataforma jogo
        ++ "\n"
        ++ "Categoria: "
        ++ getCategoria jogo
        ++ "\n"
        ++ "Preço: "
        ++ show (getPreco jogo)
        ++ "\n"
        ++ "Quantidade Disponível: "
        ++ show (getQtdDisponivel jogo)
    jogosFiltrados = filtrarJogos titulo banco

-- BUSCAR CLIENTE POR ID: Recebe um ID de cliente e o banco de dados, como parâmetros. Usa a função filter para localiza o cliente no banco cujo ID corresponde ao ID fornecido.
buscarClientePorId :: Int -> Banco -> Maybe Cliente
buscarClientePorId id (_, clientes, _) =
  case filter (\cliente -> getIdC cliente == id) clientes of
    [] -> Nothing
    (cliente : _) -> Just cliente

-- BUSCAR JOGO POR ID: Recebe um ID de jogo e o banco de dados como parâmetros. Usa a função filter para localiza o jogo no banco cujo ID corresponde ao ID fornecido.
buscarJogoPorId :: Int -> Banco -> Maybe Jogo
buscarJogoPorId id (jogos, _, _) =
  case filter (\jogo -> getIdJ jogo == id) jogos of
    [] -> Nothing
    (jogo : _) -> Just jogo

-- BUSCAR LOCACAO POR ID: Recebe um ID de locação e o banco de dados como parâmetros. Usa a função filter para localiza a locação no banco cujo ID corresponde ao ID fornecido...
buscarLocacaoPorId :: Int -> Banco -> Maybe Locacao
buscarLocacaoPorId id (_, _, locacoes) =
  case filter (\locacao -> getIdL locacao == id) locacoes of
    [] -> Nothing
    (locacao : _) -> Just locacao

-- UPDATE

-- UPDATE JOGO

-- ALTERAR NOME JOGO: Recebe o ID de um jogo, um novo nome e o banco como parâmetros. Localiza o jogo no banco usando ID fornecido e atualiza seu nome com novo nome fornecido.
alterarNomeJogo :: Int -> String -> Banco -> Maybe Banco
alterarNomeJogo id novoNome (jogos, clientes, locacoes) =
  case find (\jogo -> getIdJ jogo == id) jogos of
    Nothing -> Nothing
    Just _ ->
      let atualizarJogo jogo = if getIdJ jogo == id then jogo {getTitulo = novoNome} else jogo
          novosJogos = map atualizarJogo jogos
       in Just (novosJogos, clientes, locacoes)

-- ALTERAR PREÇO JOGO: Recebe o ID de um jogo, um novo preço e o banco como parâmetros. Localiza o jogo no banco usando o ID e atualiza seu preço com o novo valor fornecido.
alterarPrecoJogo :: Int -> Float -> Banco -> Maybe Banco
alterarPrecoJogo id novoPreco (jogos, clientes, locacoes) =
  case find (\jogo -> getIdJ jogo == id) jogos of
    Nothing -> Nothing
    Just _ ->
      let atualizarJogo jogo = if getIdJ jogo == id then jogo {getPreco = novoPreco} else jogo
          novosJogos = map atualizarJogo jogos
       in Just (novosJogos, clientes, locacoes)

-- ALTERAR QTD JOGO: Recebe o ID de um jogo, uma nova quantidade e o banco como parâmetros. Localiza o jogo pelo ID e ajusta a quantidade disponível para o novo valor fornecido.
alterarQtdJogo :: Int -> Int -> Banco -> Maybe Banco
alterarQtdJogo id novaQtd (jogos, clientes, locacoes) =
  case find (\jogo -> getIdJ jogo == id) jogos of
    Nothing -> Nothing
    Just _ ->
      let atualizarJogo jogo = if getIdJ jogo == id then jogo {getQtdDisponivel = novaQtd} else jogo
          novosJogos = map atualizarJogo jogos
       in Just (novosJogos, clientes, locacoes)

-- UPDATE CLIENTE

-- ALTERAR NOME CLIENTE: Recebe o ID de um cliente, um novo nome e o banco como parâmetros. Localiza o cliente no banco usando o ID e atualiza seu nome para o novo nome fornecido.
alterarNomeCliente :: Int -> String -> Banco -> Maybe Banco
alterarNomeCliente id novoNome (jogos, clientes, locacoes) =
  case find (\cliente -> getIdC cliente == id) clientes of
    Nothing -> Nothing
    Just _ ->
      let atualizarCliente cliente = if getIdC cliente == id then cliente {getNome = novoNome} else cliente
          novosClientes = map atualizarCliente clientes
       in Just (jogos, novosClientes, locacoes)

-- ALTERAR TELEFONE CLIENTE: Recebe o ID de um cliente, um novo número de telefone e o banco como parâmetros. Localiza o cliente pelo ID e atualiza seu número de telefone com o novo número fornecido.
alterarTelefoneCliente :: Int -> String -> Banco -> Maybe Banco
alterarTelefoneCliente id novoTelefone (jogos, clientes, locacoes) =
  case find (\cliente -> getIdC cliente == id) clientes of
    Nothing -> Nothing
    Just _ ->
      let atualizarCliente cliente = if getIdC cliente == id then cliente {getTelefone = novoTelefone} else cliente
          novosClientes = map atualizarCliente clientes
       in Just (jogos, novosClientes, locacoes)

-- ALTERAR ENDEREÇO CLIENTE: Utiliza o ID de um cliente, um novo endereço e o banco como parâmetros. Localiza o cliente pelo ID e atualiza seu endereço para o novo endereço fornecido.
alterarEnderecoCliente :: Int -> String -> Banco -> Maybe Banco
alterarEnderecoCliente id novoEndereco (jogos, clientes, locacoes) =
  case find (\cliente -> getIdC cliente == id) clientes of
    Nothing -> Nothing
    Just _ ->
      let atualizarCliente cliente = if getIdC cliente == id then cliente {getEndereco = novoEndereco} else cliente
          novosClientes = map atualizarCliente clientes
       in Just (jogos, novosClientes, locacoes)

-- DESTROY

-- REMOVER JOGO POR ID
removerJogo :: Int -> Banco -> Maybe Banco --- REMOVER JOGO POR ID(funciona) deve comparar id e voltar lista sem o item
removerJogo id (jogos, clientes, locacoes) =
  case find (\jogo -> getIdJ jogo == id) jogos of
    Nothing -> Nothing -- Jogo não encontrado, retorna nada
    Just _ ->
      let novosJogos = filter (\jogo -> getIdJ jogo /= id) jogos
       in Just (novosJogos, clientes, locacoes)

-- REMOVER CLIENTE POR ID
removerCliente :: Int -> Banco -> Maybe Banco --- Remove CLIENTE POR ID(funciona) deve comparar id e voltar lista sem o item
removerCliente id (jogos, clientes, locacoes) =
  case find (\cliente -> getIdC cliente == id) clientes of
    Nothing -> Nothing -- Cliente não encontrado, retorna nada
    Just _ ->
      let clientesRemovidos = filter (\cliente -> getIdC cliente /= id) clientes
       in Just (jogos, clientesRemovidos, locacoes)

-- DESALOCAR JOGO POR ID
-- Esta função desaloca um jogo do banco de dados pelo ID da locação. Se a locação for encontrada, a função atualiza a quantidade disponível do jogo e retorna o banco de dados atualizado. Se a locação não for encontrada, retorna uma mensagem de erro.
desalocarJogo :: Int -> Banco -> Either String Banco
desalocarJogo idLocacao banco@(jogos, clientes, locacoes) =
  case buscarLocacaoPorId idLocacao banco of
    Just locacao ->
      let jogo = getJogo locacao
          novoJogo = jogo {getQtdDisponivel = getQtdDisponivel jogo + 1}
          novosJogos = novoJogo : filter (\j -> getIdJ j /= getIdJ jogo) jogos
          novasLocacoes = filter (\l -> getIdL l /= idLocacao) locacoes
       in Right (novosJogos, clientes, novasLocacoes)
    Nothing -> Left "\nA locação não existe.\n"

-- RELATÓRIO
-- Esta função gera um relatório do banco de dados, incluindo a lista de locações, o número de locações atuais, o número de itens disponíveis para cada jogo e a receita total recebida por todas as locações.
relatorio :: Banco -> String
relatorio banco@(jogos, _, locacoes) =
  "Locações:\n\n"
    ++ listarLocacoes banco
    ++ "Número de locações atuais: "
    ++ show (length locacoes)
    ++ "\n"
    ++ "Número de itens disponíveis para cada jogo:\n"
    ++ unlines (map jogoEQuantidade jogos)
    ++ "Receita total recebida por todas as locações: "
    ++ show (sum (map precoLocacao locacoes))
    ++ "\n"
  where
    jogoEQuantidade jogo = getTitulo jogo ++ ": " ++ show (getQtdDisponivel jogo)
    precoLocacao locacao = getPreco (getJogo locacao)

-- Print salvar e sair
-- Esta função retorna uma string informando que o banco de dados está sendo salvo e o programa está saindo.
printSalvarESair :: Banco -> String
printSalvarESair banco@(_, _, _) = "\nSalvando e saindo..."

-- ARQUIVO
-- Esta função escreve o banco de dados em um arquivo.
escreveArquivo :: FilePath -> Banco -> IO ()
escreveArquivo file banco = writeFile file (show banco)

-- Esta função lê o banco de dados de um arquivo.
leArquivo :: FilePath -> IO Banco
leArquivo file = do
  conteudo <- readFile file
  return (read conteudo :: Banco)

nomeArquivo = "repositorio.txt"

-- Esta é a função principal que lê o banco de dados do arquivo e inicia o menu principal.
main :: IO ()
main = do
  banco <- leArquivo nomeArquivo
  menuPrincipal banco

-- Menu Principal
-- Esta função exibe o menu principal e lê a opção do usuário. Dependendo da opção escolhida pelo usuário, a função chama a função correspondente.
menuPrincipal :: Banco -> IO ()
menuPrincipal banco = do
  putStr "\n<Bem-vindo à Locadora de Jogos>\n\n"
  putStrLn " 1) Criar"
  putStrLn " 2) Atualizar"
  putStrLn " 3) Consultar"
  putStrLn " 4) Remover"
  putStrLn " 5) Relatório"
  putStrLn " 6) Salvar Alterações e Sair"

  putStr "\nEscolha uma opção: "
  hFlush stdout
  opcao <- getLine

  case opcao of
    "1" -> menuCriar banco
    "2" -> menuAtualizar banco
    "3" -> menuConsultar banco
    "4" -> menuDeletar banco
    "5" -> do
      putStrLn "\n(Relatório)\n"
      putStrLn $ relatorio banco
      menuPrincipal banco
    "6" -> do
      putStrLn $ printSalvarESair banco
      escreveArquivo nomeArquivo banco
      return ()
    _ -> do
      putStrLn "\nOpção inválida. Tente novamente."
      menuPrincipal banco

-- Menu Criar
-- Esta função exibe o menu de criação e lê a opção do usuário. Dependendo da opção escolhida pelo usuário, a função solicita as informações necessárias do usuário e chama a função correspondente para adicionar um cliente, adicionar um jogo ou alugar um jogo.
menuCriar :: Banco -> IO ()
menuCriar banco = do
  putStr "\n(CADASTRO)\n\n"
  putStrLn " 1) Adicionar Cliente"
  putStrLn " 2) Adicionar Jogo"
  putStrLn " 3) Alugar Jogo"
  putStrLn " 4) Voltar para o Menu Principal"

  putStr "\nEscolha uma opção: "
  hFlush stdout
  opcao <- getLine

  case opcao of
    -- Opção 1: Adicionar Cliente
    "1" -> do
      putStr "\nDigite o nome do cliente: "
      nome <- getLine
      putStr "Digite o telefone do cliente: "
      telefone <- getLine
      putStr "Digite o endereço do cliente: "
      endereco <- getLine
      putStr "Digite o cpf do cliente: "
      cpf <- getLine
      putStr "\n"
      let novoCliente = Cliente {getIdC = 0, getNome = nome, getTelefone = telefone, getEndereco = endereco, getCpf = cpf}
      novoBanco <- adicionarCliente novoCliente banco
      menuPrincipal novoBanco

    -- Opção 2: Adicionar Jogo
    "2" -> do
      putStr "\nDigite o título do jogo: "
      titulo <- getLine
      putStr "Digite a plataforma do jogo: "
      plataforma <- getLine
      putStr "Digite a categoria do jogo: "
      categoria <- getLine
      putStr "Digite o preço do jogo: "
      preco <- readLn
      putStr "Digite a quantidade disponível do jogo: "
      qtd <- readLn
      putStr "\n"
      let novoJogo = Jogo {getIdJ = 0, getTitulo = titulo, getPlataforma = plataforma, getCategoria = categoria, getPreco = preco, getQtdDisponivel = qtd}
      novoBanco <- adicionarJogo novoJogo banco
      menuPrincipal novoBanco

    -- Opção 3: Alugar Jogo
    "3" -> do
      putStr "\nDigite o ID do cliente:"
      idCliente <- readLn
      putStr "Digite o ID do jogo:"
      idJogo <- readLn
      case alugarJogo idCliente idJogo banco of
        Right novoBanco -> do
          putStrLn "\nJogo alugado com sucesso!\n"
          menuPrincipal novoBanco
        Left mensagem -> do
          putStrLn mensagem
          menuPrincipal banco

    -- Opção 4: Voltar para o menuPrincipal
    "4" -> do
      putStrLn "\nVoltando..."
      menuPrincipal banco
    _ -> do
      putStrLn "\nOpção inválida. Tente novamente."
      menuCriar banco

-- Menu Atualizar
-- Esta função exibe o menu de atualização e lê a opção do usuário. Dependendo da opção escolhida pelo usuário, a função solicita as informações necessárias do usuário e chama a função correspondente para alterar o nome do jogo, alterar o preço do jogo, alterar a quantidade do jogo, alterar o nome do cliente, alterar o telefone do cliente ou alterar o endereço do cliente.
menuAtualizar :: Banco -> IO ()
menuAtualizar banco = do
  putStr "\n(ATUALIZAR)\n\n"
  putStrLn " 1) Alterar Nome de Jogo"
  putStrLn " 2) Alterar Preço de Jogo"
  putStrLn " 3) Alterar Quantidade de Jogo"
  putStrLn " 4) Alterar Nome de Cliente"
  putStrLn " 5) Alterar Telefone de Cliente"
  putStrLn " 6) Alterar Endereço de Cliente"
  putStrLn " 7) Voltar para o Menu Principal"

  putStr "\nEscolha uma opção: "
  hFlush stdout
  opcao <- getLine

  case opcao of
    -- Opção 1: Alterar Nome de Jogo
    "1" -> do
      putStr "\nDigite o ID do jogo que deseja alterar o nome: "
      idJogo <- readLn
      putStr "Digite o novo nome do jogo: "
      novoNome <- getLine
      putStr "\n"

      let resultadoAlteracao = alterarNomeJogo idJogo novoNome banco
      case resultadoAlteracao of
        Nothing -> do
          putStrLn "\nErro: Jogo não encontrado.\n"
          menuPrincipal banco
        Just novoBanco -> do
          putStrLn "Alteração realizada com sucesso!\n"
          menuPrincipal novoBanco

    -- Opção 2: Alterar Preço de Jogo
    "2" -> do
      putStr "\nDigite o ID do jogo que deseja alterar o preço: "
      idJogo <- readLn
      putStr "Digite o novo preço do jogo: "
      novoPrecoStr <- getLine
      putStr "\n"

      case readMaybe novoPrecoStr of
        Nothing -> do
          putStrLn "Erro: Preço inválido. Certifique-se de digitar um número.\n"
          menuPrincipal banco
        Just novoPreco -> do
          let resultadoAlteracao = alterarPrecoJogo idJogo novoPreco banco
          case resultadoAlteracao of
            Nothing -> do
              putStrLn "Erro: Jogo não encontrado.\n"
              menuPrincipal banco
            Just novoBanco -> do
              putStrLn "Alteração realizada com sucesso!\n"
              menuPrincipal novoBanco

    -- Opção 3: Alterar Quantidade de Jogo
    "3" -> do
      putStr "\nDigite o ID do jogo que deseja alterar a quantidade: "
      idJogo <- readLn
      putStr "Digite a nova quantidade do jogo: "
      novaQuantidade <- getLine
      putStr "\n"

      case readMaybe novaQuantidade of
        Nothing -> do
          putStrLn "Erro: Quantidade inválida. Certifique-se de digitar uma quantidade válida.\n"
          menuPrincipal banco
        Just novaQuantidadeValue -> do
          let resultadoAlteracao = alterarQtdJogo idJogo novaQuantidadeValue banco
          case resultadoAlteracao of
            Nothing -> do
              putStrLn "Erro: Jogo não encontrado.\n"
              menuPrincipal banco
            Just novoBanco -> do
              putStrLn "Alteração realizada com sucesso!\n"
              menuPrincipal novoBanco

    -- Opção 4: Alterar Nome de Cliente
    "4" -> do
      putStr "\nDigite o ID do cliente que deseja alterar o nome: "
      idCliente <- readLn
      putStr "Digite o novo nome do cliente: "
      novoNome <- getLine
      putStr "\n"

      let resultadoAlteracao = alterarNomeCliente idCliente novoNome banco
      case resultadoAlteracao of
        Nothing -> do
          putStrLn "Erro: Cliente não encontrado.\n"
          menuPrincipal banco
        Just novoBanco -> do
          putStrLn "Alteração realizada com sucesso!\n"
          menuPrincipal novoBanco

    -- Opção 5: Alterar Telefone de Cliente
    "5" -> do
      putStr "\nDigite o ID do cliente que deseja alterar o telefone: "
      idCliente <- readLn
      putStr "Digite o novo telefone do cliente: "
      novoTelefone <- getLine
      putStr "\n"

      let resultadoAlteracao = alterarTelefoneCliente idCliente novoTelefone banco
      case resultadoAlteracao of
        Nothing -> do
          putStrLn "Erro: Cliente não encontrado.\n"
          menuPrincipal banco
        Just novoBanco -> do
          putStrLn "Alteração realizada com sucesso!\n"
          menuPrincipal novoBanco

    -- Opção 6: Alterar Endereço de Cliente
    "6" -> do
      putStr "\nDigite o ID do cliente que deseja alterar o endereço: "
      idCliente <- readLn
      putStr "Digite o novo endereço do cliente: "
      novoEndereco <- getLine
      putStr "\n"

      let resultadoAlteracao = alterarEnderecoCliente idCliente novoEndereco banco
      case resultadoAlteracao of
        Nothing -> do
          putStrLn "Erro: Cliente não encontrado.\n"
          menuPrincipal banco
        Just novoBanco -> do
          putStrLn "Alteração realizada com sucesso!\n"
          menuPrincipal novoBanco

    -- Opção 7: Voltar para o menuPrincipal
    "7" -> do
      putStrLn "\nVoltando..."
      menuPrincipal banco
    _ -> do
      putStrLn "\nOpção inválida. Tente novamente."
      menuAtualizar banco

-- Menu Consultar
-- Esta função exibe o menu de consulta e lê a opção do usuário. Dependendo da opção escolhida pelo usuário, a função solicita as informações necessárias do usuário e chama a função correspondente para listar clientes, listar jogos, mostrar locações, buscar cliente por ID, buscar jogo por ID, buscar locação por ID, buscar cliente por CPF, buscar jogo por título ou voltar para o menu principal.
menuConsultar :: Banco -> IO ()
menuConsultar banco = do
  putStr "\n(CONSULTAR)\n\n"
  putStrLn " 1) Listar Clientes"
  putStrLn " 2) Listar Jogos"
  putStrLn " 3) Mostrar locações"
  putStrLn " 4) Buscar Cliente por ID"
  putStrLn " 5) Buscar Jogo por ID"
  putStrLn " 6) Buscar Locação por ID"
  putStrLn " 7) Buscar Cliente por Cpf"
  putStrLn " 8) Buscar Jogo por Titulo"
  putStrLn " 9) Voltar para o Menu Principal"

  putStr "\nEscolha uma opção: "
  hFlush stdout
  opcao <- getLine

  case opcao of
    -- Opção 1: Listar Clientes
    "1" -> do
      putStr "\n(Lista De Clientes)\n"
      putStrLn $ listarClientes banco
      menuPrincipal banco

    -- Opção 2: Listar Jogos
    "2" -> do
      putStr "\n(Lista De Jogos)\n"
      putStrLn $ listarJogos banco
      menuPrincipal banco

    -- Opção 3: Mostrar locações
    "3" -> do
      putStr "\n(Locações)\n"
      putStrLn $ listarLocacoes banco
      menuPrincipal banco

    -- Opção 4: Buscar Cliente por ID
    "4" -> do
      putStr "\nDigite o ID do cliente: "
      id <- readLn
      case buscarClientePorId id banco of
        Nothing -> putStrLn "\nErro: Cliente não encontrado.\n"
        Just cliente -> putStrLn $ formatarCliente cliente
      menuPrincipal banco
      where
        formatarCliente cliente =
          "\nId: "
            ++ show (getIdC cliente)
            ++ "\n"
            ++ "Nome: "
            ++ getNome cliente
            ++ "\n"
            ++ "Telefone: "
            ++ getTelefone cliente
            ++ "\n"
            ++ "Endereço: "
            ++ getEndereco cliente
            ++ "\n"

    -- Opção 5: Buscar Jogo por ID
    "5" -> do
      putStr "\nDigite o ID do jogo: "
      id <- readLn
      case buscarJogoPorId id banco of
        Nothing -> putStrLn "\nErro: Jogo não encontrado.\n"
        Just jogo -> putStrLn $ formatarJogo jogo
      menuPrincipal banco
      where
        formatarJogo jogo =
          "\nId: "
            ++ show (getIdJ jogo)
            ++ "\n"
            ++ "Título: "
            ++ getTitulo jogo
            ++ "\n"
            ++ "Plataforma: "
            ++ getPlataforma jogo
            ++ "\n"
            ++ "Categoria: "
            ++ getCategoria jogo
            ++ "\n"
            ++ "Preço: "
            ++ show (getPreco jogo)
            ++ "\n"
            ++ "Quantidade Disponível: "
            ++ show (getQtdDisponivel jogo)
            ++ "\n"

    -- Opção 6: Buscar Locação por ID
    "6" -> do
      putStr "\nDigite o ID da locação: "
      id <- readLn
      case buscarLocacaoPorId id banco of
        Nothing -> putStrLn "\nErro: Locação não encontrada.\n"
        Just locacao -> putStrLn $ formatarLocacao locacao
      menuPrincipal banco
      where
        formatarLocacao locacao =
          "\nId da Locação: "
            ++ show (getIdL locacao)
            ++ "\n"
            ++ "Id do Cliente: "
            ++ show (getIdC (getCliente locacao))
            ++ "\n"
            ++ "Nome do Cliente: "
            ++ getNome (getCliente locacao)
            ++ "\n"
            ++ "Id do Jogo: "
            ++ show (getIdJ (getJogo locacao))
            ++ "\n"
            ++ "Nome do Jogo: "
            ++ getTitulo (getJogo locacao)
            ++ "\n"
            ++ "Preço do Jogo: "
            ++ show (getPreco (getJogo locacao))
            ++ "\n"

    -- Opção 7: Buscar Cliente por Cpf
    "7" -> do
      putStr "\nDigite o cpf do cliente que deseja buscar: "
      cpf <- getLine
      putStr "\n"
      putStrLn $ buscarClientePorCpf cpf banco
      menuPrincipal banco

    -- Opção 8: Buscar Jogo por Titulo
    "8" -> do
      putStr "\nDigite o título do jogo que deseja buscar: "
      titulo <- getLine
      putStr "\n"
      putStrLn $ buscarJogoPorTitulo titulo banco
      menuPrincipal banco

    -- Opção 9: Voltar para o menuPrincipal
    "9" -> do
      putStrLn "\nVoltando..."
      menuPrincipal banco
    _ -> do
      putStrLn "\nOpção inválida. Tente novamente."
      menuConsultar banco

-- Menu Deletar
-- Esta função exibe o menu de remoção e lê a opção do usuário. Dependendo da opção escolhida pelo usuário, a função solicita as informações necessárias do usuário e chama a função correspondente para remover cliente por ID, remover jogo por ID, desalocar jogo ou voltar para o menu principal.
menuDeletar :: Banco -> IO ()
menuDeletar banco = do
  putStr "\n(Remoção)\n\n"
  putStrLn " 1) Remover Cliente por ID"
  putStrLn " 2) Remover Jogo por ID"
  putStrLn " 3) Desalocar Jogo"
  putStrLn " 4) Voltar para o Menu Principal"

  putStr "\nEscolha uma opção: "
  hFlush stdout
  opcao <- getLine

  case opcao of
    -- Opção 1: Remover Cliente
    "1" -> do
      putStr "\nDigite o ID do cliente que deseja remover: "
      hFlush stdout
      id <- readLn
      let delecaoAlteracao = removerCliente id banco
      case delecaoAlteracao of
        Nothing -> do
          putStrLn "\nErro: Cliente não encontrado\n"
          menuPrincipal banco
        Just novoBanco -> do
          putStrLn "\nRemoção realizada com sucesso!\n"
          menuPrincipal novoBanco

    -- Opção 2: Remover Jogo
    "2" -> do
      putStr "\nDigite o ID do jogo que deseja remover: "
      hFlush stdout
      id <- readLn
      let delecaoAlteracao = removerJogo id banco
      case delecaoAlteracao of
        Nothing -> do
          putStr "\nErro: Jogo não encontrado\n"
          menuPrincipal banco
        Just novoBanco -> do
          putStrLn "\nRemoção realizada com sucesso!\n"
          menuPrincipal novoBanco

    -- Opção 3: Desalocar Jogo
    "3" -> do
      putStr "\nDigite o ID da locação que deseja desalocar: "
      idLocacao <- readLn
      case desalocarJogo idLocacao banco of
        Right novoBanco -> do
          putStrLn "\nJogo desalocado com sucesso!\n"
          menuPrincipal novoBanco
        Left mensagem -> do
          putStrLn mensagem
          menuPrincipal banco

    -- Opção 4: Voltar para o menuPrincipal
    "4" -> do
      putStrLn "\nVoltando..."
      menuPrincipal banco
    _ -> do
      putStrLn "\nOpção inválida. Tente novamente."
      menuDeletar banco
--FIM