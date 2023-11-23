module Main where

---- Link para replit.com: https://replit.com/@christiandoramo/Projeto-Paradigmas-1va#Main.hs
---- Equipe
-- CAIO FONTES DE MELO
-- CARLOS EDUARDO RIBEIRO
-- CHRISTIAN OLIVEIRA DO RAMO
-- GIOVANNI LIMA DO NASCIMENTO

-- TIPOS PRINCIPAIS
data Cliente = Cliente
  { getId1 :: Int,
    getNome :: String,
    getTelefone :: String,
    getEndereco :: String
  }
  deriving (Show)

data Jogo = Jogo
  { getId2 :: Int,
    getTitle :: String,
    getConsole :: String,
    getPrice :: Float,
    getQtd :: Int
  }
  deriving (Show)

data ItemJogo = ItemJogo
  { getId3 :: Int,
    getJogo :: Jogo
  }
  deriving (Show)

data Locacao = Locacao
  { getId4 :: Int,
    getCliente :: Cliente,
    getItemJogo :: ItemJogo,
    getValue :: Float
  }
  deriving (Show)

data Locadora = Locadora
  { getClientes :: [Cliente],
    getJogosDisponiveis :: [Jogo],
    getLocacoes :: [Locacao]
  }
  deriving (Show)

instance Eq Cliente where
  Cliente i1 t1 n1 e1 == Cliente i2 t2 n2 e2 = i1 == i2 && t1 == t2 && n1 == n2 && e1 == e2

type BancoDeJogos = [Jogo]

type BancoDeClientes = [Cliente]

type BancoDeLocacoes = [Locacao]

-- FUNÇES PRINCIPAIS

-- CREATE
-- CADASTRAR JOGO(funciona)
bancoDeJogosInicial :: BancoDeJogos
bancoDeJogosInicial = []

adicionarJogos :: BancoDeJogos -> Jogo -> BancoDeJogos
adicionarJogos bancoDeJogos novoJogo = novoJogo : bancoDeJogos

-- CADASTRAR Cliente(funciona)
bancoDeClientesInicial :: BancoDeClientes
bancoDeClientesInicial = []

adicionarClientes :: BancoDeClientes -> Cliente -> BancoDeClientes
adicionarClientes bancoDeClientes novoCliente = novoCliente : bancoDeClientes

-- CADASTRAR PEDIDO/LOCACAO(funciona)
bancoDeLocacoesInicial :: BancoDeLocacoes
bancoDeLocacoesInicial = []

adicionarLocacoes :: BancoDeLocacoes -> Locacao -> BancoDeLocacoes
adicionarLocacoes bancoDeLocacoes novoLocacao = novoLocacao : bancoDeLocacoes

-- READ
-- LISTAR JOGOS(funciona)
listarJogos :: BancoDeJogos -> [Jogo]
listarJogos = id

-- LISTAR CLIENTES(funciona)
listarClientes :: BancoDeClientes -> [Cliente]
listarClientes = id

-- LISTAR LOCACOES(funciona)
listarLocacoes :: BancoDeLocacoes -> [Locacao]
listarLocacoes = id

-- BUSCAR JOGO POR ID(funciona)
buscarJogoPorId :: BancoDeJogos -> Int -> Jogo
buscarJogoPorId bancoDeJogos id = head (filter (\jogo -> getId2 jogo == id) bancoDeJogos)

-- BUSCAR PEDIDO POR ID(funciona)
buscarLocacaoPorId :: BancoDeLocacoes -> Int -> Locacao
buscarLocacaoPorId bancoDeLocacao id = head (filter (\locacao -> getId4 locacao == id) bancoDeLocacao)

-- BUSCAR CLIENTE POR ID(funciona)
buscarClientePorId :: BancoDeClientes -> Int -> Cliente
buscarClientePorId bancoDeClientes id = head (filter (\cliente -> getId1 cliente == id) bancoDeClientes)

-- UPDATE:
-- ALTERAR NOME JOGO(funciona)
alterarNomeJogo :: BancoDeJogos -> Int -> String -> BancoDeJogos
alterarNomeJogo bancoDeJogos id novoNome = map (\jogo -> if getId2 jogo == id then jogo {getTitle = novoNome} else jogo) bancoDeJogos

-- ALTERAR PREÇO JOGO(funciona)
alterarPrecoJogo :: BancoDeJogos -> Int -> Float -> BancoDeJogos
alterarPrecoJogo bancoDeJogos id novoPreco = map (\jogo -> if getId2 jogo == id then jogo {getPrice = novoPreco} else jogo) bancoDeJogos

-- ALTERAR QTE JOGO(funciona)
alterarQtdJogo :: BancoDeJogos -> Int -> Int -> BancoDeJogos
alterarQtdJogo bancoDeJogos id novaQtd = map (\jogo -> if getId2 jogo == id then jogo {getQtd = novaQtd} else jogo) bancoDeJogos

-- DESTROY

--- DELETAR CLIENTE POR NOME(funciona)
removerCliente :: BancoDeClientes -> String -> BancoDeClientes
removerCliente [] _ = []
removerCliente (cliente : clientes) nomeRemover
  | getNome cliente == nomeRemover = clientes
  | otherwise = cliente : removerCliente clientes nomeRemover

-- DELETAR JOGO POR NOME(funciona)
removerJogo :: BancoDeJogos -> String -> BancoDeJogos
removerJogo [] _ = []
removerJogo (jogo : bancoDeJogos) tituloRemover
  | getTitle jogo == tituloRemover = bancoDeJogos
  | otherwise = jogo : removerJogo bancoDeJogos tituloRemover

-- DELETAR LOCACAO POR ID(funciona)
removerLocacaoPorId :: Int -> BancoDeLocacoes -> BancoDeLocacoes
removerLocacaoPorId _ [] = []
removerLocacaoPorId idRemover (locacao : restoLocacoes)
  | getId4 locacao == idRemover = restoLocacoes
  | otherwise = locacao : removerLocacaoPorId idRemover restoLocacoes

-- DELETAR JOGO POR ID(funciona)
deletarJogoPorId :: Int -> [Jogo] -> [Jogo]
deletarJogoPorId idToDelete jogos = filter (\jogo -> getId2 jogo /= idToDelete) jogos

-- Função para realizar o cálculo do preço de um jogo(funciona)
precoDiario :: Jogo -> Float
precoDiario jogo = getPrice jogo

-- Função para realizar uma transação de aluguel(funciona)
realizarAluguel :: Locadora -> Cliente -> Jogo -> Float -> Locadora
realizarAluguel locadora cliente jogo valor
  | getQtd jogo > 0 =
      let novoJogoDisponivel = filter (\j -> getId2 j /= getId2 jogo) (getJogosDisponiveis locadora)
          novoItemJogo = ItemJogo (getId2 jogo) jogo
          novaLocacao = Locacao (length (getLocacoes locadora) + 1) cliente novoItemJogo valor
       in Locadora (getClientes locadora) novoJogoDisponivel (novaLocacao : getLocacoes locadora)
  | otherwise = locadora

-- Função para obter a lista de jogos disponíveis na locadora(funciona)
verJogosDisponiveis :: Locadora -> [Jogo]
verJogosDisponiveis locadora = undefined

-- Função para obter a quantidade total de exemplares de um jogo(funciona)
qtdExemplares :: Locadora -> Jogo -> Int
qtdExemplares locadora jogo = undefined

-- FUNÇÃO PARA VISUALIZAR LOCAÇÕES DE UM CLIENTE(funciona)
verLocacoesCliente :: Cliente -> [Locacao] -> [Locacao]
verLocacoesCliente cliente = filter (\locacao -> getCliente locacao == cliente)

-- O restante do código Main
main :: IO ()
main = do
  putStrLn "Primeira entrega"