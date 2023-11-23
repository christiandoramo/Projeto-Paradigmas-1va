module Main where

-- Link para replit.com: [Link aqui]

-- TIPOS PRINCIPAIS
data Cliente = C 
{
  getId::Int,
  getNome :: String,
  getTelefone :: String,
  getEndereco :: String
} deriving (Show)

data Locadora = Ldora
{ 
  getClientes :: [Cliente],
  getJogosDisponiveis :: [Jogo],
  getLocacoes :: [(Cliente, Jogo)]
} deriving (Show)

data Jogo = J {
  getId::Int,
  getTitle::String, 
  getConsole::String, 
  getPrice::Float,
  getQtd::Int
} deriving Show

data ItemJogo = J {
  getId::Int,
  getJogo::Jogo
} deriving Show

data Locacao = Lcao{
  getId::Int,
  getCliente::Cliente,
  getItemJogo::ItemJogo,
  getValue::Float
}

type BancoDeJogos = [Jogo]
let BancoDeJogos = []

type BancoDeClientes = [Cliente]
let BancoDeClientes = []

type BancoDeLocacoes = [Locacao]
let BancoDeLocacoes = []
 
-- FUNÇES PRINCIPAIS
  -- CREATE
    -- CADASTRAR JOGO
adicionarJogo :: BancoDeJogos -> Jogo -> BancoDeJogos
adicionarJogo BancoDeJogos novoJogo = novoJogo : BancoDeJogos
-- CADASTRAR CLIENTE
adicionarClientes :: BancoDeClientes -> Cliente -> BancoDeClientes
adicionarClientes BancoDeClientes novoCliente = novoCliente : BancoDeClientes
-- CADASTRAR PEDIDO/LOCACAO
adicionarLocacoes :: BancoDeLocacoes -> Locacao -> BancoDeLocacoes
adicionarLocacoes BancoDeLocacoes novoLocacao = novoLocacao : BancoDeLocacoes

  -- READ  
    -- LISTAR JOGOS
listarJogos :: BancoDeJogos -> [Jogos] 
    -- LISTAR CLIENTES
listarClientes :: BancoDeClientes -> [String] 
    -- LISTAR LOCACOES
listarClientes :: BancoDeClientes -> [String] 
    -- MOSTRAR LOCADORA
mostrarLocadora :: Locadora -> String
    
buscarJogoPorId :: BancoDeJogos -> Int -> Jogo
    -- BUSCAR CLIENTE POR ID
buscarClientePorId :: BancoDeClientes -> Int -> Cliente
    -- BUSCAR PEDIDO POR ID
buscarLocacaoPorId :: BancoDeLocacao -> Int -> Locacao
    -- BUSCAR PEDIDO POR ID
buscarLocacaoPorId :: BancoDeLocacao -> Int -> Locacao
    
  -- UPDATE:
    -- ALTERAR NOME JOGO
alterarNomeJogo :: BancoDeJogos -> Int -> String -> BancoDeJogos
alterarNomeJogo bancoDeJogos id novoNome = map (\jogo -> if getId jogo == id then jogo { getTitle = novoNome } else jogo) bancoDeJogos
    
    -- ALTERAR PREÇO JOGO

AlterarPrecoJogo :: BancoDeJogos -> Int -> Float -> BancoDeJogos
alterarPrecoJogo bancoDeJogos id novoPreco = map (\jogo -> if getId jogo == id then jogo { getPrice = novoPreco } else jogo) bancoDeJogos
    
    -- ALTERAR QTE JOGO

alterarQtdJogo :: BancoDeJogos -> Int -> Int -> BancoDeJogos 
alterarQtdJogo bancoDeJogos id novaQtd = map (\jogo -> if getId == id then jogo { getQtd == novaQtd} else jogos) bancoDeJogos
  
  -- DESTROY
    -- DELETAR JOGO POR ID
deletarJogoPorId :: Int -> [Jogo] -> [Jogo]
deletarJogoPorId idToDelete jogos = filter (\jogo -> getId jogo /= idToDelete) jogos
--  DELETAR JOGO POR NOME
removerJogo :: BancoDeJogos -> String -> BancoDeJogos
removerJogo _ [] = []
removerJogo (Jogo:BancoDeJogos) tituloRemover
|getTitle Jogo == tituloRemover = BancoDeJogos --ignora o jogo
| otherwise = Jogo:removerJogo BancoDeJogos tituloRemover --remove o jogo

  -- DELETAR CLIENTE POR ID
removerCliente :: BancoDeClientes -> Int -> BancoDeClientes
removerCliente _ [] = []
removerCliente (Cliente:BancoDeClientes) idRemover
|getId Cliente == idRemover = BancoDeClientes --ignora o cliente
| otherwise = Cliente:removerCliente BancoDeClientes idRemover --remove o cliente
  
  -- DELETAR CLIENTE POR NOME
removerCliente :: BancoDeClientes -> String -> BancoDeClientes
removerCliente _ [] = []
removerCliente (Cliente:BancoDeClientes) nomeRemover
|getNome Cliente == nomeRemover = BancoDeClientes --ignora o cliente
| otherwise = Cliente:removerCliente BancoDeClientes nomeRemover --remove o cliente

  -- DELETAR LOCACAO POR ID
removerLocacao :: BancoDeLocacoes -> Int -> BancoDeLocacoes
removerLocacao _ [] = []
removerLocacao (Locacao:BancoDeLocacoes) idRemover
|getId Locacao == idRemover = BancoDeLocacoes
| otherwise = Locacao:removerLocacao BancoDeClientes idRemover

  -- Função para realizar o cálculo do preço de um jogo
precoDiario :: Jogo -> Float

 -- Função para realizar uma transação de aluguel
realizarAluguel :: Locadora -> Cliente -> Jogo -> Float

-- Função para obter a lista de jogos disponíveis na locadora
verJogosDisponiveis :: Locadora -> [Jogo]
verJogosDisponiveis locadora = undefined -- implementação necessária

-- Função para obter a quantidade total de exemplares de um jogo
qtdExemplares :: Locadora -> Jogo -> Int
qtdExemplares locadora jogo = undefined -- implementação necessária

bancoDadosLocadora :: Locadora
bancoDadosLocadora = Locadora pessoasLocadora jogosDisponiveisLocadora emprestimosLocadora

-- FUNÇÃO PARA VISUALIZAR LOCAÇÕES DE UM CLIENTE
verLocacoesCliente :: Cliente -> [Locacao]

-- Exemplo de dados
pessoasLocadora :: [Pessoa]
pessoasLocadora = [P "Leandro" 12345678, P "Joabe" 45678910, P "Lucas" 96874343]

jogosDisponiveisLocadora :: [Jogo]
jogosDisponiveisLocadora = ["FIFA 22", "GTA V", "Cyberpunk 2077"]

-- O restante do código Main
main = do
  putStrLn "Exemplo de código para a Locadora de Jogos em Haskell"