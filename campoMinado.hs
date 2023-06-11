import System.Random (randomRIO)
import Data.Char (toUpper)
import Data.List (intercalate)

type Position = (Int, Int)
type Board = [[Char]]

data GameState = GameState
  { board :: Board
  , bombs :: [Position]
  , opened :: [Position]
  , marked :: [Position]
  }

-- Função principal para iniciar o jogo
main :: IO ()
main = do
  putStrLn "Bem-vindo ao Campo Minado!"
  size <- getBoardSize
  numBombs <- getNumBombs size
  gameState <- initializeGameState size numBombs
  playGame gameState

-- Lê o tamanho do tabuleiro
getBoardSize :: IO Int
getBoardSize = do
  putStrLn "Digite o tamanho do tabuleiro:"
  size <- readLn
  return size

-- Lê o número de bombas
getNumBombs :: Int -> IO Int
getNumBombs size = do
  putStrLn "Digite o número de bombas:"
  numBombs <- readLn
  return (clamp 1 (size*size `div` 2) numBombs)

-- Inicializa o estado do jogo
initializeGameState :: Int -> Int -> IO GameState
initializeGameState size numBombs = do
  bombPositions <- generateBombPositions size numBombs
  let initialBoard = replicate size (replicate size '*')
  return GameState
    { board = initialBoard
    , bombs = bombPositions
    , opened = []
    , marked = []
    }

-- Gera posições aleatórias para as bombas
generateBombPositions :: Int -> Int -> IO [Position]
generateBombPositions size numBombs = do
  positions <- sequence (replicate numBombs (generatePosition size))
  return positions

-- Gera uma posição aleatória no tabuleiro
generatePosition :: Int -> IO Position
generatePosition size = do
  row <- randomRIO (0, size - 1)
  col <- randomRIO (0, size - 1)
  return (row, col)

-- Função principal para jogar o jogo
playGame :: GameState -> IO ()
playGame gameState = do
  putStrLn $ renderBoard (board gameState)
  command <- getCommand
  let newState = executeCommand gameState command
  case checkGameOver newState of
    Just "Game Over! Você foi explodido!" -> putStrLn $ showBoardWithBombs newState
    Just "Parabéns! Você venceu!" -> putStrLn $ renderBoard (board newState)
    Nothing -> playGame newState

-- Lê o comando do jogador
getCommand :: IO String
getCommand = do
  putStrLn "Digite um comando (posição para abrir, +posição para marcar, -posição para desmarcar):"
  getLine

-- Executa o comando do jogador e retorna o novo estado do jogo
executeCommand :: GameState -> String -> GameState
executeCommand gameState command =
  case parseCommand command of
    Just (Open pos) -> openCell pos gameState
    Just (Mark pos) -> markCell pos gameState
    Just (Unmark pos) -> unmarkCell pos gameState
    Nothing -> gameState -- Comando inválido, retorna o estado atual do jogo

-- Analisa e retorna o tipo de comando
parseCommand :: String -> Maybe Command
parseCommand command =
  case command of
    '+' : posStr -> Just (Mark (parsePosition posStr))
    '-' : posStr -> Just (Unmark (parsePosition posStr))
    _ -> Just (Open (parsePosition command))

-- Analisa e retorna uma posição válida
parsePosition :: String -> Position
parsePosition posStr = (row, col)
  where
    row = read [toUpper (posStr !! 0)] - 65 -- 'A' = 65
    col = read (drop 1 posStr) - 1

-- Abre uma célula no tabuleiro
openCell :: Position -> GameState -> GameState
openCell pos gameState =
  if isValidPosition pos && not (isOpened pos gameState)
    then gameState { opened = pos : opened gameState }
    else gameState

-- Marca uma célula como bomba
markCell :: Position -> GameState -> GameState
markCell pos gameState =
  if isValidPosition pos && not (isOpened pos gameState) && not (isMarked pos gameState) && length (marked gameState) < length (bombs gameState)
    then gameState { marked = pos : marked gameState }
    else gameState

-- Desmarca uma célula
unmarkCell :: Position -> GameState -> GameState
unmarkCell pos gameState =
  if isValidPosition pos && isMarked pos gameState
    then gameState { marked = filter (/= pos) (marked gameState) }
    else gameState

-- Verifica se uma posição é válida
isValidPosition :: Position -> Bool
isValidPosition (row, col) = row >= 0 && row < size && col >= 0 && col < size
  where
    size = length (board (gameState :: GameState))

-- Verifica se uma célula já foi aberta
isOpened :: Position -> GameState -> Bool
isOpened pos gameState = pos `elem` opened gameState

-- Verifica se uma célula já foi marcada como bomba
isMarked :: Position -> GameState -> Bool
isMarked pos gameState = pos `elem` marked gameState

-- Verifica se o jogo terminou (ou perdeu ou venceu)
checkGameOver :: GameState -> Maybe String
checkGameOver gameState
  | any (`elem` bombs gameState) (opened gameState) = Just "Game Over! Você foi explodido!"
  | length (opened gameState) + length (marked gameState) == size*size && all (`elem` bombs gameState) (marked gameState) = Just "Parabéns! Você venceu!"
  | otherwise = Nothing
  where
    size = length (board gameState)

-- Retorna o tabuleiro como uma string formatada
renderBoard :: Board -> String
renderBoard board = unlines (map unwords board)

-- Retorna o tabuleiro com as bombas reveladas
showBoardWithBombs :: GameState -> String
showBoardWithBombs gameState = renderBoard (map revealBombs (board gameState))
  where
    revealBombs row = [if pos `elem` bombs gameState then 'X' else cell | (pos, cell) <- zip positions row]
    size = length (board gameState)
    positions = [(row, col) | row <- [0..size-1], col <- [0..size-1]]

-- Função auxiliar para limitar um valor dentro de um intervalo
clamp :: Ord a => a -> a -> a -> a
clamp low high = max low . min high
