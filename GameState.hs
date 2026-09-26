
module GameState (
    GameState(..),
    createGame,
    gameEndCheck,
    makeMove,
    nextPlayer,
    GameMode(..),
    Participant(..),
    GameResult(..),
    Difficulty(..),
    Player(..),
    Board,
    Cell(..),
    getParticipant,
    getParticipantByPlayer,
    hasEmptyCells,
    checkWinner
) where

import Data.List (transpose, any, elem)

-- Определение типа для игрового поля
type Board = [[Cell]]

-- Основные типы данных
data Cell = Empty | X | O
  deriving (Show, Eq)

data Player = PlayerX | PlayerO
  deriving (Show, Eq, Ord)

data Difficulty = Easy | Medium | Hard
  deriving (Show, Eq, Read)

data GameMode = PvP | PvBot { botPlayer :: Player, difficulty :: Difficulty }
  deriving (Show, Eq)

data Participant = Human Player | Bot Player Difficulty
  deriving (Show, Eq)

data GameResult = InProgress | Draw | Winner Participant
  deriving (Show, Eq)

data GameState = GameState { 
    board         :: Board,
    currentPlayer :: Player,
    gameMode      :: GameMode,
    gameResult    :: GameResult,
    participantX  :: Participant,
    participantO  :: Participant
} deriving (Show, Eq)

-- Основные функции игры

-- Создание новой игры
createGame :: GameMode -> GameState
createGame mode = GameState { 
    board         = createBoard,
    currentPlayer = PlayerX,
    gameMode      = mode,
    gameResult    = InProgress,
    participantX  = getParticipant PlayerX mode,
    participantO  = getParticipant PlayerO mode
}

-- Проверка окончания игры
gameEndCheck :: GameState -> GameResult
gameEndCheck state =
  case checkWinner (board state) of
    Just PlayerX -> Winner (participantX state)
    Just PlayerO -> Winner (participantO state)
    Nothing      -> if hasEmptyCells (board state) then InProgress else Draw

-- Выполнение хода игрока
makeMove :: GameState -> (Int, Int) -> GameState
makeMove state (x, y) = 
    let -- Обновляем доску
        currentRow = board state !! x
        currentPlayerCell = if currentPlayer state == PlayerX then X else O
        newRow = take y currentRow ++ [currentPlayerCell] ++ drop (y + 1) currentRow
        newBoard = take x (board state) ++ [newRow] ++ drop (x + 1) (board state)
        
        -- Создаем промежуточное состояние
        intermediateState = state { 
            board = newBoard,
            currentPlayer = nextPlayer (currentPlayer state)
        }
        
        -- Проверяем состояние игры
        finalResult = gameEndCheck intermediateState
    in intermediateState { gameResult = finalResult }

-- Вспомогательные функции

-- Создание пустого поля 5x5
createBoard :: Board
createBoard = replicate 5 (replicate 5 Empty)

-- Смена игрока
nextPlayer :: Player -> Player
nextPlayer PlayerX = PlayerO
nextPlayer PlayerO = PlayerX

-- Получение участника по игроку и режиму
getParticipant :: Player -> GameMode -> Participant
getParticipant player PvP = Human player
getParticipant player (PvBot bot diff) =
  if player == bot then Bot player diff else Human player

-- Проверка наличия пустых клеток
hasEmptyCells :: Board -> Bool
hasEmptyCells = any (elem Empty)

-- Проверка победителя
checkWinner :: Board -> Maybe Player
checkWinner b
  | hasWinningLine X b = Just PlayerX
  | hasWinningLine O b = Just PlayerO
  | otherwise          = Nothing

-- Проверка выигрышной линии
hasWinningLine :: Cell -> Board -> Bool
hasWinningLine cell b =
  any (hasFourInRow cell) (directions b)

-- Проверка 4 в ряд
hasFourInRow :: Cell -> [Cell] -> Bool
hasFourInRow cell line =
  let counts = scanl (\acc c -> if c == cell then acc + 1 else 0) 0 line
  in maximum counts >= 4

-- Все направления для проверки (горизонтали, вертикали, диагонали)
directions :: Board -> [[Cell]]
directions b = horizontals ++ verticals ++ diagonals
  where
    horizontals = b
    verticals   = transpose b
    diagonals   = mainDiagonals b ++ antiDiagonals b

-- Главные диагонали
mainDiagonals :: Board -> [[Cell]]
mainDiagonals b = filter ((>=4) . length) (fromFirstCol ++ fromFirstRow)
  where
    size = length b
    fromFirstCol = [diag b i 0 | i <- [0..(size - 4)]]
    fromFirstRow = [diag b 0 j | j <- [1..(size - 4)]]
    diag b i j = [ b !! (i+k) !! (j+k) | k <- [0..min (size-i-1) (size-j-1)] ]

-- Антидиагонали
antiDiagonals :: Board -> [[Cell]]
antiDiagonals b = filter ((>=4) . length) (fromLastCol ++ fromFirstRow)
  where
    size = length b
    fromLastCol = [antiDiag b i (size - 1) | i <- [0..(size - 4)]]
    fromFirstRow = [antiDiag b 0 j | j <- [3..(size - 1)]]
    antiDiag b i j = [ b !! (i+k) !! (j-k) | k <- [0..min (size-i-1) j] ]

-- Получение участника по игроку
getParticipantByPlayer :: Player -> GameState -> Participant
getParticipantByPlayer PlayerX state = participantX state
getParticipantByPlayer PlayerO state = participantO state
