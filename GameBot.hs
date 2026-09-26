module GameBot (makeAIMove) where
import GameState
import System.Random (StdGen, randomR, newStdGen)
import Data.List (find)
import Data.Maybe (mapMaybe)

{-

ход осуществляется по приоритету:

hard:
1) закрыть свой ряд из 3 фигур
2) заблокировать ряд человека из 3 фигур
3) заблокировать ряд человека из 2 фигур 
4) продолжить строить свой ряд из 2 фигур
5) выбрать один из доступных стратегически выгодных ходов (2,2), (0,0), (0,4), (4,0), (4,4), (0,2), (4,2), (2,0), (2,4)
6) закрыть потенциальный выигрышный ряд человека с одной стороны от его фигуры
7) продолжить строить свой ряд относительно своей стоящей фигуры
8) рандомный ход

medium:
1) закрыть свой ряд из 3 фигур
2) заблокировать ряд человека из 3 фигур
3) заблокировать ряд человека из 2 фигур 
4) рандомный ход

easy:
1) закрыть свой ряд из 3 фигур
2) рандомный ход

-}

----------------------------------------------------------------------------------------------------------------------
-- ОСНОВНЫЕ ФУНКЦИИ

-- сделать ход с определенной сложностью
makeAIMove :: StdGen -> GameState -> (GameState, StdGen)
makeAIMove gen state
    | dif == Easy = makeAIMoveEasy gen state
    | dif == Medium = makeAIMoveMedium gen state
    | otherwise = makeAIMoveHard gen state
  where
    dif = extD (gameMode state)

makeAIMoveHard :: StdGen -> GameState -> (GameState, StdGen)
makeAIMoveHard gen state = (newState, newGen)
  where
    board' = board state
    emptyCells = getEmptyCells board'
    
    -- определяем фигуры игрока и бота 
    currentCell = playerToCell (currentPlayer state)
    opponentCell = playerToCell (nextPlayer (currentPlayer state))
    
    -- ищем выигрышную позицию для бота (завершить ряд из 3 фигур)
    botWinningMove3 = findNInARow board' currentCell 4
    -- поставить напротив 2 в ряд бота
    botWinningMove2 = findNInARow board' currentCell 3
    -- поставить напротив фигуры бота
    botWinningMove1 = findNInARow board' currentCell 2
    
    -- ищем выигрышную позицию для человека 
    humanWinningMove3 = findNInARow board' opponentCell 4
    -- закрыть 2 в ряд человека 
    humanWinningMove2 = findNInARow board' opponentCell 3
    -- поставить напротив фигуры человека 
    humanWinningMove1 = findNInARow board' opponentCell 2
    
    -- выбираем ход по приоритетам
    (chosenMove, newGen) = case botWinningMove3 of
      Just move -> (move, gen)
      Nothing -> case humanWinningMove3 of
        Just move -> (move, gen)
        Nothing -> case humanWinningMove2 of
          Just move -> (move, gen)
          Nothing -> case botWinningMove2 of 
            Just move -> (move, gen)
            Nothing -> case chooseMove board' of
              Just move -> (move, gen)
              Nothing -> case humanWinningMove1 of
                Just move -> (move, gen)
                Nothing -> case botWinningMove1 of 
                  Just move -> (move, gen)
                  Nothing -> randomChoice gen emptyCells
    
    -- ходим
    newState = makeMove state chosenMove

makeAIMoveMedium :: StdGen -> GameState -> (GameState, StdGen)
makeAIMoveMedium gen state = (newState, newGen)
  where
    board' = board state
    emptyCells = getEmptyCells board'
    
    -- определяем фигуры игрока и бота 
    currentCell = playerToCell (currentPlayer state)
    opponentCell = playerToCell (nextPlayer (currentPlayer state))
    
    -- ищем выигрышную позицию для бота
    botWinningMove3 = findNInARow board' currentCell 4

    -- ищем выигрышные позиции для человека 
    humanWinningMove3 = findNInARow board' opponentCell 4
    humanWinningMove2 = findNInARow board' opponentCell 3

    -- выбираем ход
    (chosenMove, newGen) = case botWinningMove3 of
      Just move -> (move, gen)
      Nothing -> case humanWinningMove3 of 
        Just move -> (move, gen)
        Nothing -> case humanWinningMove2 of
          Just move -> (move, gen)
          Nothing -> randomChoice gen emptyCells
    
    -- ходим
    newState = makeMove state chosenMove

makeAIMoveEasy :: StdGen -> GameState -> (GameState, StdGen)
makeAIMoveEasy gen state = (newState, newGen)
  where
    board' = board state
    emptyCells = getEmptyCells board'
    
    -- определяем фигуру бота 
    currentCell = playerToCell (currentPlayer state)
    
    -- ищем выигрышную позицию для бота
    botWinningMove3 = findNInARow board' currentCell 4

    -- выбираем ход
    (chosenMove, newGen) = case botWinningMove3 of
      Just move -> (move, gen)
      Nothing -> randomChoice gen emptyCells
    -- ходим
    newState = makeMove state chosenMove

----------------------------------------------------------------------------------------------------------------------
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ

-- выбор потенциально выгодных ходов, если более выгодных (продолжить уже имеющийся ряд) нет
chooseMove :: Board -> Maybe (Int, Int)
chooseMove board = case moves of
  [] -> Nothing
  (move:_) -> Just move
  where emptyCells = getEmptyCells board
        moves = foldl (\acc (x, y) -> if (x, y) `elem` emptyCells then acc ++ [(x, y)] else acc)
                      [] [(2,2),(0,0),(0,4),(4,0),(4,4),(0,2),(4,2),(2,0),(2,4)]

-- определить фигуры каждого игрока 
playerToCell :: Player -> Cell
playerToCell PlayerX = X
playerToCell PlayerO = O

-- получить список всех пустых клеток 
getEmptyCells :: Board -> [(Int, Int)]
getEmptyCells board = 
  [(x, y) | x <- [0..4], y <- [0..4], (board !! x) !! y == Empty]

-- выбрать случайную из доступных клеток
randomChoice :: StdGen -> [(Int, Int)] -> ((Int, Int), StdGen)
randomChoice gen moves = (moves !! index, newGen)
  where
    (index, newGen) = randomR (0, length moves - 1) gen

-- найти ряд, где есть заданное количество фигур + место для еще одной
findNInARow :: Board -> Cell -> Int -> Maybe (Int, Int)
findNInARow board cell windowSize =
  case mapMaybe (checkLine board cell windowSize) getAllLines of
    [] -> Nothing
    (move:_) -> Just move

-- получить все линии на доске 5x5 (горизонтали, вертикали, диагонали) - список списков координат
-- короткие линии отсеиваются в checkLine
getAllLines :: [[(Int, Int)]]
getAllLines =
  -- горизонтали
  [[(i, j) | j <- [0..4]] | i <- [0..4]] ++

  -- вертикали
  [[(i, j) | i <- [0..4]] | j <- [0..4]] ++

  -- главные диагонали: у всех клеток одинаковая разность i - j
  [[(i, j) | i <- [0..4], j <- [0..4], i - j == d] | d <- [-4..4]] ++

  -- антидиагонали: у всех клеток одинаковая сумма i + j
  [[(i, j) | i <- [0..4], j <- [0..4], i + j == s] | s <- [0..8]]

{-

проверяет, содержит ли заданная последовательность координат выигрышную комбинацию для фигуры:
функция разбивает последовательность на все возможные окна размера windowSize и
возвращает координаты пустой клетки, если в окне ровно (windowSize - 1) клеток содержат указанную фигуру и ровно одна клетка пуста;
иначе возвращает Nothing

-}
checkLine :: Board -> Cell -> Int -> [(Int, Int)] -> Maybe (Int, Int)
checkLine board cell windowSize coords
  | length coords < windowSize = Nothing
  | otherwise = findWinningMove (windows coords windowSize) windowSize
  where
    -- разбиваем список координат на окна, т.е. последовательности из windowSize клеток
    windows :: [(Int, Int)] -> Int -> [[(Int, Int)]]
    windows lstCoords windowSize
      | length lstCoords < windowSize = []
      | otherwise = take windowSize lstCoords : windows (drop 1 lstCoords) windowSize

    -- проходим по всем окнам, ищем первое выигрышное
    findWinningMove :: [[(Int, Int)]] -> Int -> Maybe (Int, Int)
    findWinningMove [] _ = Nothing
    findWinningMove (w:ws) windowSize =
      case winningMoveInWindow w windowSize of
        Just pos -> Just pos
        Nothing -> findWinningMove ws windowSize

    -- проверяем конкретное окно "на выигрышность"
    winningMoveInWindow :: [(Int, Int)] -> Int -> Maybe (Int, Int)
    winningMoveInWindow window windowSize =
      let windowCells = map (\(x, y) -> board !! x !! y) window
          countCell = length $ filter (== cell) windowCells
          countEmpty = length $ filter (== Empty) windowCells
      in if countCell == (windowSize - 1) && countEmpty == 1
         then find (\(x, y) -> board !! x !! y == Empty) window
         else Nothing

-- извлечь сложность бота 
extD :: GameMode -> Difficulty
extD (PvBot player difficulty) = difficulty
