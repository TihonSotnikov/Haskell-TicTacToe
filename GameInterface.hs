module GameInterface (
    runGameLoop
) where

import GameState
import GameBot
import GameScore (Score, initScore, updateScore, readScore, resetScore)
import System.IO
import System.Random (newStdGen)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)
import System.Info (os)
import Control.Monad (unless)

-- запуск программы
runGameLoop :: IO ()
runGameLoop = do
    hSetBuffering stdout NoBuffering
    -- при локали C/POSIX вывод кириллицы падает, поэтому явно включаем UTF-8
    unless (os == "mingw32") (hSetEncoding stdout utf8)
    score <- initScore
    menuLoop score ""

-- главное меню; msg - сообщение под меню (пустое, если нечего сказать)
menuLoop :: Score -> String -> IO ()
menuLoop score msg = do
    current <- readScore score
    clearScreen
    putStrLn "=== Крестики-нолики 5x5, 4 в ряд ==="
    putStrLn ("Счёт против бота: " ++ show current)
    putStrLn ""
    putStrLn "1 - Игра вдвоём"
    putStrLn "2 - Против бота (лёгкий)"
    putStrLn "3 - Против бота (средний)"
    putStrLn "4 - Против бота (сложный)"
    putStrLn "5 - Сбросить счёт"
    putStrLn "0 - Выход"
    putStrLn ""
    putStrLn msg
    putStr "> "
    choice <- readInput
    case choice of
        "1" -> playGame score PvP >> menuLoop score ""
        "2" -> playGame score (PvBot PlayerO Easy) >> menuLoop score ""
        "3" -> playGame score (PvBot PlayerO Medium) >> menuLoop score ""
        "4" -> playGame score (PvBot PlayerO Hard) >> menuLoop score ""
        "5" -> resetScore score >> menuLoop score "Счёт сброшен."
        "0" -> clearScreen
        _   -> menuLoop score "Нет такого пункта."

-- одна партия от начала до конца
playGame :: Score -> GameMode -> IO ()
playGame score mode = do
    final <- gameLoop "" (createGame mode)
    updateScore score final
    clearScreen
    printBoard (board final)
    putStrLn ""
    putStrLn (resultText (gameResult final))
    putStr "Нажми Enter, чтобы вернуться в меню."
    _ <- readInput
    return ()

-- ходы по очереди, пока игра не закончится; msg - сообщение под доской
gameLoop :: String -> GameState -> IO GameState
gameLoop msg state
    | gameResult state /= InProgress = return state
    | isBotTurn state = do
        gen <- newStdGen
        let (newState, _) = makeAIMove gen state
        gameLoop (botMoveText (board state) (board newState)) newState
    | otherwise = do
        clearScreen
        printBoard (board state)
        putStrLn ""
        putStrLn msg
        putStr ("Ходит " ++ playerName (currentPlayer state) ++ " (строка столбец): ")
        line <- readInput
        case parseMove state line of
            Just move -> gameLoop "" (makeMove state move)
            Nothing   -> gameLoop "Неверный ход, попробуй ещё раз." state

-- разобрать ввод вида "2 3"; Nothing, если ввод неверный или клетка занята
parseMove :: GameState -> String -> Maybe (Int, Int)
parseMove state line = case map readMaybe (words line) of
    [Just r, Just c] | isValidMove state (r - 1) (c - 1) -> Just (r - 1, c - 1)
    _ -> Nothing

-- найти клетку, куда походил бот, сравнив доску до и после хода
botMoveText :: Board -> Board -> String
botMoveText old new =
    case [(r, c) | r <- [0..4], c <- [0..4], old !! r !! c /= new !! r !! c] of
        ((r, c):_) -> "Бот походил: " ++ show (r + 1) ++ " " ++ show (c + 1)
        []         -> ""

-- очистить экран и поставить курсор в начало
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- чтение строки; если ввод закончился (Ctrl+D) - выходим
readInput :: IO String
readInput = do
    eof <- isEOF
    if eof then putStrLn "" >> exitSuccess else getLine

-- вывод поля с номерами строк и столбцов
printBoard :: Board -> IO ()
printBoard b = do
    putStrLn "  1 2 3 4 5"
    mapM_ printRow (zip [1 :: Int ..] b)
  where
    printRow (i, row) = putStrLn (show i ++ " " ++ unwords (map cellText row))

cellText :: Cell -> String
cellText Empty = "."
cellText X     = "X"
cellText O     = "O"

playerName :: Player -> String
playerName PlayerX = "X"
playerName PlayerO = "O"

resultText :: GameResult -> String
resultText (Winner (Bot _ _))   = "Победил бот!"
resultText (Winner (Human p))   = "Победил " ++ playerName p ++ "!"
resultText Draw                 = "Ничья!"
resultText InProgress           = ""

isBotTurn :: GameState -> Bool
isBotTurn state = case gameMode state of
    PvBot p _ -> currentPlayer state == p
    _ -> False

isValidMove :: GameState -> Int -> Int -> Bool
isValidMove state x y =
    x >= 0 && x < 5 && y >= 0 && y < 5 &&
    (board state !! x !! y) == Empty
