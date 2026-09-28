module GameScore (
    Score,
    initScore,
    updateScore,
    readScore,
    resetScore
) where

import System.IO
import GameState
import Data.IORef
import Control.Exception (try, IOException)

type Score = IORef Int

-- Инициализация счёта
initScore :: IO Score
initScore = do
    score <- readScoreFile
    newIORef score

-- Обновление счёта (только для режима PvBot)
updateScore :: Score -> GameState -> IO ()
updateScore scoreRef state = case gameMode state of
    PvBot _ _ -> do
        case gameResult state of
            (Winner (Human PlayerX)) -> modifyScore scoreRef (+ winPoints)
            (Winner _)               -> modifyScore scoreRef (subtract lossPoints)
            Draw                     -> modifyScore scoreRef (+ drawPoints)
            _                        -> return ()
        current <- readScore scoreRef
        writeScoreFile current
    _ -> return ()
  where
    winPoints = case extractDiff (gameMode state) of
        Easy -> 5; Medium -> 25; Hard -> 125
    lossPoints = case extractDiff (gameMode state) of
        Easy -> 125; Medium -> 25; Hard -> 5
    drawPoints = case extractDiff (gameMode state) of
        Easy -> -10; Medium -> 3; Hard -> 10

-- Чтение счёта
readScore :: Score -> IO Int
readScore = readIORef

-- Сброс счёта
resetScore :: Score -> IO ()
resetScore scoreRef = do
    writeIORef scoreRef 0
    writeScoreFile 0

-- Вспомогательные функции
modifyScore :: Score -> (Int -> Int) -> IO ()
modifyScore scoreRef f = atomicModifyIORef' scoreRef (\s -> (f s, ()))

readScoreFile :: IO Int
readScoreFile = do
    -- читаем файл целиком, чтобы он закрылся до записи нового счёта
    result <- try (readFile "score.dat" >>= \s -> length s `seq` return s) :: IO (Either IOException String)
    case result of
        Left _  -> return 0
        Right content -> 
            case reads content of
                [(score, _)] -> return score
                _ -> return 0

writeScoreFile :: Int -> IO ()
writeScoreFile score = do
    result <- try (writeFile "score.dat" (show score)) :: IO (Either IOException ())
    case result of
        Left e -> putStrLn $ "Не удалось сохранить счёт: " ++ show e
        Right _ -> return ()

extractDiff :: GameMode -> Difficulty
extractDiff (PvBot _ dif) = dif
extractDiff _ = Easy