<p align="center">
  Консольные крестики-нолики на Haskell: поле 5x5, победа за 4 в ряд, игра вдвоём или против бота с тремя уровнями сложности.
</p>

<p align="center">
  <img src="https://img.shields.io/badge/Haskell-2010-a8c8f0?style=flat&logo=haskell&logoColor=white" alt="Haskell 2010">
  <img src="https://img.shields.io/badge/License-MIT-d4c8f0?style=flat" alt="License MIT">
</p>

<p align="center">
  <img src="docs/assets/demo.gif" alt="Партия против сложного бота" width="640">
</p>

## Возможности

- Режимы: игра вдвоём за одним терминалом и игра против бота (человек ходит за X, бот за O).
- Проверка победы по горизонталям, вертикалям и обеим диагоналям, определение ничьей при заполненном поле.
- Бот выбирает ход по приоритетам, набор правил зависит от уровня:

| Уровень | Правила |
|---|---|
| Лёгкий | завершить свою тройку, иначе случайный ход |
| Средний | завершить свою тройку, заблокировать тройку и двойку соперника, иначе случайный ход |
| Сложный | правила среднего уровня, продолжение своей двойки, занятие центра, углов и середин краёв, блокировка одиночных фигур соперника |

- Счёт против бота хранится в `score.dat` между запусками. Очки зависят от сложности: победа над лёгким ботом даёт +5, над сложным +125; поражение от лёгкого стоит -125, от сложного -5.

## Установка

Готовые исполняемые файлы для Linux x86_64, macOS arm64 и Windows x86_64 публикуются на странице [Releases](https://github.com/TihonSotnikov/Haskell-TicTacToe/releases). Windows-версия запускается напрямую.

Linux:

```bash
chmod +x tictactoe-linux-x86_64
./tictactoe-linux-x86_64
```

macOS (файл не подписан, поэтому с него снимается карантин):

```bash
xattr -d com.apple.quarantine tictactoe-macos-arm64
chmod +x tictactoe-macos-arm64
./tictactoe-macos-arm64
```

## Сборка

Требуются GHC и cabal-install. Зависимости: `base`, `random`.

```bash
git clone https://github.com/TihonSotnikov/Haskell-TicTacToe.git
cd Haskell-TicTacToe
cabal build
cabal run tictactoe
```

## Использование

Пункт меню выбирается цифрой, ход вводится как номер строки и столбца через пробел, например `3 3`. Выход - пункт `0` или Ctrl+D.

## Структура проекта

| Модуль | Назначение |
|---|---|
| `GameState.hs` | типы данных, ходы, проверка победы и ничьей |
| `GameBot.hs` | логика бота для трёх уровней сложности |
| `GameScore.hs` | подсчёт очков и сохранение счёта в файл |
| `GameInterface.hs` | меню, игровой цикл, вывод поля и разбор ввода |
| `Main.hs` | точка входа |
