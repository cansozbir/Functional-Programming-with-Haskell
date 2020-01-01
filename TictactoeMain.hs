module TictactoeMain where
import TictactoeFunc

board = ['-', '-', '-',
         '-', '-', '-',
         '-', '-', '-']


main bo player = do
    putStrLn ("Player: " ++ show(player) ++ " Satir giriniz: ")
    satir <- getLine
    putStrLn ("Player: " ++ show(player) ++ " Sutun giriniz: ")
    sutun <- getLine
    printBoard (play bo player (read satir::Int) (read sutun::Int))
    if (winCondition (play bo player (read satir::Int) (read sutun::Int))) == 1
        then putStrLn ("Player 1 KAZANDI")
        else if (winCondition (play bo player (read satir::Int) (read sutun::Int))) == 2
            then putStrLn ("Player 2 KAZANDI")
            else if (winCondition (play bo player (read satir::Int) (read sutun::Int))) == 0
                then putStrLn("BERABERE")
                else if player == 1
                    then main (play bo player (read satir::Int) (read sutun::Int)) 2
                    else main (play bo player (read satir::Int) (read sutun::Int)) 1

start = do
    main board 1


-- Baslatmak icin:
-- Terminalde ghci calistirdiktan sonra
-- Prelude> :load TictactoeMain.hs
-- *TictactoeMain> start

-- Oyunda secilen hamlenin zaten dolu olup olmadigi kontrol edilmemistir.