module TictactoeFunc where

mySplitAndRemoveN liste n = [take n liste , drop (n+1) liste]

getAPieceOfCake liste i n =  take n (drop (i-1) liste)  -- i >index of first element, n > how many elements do you want.
getCol liste r = (liste!!r) :(liste!!(r+3)) : (liste!!(r+6)) : []
getCross1 liste = (liste !! 0) : (liste !! 4) : (liste !! 8) : []
getCross2 liste = (liste !! 2) : (liste !! 4) : (liste !! 6) : []

printBoard bo = do
        putChar (bo!!0)
        putChar (bo!!1)
        putChar (bo!!2)
        putStrLn ("")
        putChar (bo!!3)
        putChar (bo!!4)
        putChar (bo!!5)
        putStrLn ("")
        putChar (bo!!6)
        putChar (bo!!7)
        putChar (bo!!8)
        putStrLn ("")

play bo playerNo satir sutun = 
        let index = (satir-1) * 3 + (sutun - 1)
        in  let splitted = mySplitAndRemoveN bo index
            in (splitted!!0) ++ show(playerNo) ++ (splitted!!1)


winCondition bo 
    | (getAPieceOfCake bo 0 3 == "111") = 1
    | (getAPieceOfCake bo 0 3 == "222") = 2
    | (getAPieceOfCake bo 3 3 == "111") = 1
    | (getAPieceOfCake bo 3 3 == "222") = 2
    | (getAPieceOfCake bo 6 3 == "111") = 1
    | (getAPieceOfCake bo 6 3 == "222") = 2
    | (getCol bo 0 == "111") = 1
    | (getCol bo 0 == "222") = 2
    | (getCol bo 1 == "111") = 1
    | (getCol bo 1 == "222") = 2
    | (getCol bo 2 == "111") = 1
    | (getCol bo 2 == "222") = 2
    | (getCross1 bo == "111") = 1
    | (getCross1 bo == "222") = 2
    | (getCross2 bo == "111") = 1
    | (getCross2 bo == "222") = 2
    | not (any (=='-') bo) = 0
    | otherwise = -1
