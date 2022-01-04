namespace FSAI
open System


module Minimax =
    
    //Returns max or min between bestscore and nodeScore depending on isMaxPlayer value
    let minOrMax isMaxPlayer bestScore nodeScore = 
        if(isMaxPlayer) then max bestScore nodeScore
        else min bestScore nodeScore
    
    //Checks AlphaBeta
    let shouldBreak isMaxPlayer bestScore a b =
        if(isMaxPlayer) then
            let newA = max bestScore a
            if(b <= newA) then true
            else false
        else
            let newB = min bestScore b
            if(newB <= a) then true
            else false

    //Ruturns lowest or highest int32 value
    let getFakeBestScore isMaxPlayer =
         if(isMaxPlayer) then Int32.MinValue
         else Int32.MaxValue

    let isEmpty (list) = 
        match list with 
        | [] -> true
        | _ -> false

    let newMaxA isMaxPlayer a bestScore = 
        if(isMaxPlayer) then max bestScore a
        else a
    
    let newMinB isMaxPlayer b bestScore = 
         if( not isMaxPlayer) then min bestScore b
         else b   


    

    let rec minimaxAlphaBeta (board:byte[,]) (depth:int) (a:int) (b:int) (tile:Byte) (isMaxPlayer:bool) (getWinner: byte[,] -> byte) (makeMove: byte[,] -> int * int-> byte -> byte[,]) (getValidMoves) (evaluation: byte[,] -> int) (otherTile: byte -> byte) =
        let validMovesResizeArray:ResizeArray<Tuple<int,int>> = getValidMoves board tile 
        let validMoves = validMovesResizeArray.ToArray() |> Array.toList
        let emptyByte:byte = byte 0
        let winnerExist =  (getWinner board) <> emptyByte
        if(depth = 0 || winnerExist ) then evaluation board
        else
            let childBoard:Byte[,] = Array2D.copy board
            let bestScore = getFakeBestScore isMaxPlayer

    

