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


    
    // minimaxAlphaBeta function that returns scoer acording to the minimax alphabeta algorithm 
    let rec minimaxAlphaBeta (board:byte[,]) (depth:int) (a:int) (b:int) (tile:Byte) (isMaxPlayer:bool) (getWinner: byte[,] -> byte) (makeMove: byte[,] -> int * int-> byte -> byte[,]) (getValidMoves) (evaluation: byte[,] -> int) (otherTile: byte -> byte) =
        let validMovesResizeArray:ResizeArray<Tuple<int,int>> = getValidMoves board tile 
        let validMoves = validMovesResizeArray.ToArray() |> Array.toList
        let winnerExist =  (getWinner board) <> byte 0

        if(depth = 0 || winnerExist || validMoves.Length < 1 ) then evaluation board
        else
            let childBoard:Byte[,] = Array2D.copy board
            let bestScore = getFakeBestScore isMaxPlayer
            // look into each valid move and returns the best score 
            let rec bestScoreResult a b childBoard bestScore moves = 
                match moves with
                    |[] -> bestScore
                    |head::tail -> 
                        // gets the nodes best score
                        let nodeScore = minimaxAlphaBeta childBoard (depth-1) a b (otherTile tile) (not isMaxPlayer) getWinner makeMove getValidMoves  evaluation  otherTile 
                        let newBoard = makeMove childBoard head tile 
                        let newA = newMaxA isMaxPlayer nodeScore a 
                        let newB =  newMinB isMaxPlayer nodeScore b
                        let newBestScore = minOrMax  isMaxPlayer bestScore nodeScore
                        
                        if(shouldBreak isMaxPlayer newBestScore newA newB ) then newBestScore
                        else bestScoreResult newA newB newBoard newBestScore tail

            if( validMoves.Length > 0 ) then bestScoreResult a b childBoard bestScore validMoves
            else evaluation board
    

