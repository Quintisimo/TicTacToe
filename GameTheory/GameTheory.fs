namespace QUT

    module GameTheory =

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =
            // Basic MiniMax algorithm without using alpha beta pruning
            let rec MiniMax (game: 'Game) (perspective: 'Player) =
                NodeCounter.Increment()
                let over = gameOver game

                if over then
                    let score = heuristic game perspective
                    (None, score)
                else
                    let moves = moveGenerator game
                    let gameStates = Seq.map (fun move -> applyMove game move) moves
                    let tuples = Seq.map (fun gameState -> MiniMax gameState perspective) gameStates
                    let scores = Seq.map (fun (move, score) -> score) tuples

                    let nextPerspective = getTurn game
                    let idealScore = match (nextPerspective = perspective) with
                                     | true -> Seq.max scores
                                     | false -> Seq.min scores

                    let idealTuple = Seq.find (fun (move, score) -> score = idealScore) tuples
                    let newGameState = Seq.find (fun gameState -> MiniMax gameState perspective = idealTuple) gameStates
                    let idealMove = Seq.tryFind (fun move -> applyMove game move = newGameState) moves
                    let result = MiniMax newGameState perspective
                    (idealMove, idealScore)               
            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax (alpha: int32) (beta: int32) (oldState: 'Game) (perspective: 'Player) =
                NodeCounter.Increment()
                let over = gameOver oldState

                if over then
                    let score = heuristic oldState perspective
                    (None, score)
                else
                    let moves = moveGenerator oldState
                    let gameStates = Seq.map (fun move -> applyMove oldState move) moves
                    let tuples = Seq.map (fun gameState -> MiniMax alpha beta gameState perspective) gameStates
                    let scores = Seq.map (fun (move, score) -> score) tuples

                    let nextPerspective = getTurn oldState

                    if nextPerspective = perspective then
                        let idealScore = Seq.max scores
                        let newAlpha = match (idealScore > alpha) with
                                       | true -> idealScore
                                       | false -> alpha

                        let idealTuple = Seq.find (fun (move, score) -> score = idealScore) tuples
                        let newGameState = Seq.find (fun gameState -> MiniMax newAlpha beta gameState perspective = idealTuple) gameStates
                        let idealMove = Seq.tryFind (fun move -> applyMove oldState move = newGameState) moves

                        if beta <= newAlpha then
                            (idealMove, idealScore)
                        else
                            MiniMax newAlpha beta newGameState perspective

                    else
                        let idealScore = Seq.min scores
                        let newBeta = match (idealScore < beta) with
                                      | true -> idealScore
                                      | false -> beta

                        let idealTuple = Seq.find (fun (move, score) -> score = idealScore) tuples
                        let newGameState = Seq.find (fun gameState -> MiniMax alpha newBeta gameState perspective = idealTuple) gameStates
                        let idealMove = Seq.tryFind (fun move -> applyMove oldState move = newGameState) moves


                        if newBeta <= alpha then
                           (idealMove, idealScore)
                        else
                           MiniMax alpha newBeta newGameState perspective
            NodeCounter.Reset()
            MiniMax
             