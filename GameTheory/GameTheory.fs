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
                    let moves = Seq.toList (moveGenerator game)
                    let gameStates = List.map (fun move -> applyMove game move) moves
                    let tuples = List.map (fun gameState -> MiniMax gameState perspective) gameStates
                    let scores = List.map (fun (_, score) -> score) tuples

                    let nextPerspective = getTurn game
                    let idealScore = match (nextPerspective = perspective) with
                                     | true -> List.max scores
                                     | false -> List.min scores

                    let idealTuple = List.find (fun (_, score) -> score = idealScore) tuples
                    let newGameState = List.find (fun gameState -> MiniMax gameState perspective = idealTuple) gameStates
                    let idealMove = List.tryFind (fun move -> applyMove game move = newGameState) moves
                    (idealMove, idealScore)               
            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax (alpha: int) (beta: int) (oldState: 'Game) (perspective: 'Player) =
                NodeCounter.Increment()
                let over = gameOver oldState

                if over then
                    let score = heuristic oldState perspective
                    (None, score)
                else
                    let moves = Seq.toList (moveGenerator oldState)
                    let gameStates = List.map (fun move -> applyMove oldState move) moves
                    let tuples = List.map (fun gameState -> MiniMax alpha beta gameState perspective) gameStates
                    let scores = List.map (fun (_, score) -> score) tuples

                    let nextPerspective = getTurn oldState

                    if nextPerspective = perspective then
                        let idealScore = List.max scores
                        let newAlpha = max idealScore alpha

                        if beta <= newAlpha then
                            (None, newAlpha)
                        else
                            let idealTuple = List.find (fun (_, score) -> score = idealScore) tuples
                            let newGameState = List.find (fun gameState -> MiniMax alpha beta gameState perspective = idealTuple) gameStates
                            let idealMove = List.tryFind (fun move -> applyMove oldState move = newGameState) moves
                            (idealMove, newAlpha)

                    else
                        let idealScore = List.min scores
                        let newBeta = min idealScore beta
                        if newBeta <= alpha then
                            (None, newBeta)
                        else
                            let idealTuple = List.find (fun (_, score) -> score = idealScore) tuples
                            let newGameState = List.find (fun gameState -> MiniMax alpha beta gameState perspective = idealTuple) gameStates
                            let idealMove = List.tryFind (fun move -> applyMove oldState move = newGameState) moves
                            (idealMove, newBeta)
            NodeCounter.Reset()
            MiniMax
             