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
                    let scores = Seq.map (fun (_, score) -> score) tuples

                    let nextPerspective = getTurn game
                    let idealScore = match (nextPerspective = perspective) with
                                     | true -> Seq.max scores
                                     | false -> Seq.min scores

                    let idealTuple = Seq.find (fun (_, score) -> score = idealScore) tuples
                    let newGameState = Seq.find (fun gameState -> MiniMax gameState perspective = idealTuple) gameStates
                    let idealMove = Seq.tryFind (fun move -> applyMove game move = newGameState) moves
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
                    let moves = moveGenerator oldState
                    let gameStates = Seq.map (fun move -> applyMove oldState move) moves
                    let tuples = Seq.map (fun gameState -> MiniMax alpha beta gameState perspective) gameStates
                    let scores = Seq.map (fun (_, score) -> score) tuples

                    let nextPerspective = getTurn oldState

                    if nextPerspective = perspective then
                        let idealScore = Seq.max scores
                        let newAlpha = max idealScore alpha

                        if beta <= newAlpha then
                            (None, newAlpha)
                        else
                            let idealTuple = Seq.find (fun (_, score) -> score = idealScore) tuples
                            let newGameState = Seq.find (fun gameState -> MiniMax alpha beta gameState perspective = idealTuple) gameStates
                            let idealMove = Seq.tryFind (fun move -> applyMove oldState move = newGameState) moves
                            (idealMove, newAlpha)

                    else
                        let idealScore = Seq.min scores
                        let newBeta = min idealScore beta
                        if newBeta <= alpha then
                            (None, newBeta)
                        else
                            let idealTuple = Seq.find (fun (_, score) -> score = idealScore) tuples
                            let newGameState = Seq.find (fun gameState -> MiniMax alpha beta gameState perspective = idealTuple) gameStates
                            let idealMove = Seq.tryFind (fun move -> applyMove oldState move = newGameState) moves
                            (idealMove, newBeta)
            NodeCounter.Reset()
            MiniMax
             