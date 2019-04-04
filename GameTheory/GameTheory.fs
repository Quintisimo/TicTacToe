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
                    let stateAndMove = List.map (fun move -> (applyMove game move, move)) moves
                    let tuples = List.map (fun (gameState, move) -> MiniMax gameState perspective) stateAndMove
                    let scores = List.map (fun (_, score) -> score) tuples

                    let nextPerspective = getTurn game
                    let idealScore = match (nextPerspective = perspective) with
                                     | true -> List.max scores
                                     | false -> List.min scores

                    let ideal = List.findIndex (fun (_, score) -> score = idealScore) tuples
                    let idealScore = scores.[ideal]
                    let (_, idealMove) = stateAndMove.[ideal]
                    (Some idealMove, idealScore)               
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
                    let stateAndMove = List.map (fun move -> (applyMove oldState move, move)) moves
                    let tuples = List.map (fun (gameState, move)  -> MiniMax alpha beta gameState perspective) stateAndMove
                    let scores = List.map (fun (_, score) -> score) tuples

                    let nextPerspective = getTurn oldState

                    if nextPerspective = perspective then
                        let idealScore = List.max scores
                        let newAlpha = max idealScore alpha

                        if beta <= newAlpha then
                            (None, newAlpha)
                        else
                            let idealTuple = List.findIndex (fun (_, score) -> score = idealScore) tuples
                            let idealScore = scores.[idealTuple]
                            let (_, idealMove) = stateAndMove.[idealTuple]
                            (Some idealMove, newAlpha)

                    else
                        let idealScore = List.min scores
                        let newBeta = min idealScore beta
                        if newBeta <= alpha then
                            (None, newBeta)
                        else
                            let ideal = List.findIndex (fun (_, score) -> score = idealScore) tuples
                            let idealScore = scores.[ideal]
                            let (_, idealMove) = stateAndMove.[ideal]
                            (Some idealMove, newBeta)
            NodeCounter.Reset()
            MiniMax
             