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
                    let possibleMoves = moveGenerator game
                    let gameStates = Seq.map (fun move -> applyMove game move) possibleMoves
                    let perspectives = Seq.map (fun gameState -> getTurn gameState) gameStates
                    let scores = Seq.map2 (fun gameState perspective -> heuristic gameState perspective) gameStates perspectives
                    let idealScore = match (NodeCounter.Count % 2) with
                                     | 0 -> Seq.max scores
                                     | _ -> Seq.min scores

                    let scoreIndex = Seq.findIndex (fun score -> score = idealScore) scores
                    let newGameState = Seq.item scoreIndex gameStates
                    let newPerspective = Seq.item scoreIndex perspectives
                    let idealMove = Seq.item scoreIndex possibleMoves
                    let over = gameOver newGameState

                    if over then
                        (Some idealMove, idealScore)
                    else
                        MiniMax newGameState newPerspective

            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()
                raise (System.NotImplementedException("Alpha Beta Pruning"))
            NodeCounter.Reset()
            MiniMax
             