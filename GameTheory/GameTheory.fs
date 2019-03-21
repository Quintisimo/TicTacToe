namespace QUT

    module GameTheory =

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =
            // Basic MiniMax algorithm without using alpha beta pruning
            let rec MiniMax (game: 'Game) (perspective: 'Player) =
                NodeCounter.Increment()
                let over = gameOver game
                let player = getTurn game

                if over then
                    let score = heuristic game player
                    (None, score)
                else
                    let possibleMoves = moveGenerator game 
                    let idealMove = match (NodeCounter.Count % 2) with
                                    | 0 -> Seq.max possibleMoves
                                    | _ -> Seq.min possibleMoves

                    let newGameState = applyMove game idealMove
                    let newPerspective = getTurn newGameState
                    let over = gameOver newGameState

                    if over then
                        let score = heuristic newGameState newPerspective
                        (Some idealMove, score)
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
             