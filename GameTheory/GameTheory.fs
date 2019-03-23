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
                    let over = gameOver newGameState

                    if over then
                        let finalScore = heuristic newGameState perspective
                        let moves = moveGenerator game
                        let idealMove = Seq.tryFind (fun move -> applyMove game move = newGameState) moves
                        (idealMove, finalScore)
                    else                
                        MiniMax newGameState perspective
            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()
                raise (System.NotImplementedException("Alpha Beta Pruning"))
            NodeCounter.Reset()
            MiniMax
             