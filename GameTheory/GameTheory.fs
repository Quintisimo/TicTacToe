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
                    let tuples = List.map (fun (gameState, _) -> MiniMax gameState perspective) stateAndMove
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

                    let rec maximizingPlayer (tuples: ('Game * 'Move)list) (counter: int) (alpha: int) (beta: int) (previousScore: int) =
                        let (gameState, move) = tuples.[counter]
                        let nextPespective = getTurn gameState
                        let (_, score) = MiniMax alpha beta gameState nextPespective
                        let idealScore = max previousScore score
                        let newAlpha = max alpha idealScore

                        if newAlpha >= beta then
                            (Some move, idealScore)
                        else
                            let newCounter = counter + 1
                            if newCounter < tuples.Length - 1 then
                                maximizingPlayer tuples newCounter alpha beta idealScore
                            else
                                (Some move, idealScore)

                    let rec minimizingPlayer (tuples: ('Game * 'Move)list) (counter: int) (alpha: int) (beta: int) (previousScore: int) = 
                        let (gameState, move) = tuples.[counter]
                        let nextPerspective = getTurn gameState
                        let (_, score) = MiniMax alpha beta gameState nextPerspective
                        let idealScore = min previousScore score
                        let newBeta = min beta idealScore

                        if alpha >= newBeta then
                            (Some move, idealScore)
                        else
                            let newCounter = counter + 1
                            if newCounter < tuples.Length - 1 then
                                minimizingPlayer tuples newCounter alpha beta idealScore
                            else
                                (Some move, idealScore)

                    let nextPerspective = getTurn oldState

                    if nextPerspective = perspective then
                        maximizingPlayer stateAndMove 0 alpha beta System.Int32.MinValue
                    else
                        minimizingPlayer stateAndMove 0 alpha beta System.Int32.MaxValue
            NodeCounter.Reset()
            MiniMax
             