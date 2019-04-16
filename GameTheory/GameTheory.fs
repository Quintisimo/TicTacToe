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

        let rec AlphaBetaPruning  (tuples: ('Game * 'Move)list) (counter: int) (alpha: int) (beta: int) (getTurn: 'Game -> 'Player) (perspective: 'Player) (miniMax: int -> int -> 'Game -> 'Player -> Option<'Move> * int) =
            let (gameState, move) = tuples.[counter]
            let (_, score) = miniMax alpha beta gameState perspective
            let nextPerspective = getTurn gameState
            let newAlpha = match (nextPerspective = perspective) with 
                            | true -> max alpha score
                            | false -> alpha

            let newBeta = match (nextPerspective <> perspective) with
                            | true -> min beta score
                            | false -> beta

            if newAlpha >= newBeta then
                if nextPerspective = perspective then
                    (Some move, newAlpha)
                else
                    (Some move, newBeta)
            else
                let newCounter = counter + 1

                if newCounter < tuples.Length - 1 then
                    AlphaBetaPruning tuples newCounter newAlpha newBeta getTurn perspective miniMax
                else
                    (Some move, score)

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
                    AlphaBetaPruning stateAndMove 0 alpha beta getTurn perspective MiniMax
            NodeCounter.Reset()
            MiniMax
             