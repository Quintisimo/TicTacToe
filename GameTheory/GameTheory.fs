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

        let rec maximizingPlayer (previousBest: (Option<'Move> * int)) (nodes: ('Game * 'Move) list) (perspective: 'Player) (alpha: int) (beta: int) (miniMax: int -> int -> 'Game -> 'Player -> Option<'Move> * int) (counter: int) =
            if counter < nodes.Length then
                let (game, move) = nodes.[counter]
                let (_, score) = miniMax alpha beta game perspective
                let (_, previousScore) = previousBest
                let newScore = max score previousScore
                let newBest = match (newScore = previousScore) with
                              | true -> previousBest
                              | false -> (Some move, score)

                let newAlpha = max alpha newScore

                let newAlphaTuple = match (newAlpha = newScore) with
                                    | true -> newBest
                                    | false -> previousBest

                if (newAlpha >= beta) then
                    newAlphaTuple
                else
                    maximizingPlayer newAlphaTuple nodes perspective newAlpha beta miniMax (counter + 1)
            else
                previousBest

        let rec minimizingPlayer (previousBest: (Option<'Move> * int)) (nodes: ('Game * 'Move) list) (perspective: 'Player) (alpha: int) (beta: int) (miniMax: int -> int -> 'Game -> 'Player -> Option<'Move> * int) (counter: int) =
            if counter < nodes.Length then
                let (game, move) = nodes.[counter]
                let (_, score) = miniMax alpha beta game perspective
                let (_, previousScore) = previousBest
                let newScore = min score previousScore
                let newBest = match (newScore = previousScore) with
                              | true -> previousBest
                              | false -> (Some move, score)

                let newBeta = min beta newScore
                let newBetaTuple = match (newBeta = newScore) with
                                   | true -> newBest
                                   | false -> previousBest

                if (alpha >= newBeta) then
                    newBetaTuple
                else
                    minimizingPlayer newBetaTuple nodes perspective alpha newBeta miniMax (counter + 1)
            else
                previousBest

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
                    let nextPerspective = getTurn oldState

                    if nextPerspective = perspective then
                        let initialScore = System.Int32.MinValue
                        maximizingPlayer (None, initialScore) stateAndMove perspective alpha beta MiniMax 0
                    else
                        let initialScore = System.Int32.MaxValue
                        minimizingPlayer (None, initialScore) stateAndMove perspective alpha beta MiniMax 0
                        
            NodeCounter.Reset()
            MiniMax
             