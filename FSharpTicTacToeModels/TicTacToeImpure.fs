namespace QUT

    module FSharpImpureTicTacToeModel =
    
        type Player = Nought | Cross

        type GameState = 
            { mutable turn: Player; size: int; pieces: string[][] }
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.turn
                member this.Size with get()    = this.size
                member this.getPiece(row, col) = this.pieces.[row].[col]

        type Move = 
            { row: int; col: int }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.col

        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        // The number of lines returned should always be (size*2+2)
        // the number of squares in each line (represented by (row,column) coordinates) should always be equal to size
        // For example, if the input size = 2, then the output would be: 
        //     seq [seq[(0,0);(0,1)];seq[(1,0);(1,1)];seq[(0,0);(1,0)];seq[(0,1);(1,1)];seq[(0,0);(1,1)];seq[(0,1);(1,0)]]
        // The order of the lines and the order of the squares within each line does not matter
        let Lines (size:int) : seq<seq<int*int>> = 
            let horizontal = seq { for row in 0..size - 1 do
                                        yield seq { for col in 0..size - 1 do
                                                        yield (row, col) }}
            let vertical = seq { for row in 0..size - 1 do
                                    yield seq { for col in 0..size - 1 do 
                                                    yield (col, row) }}

            let leftDiagonal = seq { for row in 0..size - 1 do
                                            yield! seq { for col in 0..size - 1 do
                                                            if row = col then yield (row, col) } }

            let rightDiagonal = seq { for row in 0..size - 1 do
                                        yield! seq { for col in 0..size - 1  do 
                                                        if row + col = size - 1 then yield (row, col) } }
            Seq.append (Seq.append horizontal vertical) (Seq.append (Seq. singleton leftDiagonal) (Seq.singleton rightDiagonal))

        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> =
            let pieces = List.map (fun (x, y) -> game.pieces.[x].[y]) (Seq.toList line)
            let won = List.forall (fun piece -> piece <> "" && piece = pieces.[0]) pieces
            let oneCross = List.exists (fun piece -> piece = "X") pieces
            let oneNought = List.exists (fun piece -> piece = "O") pieces
            let draw = oneCross && oneNought

            if won then
                let player = if pieces.[0] = "X" then Cross else Nought
                Win (player, line)
            elif draw then
                Draw
            else
                Undecided

        let GameOutcome (game: GameState) : TicTacToeOutcome<Player> = 
            let lines = Lines game.size
            let statuses  = Seq.map (fun line -> CheckLine game line) lines
            let win = Seq.tryFind (fun status -> status <> Undecided && status <> Draw) statuses
            let draw = Seq.forall (fun status -> status = Draw) statuses

            if Option.isSome win then
                win.Value
            elif draw then
                Draw
            else
                Undecided

        let CreateMove (row: int) (col: int) = { row = row; col = col }
        
        let MoveGenerator (game: GameState) : List<Move> = 
            let possibleIndexes = [ for i = 0 to game.size - 1 do
                                        for j = 0 to game.size - 1 do
                                            if game.pieces.[i].[j] = "" then 
                                                yield (i, j)]
            List.map (fun (x, y) -> CreateMove x y) possibleIndexes

        let ApplyMove (game: GameState) (move: Move) : GameState = 
            game.pieces.[move.row].[move.col] <- if game.turn = Cross then "X" else "O"
            game.turn <- if game.turn = Cross then Nought else Cross
            game

        let UndoMove (game: GameState) (move: Move) : GameState = 
            game.pieces.[move.row].[move.col] <- ""
            game.turn <- if game.turn = Cross then Nought else Cross
            game

        let HeuristicScore (game: GameState) (player: Player) : int =
            match GameOutcome game with
            | Draw -> 0
            | Win (winner, _) -> if winner = player then 1 else -1
            | Undecided -> raise(System.ArgumentException("No Score for Unfinished Game"))

        let GetTurn (game: GameState) : Player = game.turn

        let GameOver (game: GameState) : bool = 
            let over = GameOutcome game

            if over = Undecided then
                false
            else
                true

        let FindBestMove (game: GameState) : Move = 
            let rec MutableMinMax (game: GameState) (player: Player) : Option<Move> =
                let over = GameOver game
            
                if over then 
                    None
                else
                    let mutable alpha = System.Int32.MinValue
                    let mutable beta = System.Int32.MaxValue
                    let nextPerspective = GetTurn game
                    let possibleMoves = MoveGenerator game
                    let gameState = List.map (fun move -> ApplyMove game move) possibleMoves
                    let scores = List.map (fun game -> HeuristicScore game game.turn) gameState

                    if nextPerspective = game.turn then
                        let idealScore = List.max scores
                        alpha <- max alpha idealScore

                        if alpha >= beta then
                            None
                        else
                            let index = List.findIndex (fun score -> score = idealScore) scores
                            Some possibleMoves.[index]
                    else
                        let idealScore = List.min scores
                        beta <- min beta idealScore

                        if alpha >= beta then
                            None
                        else
                            let index = List.findIndex (fun score -> score = idealScore) scores
                            Some possibleMoves.[index]

            let move = MutableMinMax game game.turn
            move.Value

        let GameStart (first: Player) (size: int) = 
            let pieces = Array.init size (fun _ -> Array.init size (fun _ -> ""))
            { turn = first; size = size; pieces = pieces }
        // plus other helper functions ...





        type WithAlphaBetaPruning() =
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = raise (System.NotImplementedException("getCross"))
                member this.Nought with get()            = raise (System.NotImplementedException("getNought"))
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game 
                member this.ApplyMove(game, move)        = ApplyMove game  move
                member this.FindBestMove(game)           = FindBestMove game