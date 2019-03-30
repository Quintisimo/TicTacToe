namespace QUT

    module FSharpPureTicTacToeModel =
    
        // type to represent the two players: Noughts and Crosses
        type Player = Nought | Cross

        // type to represent a single move specified using (row, column) coordinates of the selected square
        type Move = 
            { row: int; col: int }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.col

        // type to represent the current state of the game, including the size of the game (NxN), who's turn it is and the pieces on the board
        type GameState = 
            { turn: Player; size: int; pieces: List<List<string>>  }
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.turn
                member this.Size with get()    = this.size
                member this.getPiece(row, col) = this.pieces.[row].[col]

        let CreateMove (row: int) (col: int) : Move = { row = row; col = col }

        let ApplyMove (oldState:GameState) (move: Move) : GameState = 
            let appliedMove = [ for i in 0..oldState.size - 1 do
                                    yield [ for j in 0..oldState.size - 1 do
                                                if i = move.row && j = move.col then
                                                    if oldState.turn = Nought then
                                                        yield "O"
                                                    elif oldState.turn = Cross then
                                                        yield "X"
                                                else
                                                    yield oldState.pieces.[i].[j] ] ]
            let nextTurn = if oldState.turn = Nought then Cross else Nought
            { turn = nextTurn; size = oldState.size; pieces = appliedMove }

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
            let draw = List.forall (fun piece -> piece = "X" || piece = "O") pieces

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


        let GameStart (firstPlayer:Player) (size: int) : GameState = 
            let pieces = List.init size (fun _ -> List.init size (fun _ -> ""))
            { turn = firstPlayer; size = size; pieces = pieces }

        let GetTurn (game: GameState) : Player = game.turn

        let MoveGenerator (game: GameState) : seq<Move> = 
            let possibleIndexes = [ for i = 0 to game.size - 1 do
                                        for j = 0 to game.size - 1 do
                                            if game.pieces.[i].[j] = "" then 
                                                yield (i, j)]
            let possibleMoves = List.map (fun (x, y) -> CreateMove x y) possibleIndexes
            List.toSeq possibleMoves

        let GameOver (game: GameState) : bool = 
            let over = GameOutcome game

            if over = Undecided then
                false
            else
                true

        let HeuristicScore (game: GameState) (player: Player) : int =
            let outcome = GameOutcome game

            if outcome = Draw then
                0 
            elif outcome = Undecided then
                -1
            else
                1

        let MiniMax (game: GameState) : Move = 
            let minMax = GameTheory.MiniMaxGenerator HeuristicScore GetTurn GameOver MoveGenerator ApplyMove
            let (move, _) = minMax game game.turn
            move.Value

            //if Option.isSome move then
            //else
                //None

        let MiniMaxWithPruning (game: GameState) : Move = 
            let minMaxPruning = GameTheory.MiniMaxWithAlphaBetaPruningGenerator HeuristicScore GetTurn GameOver MoveGenerator ApplyMove
            let (move, _) = minMaxPruning System.Int32.MinValue System.Int32.MaxValue game game.turn

            move.Value

            //if Option.isSome move then
            //else
                //None
        // plus other helper functions ...




        [<AbstractClass>]
        type Model() =
            abstract member FindBestMove : GameState -> Move
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross 
                member this.Nought with get()            = Nought 
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game
                member this.ApplyMove(game, move)        = ApplyMove game move 
                member this.FindBestMove(game)           = this.FindBestMove game

        type BasicMiniMax() =
            inherit Model()
            override this.ToString()         = "Pure F# with basic MiniMax";
            override this.FindBestMove(game) = MiniMax game


        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = MiniMaxWithPruning game