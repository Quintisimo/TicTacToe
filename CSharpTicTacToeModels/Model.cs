namespace QUT.CSharpTicTacToe
{
    public class WithAlphaBetaPruning : ITicTacToeModel<Game, Move, Player>
    {
        public Player Cross => Player.CROSS;
        public Player Nought => Player.NOUGHT;
        public override string ToString()
        {
            return "Impure C# with Alpha Beta Pruning";
        }
        public Game ApplyMove(Game game, Move move)
        {
            throw new System.NotImplementedException("ApplyMove");
        }
        public Move CreateMove(int row, int col)
        {
            return new Move(row, col);
        }
        public Move FindBestMove(Game game)
        {
            throw new System.NotImplementedException("FindBestMove");
        }
        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {
            throw new System.NotImplementedException("GameOutcome");
        }
        public Game GameStart(Player first, int size)
        {
            string[,] pieces = new string[size, size];

            for (int i = 0; i < size - 1; i++)
            {
                for (int j = 0; j < size - 1; j++)
                {
                    pieces[i,j] = "";
                }
            }

            return new Game(first, size, pieces);
        }
    }
}