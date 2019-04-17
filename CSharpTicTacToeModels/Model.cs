using System;
using System.Collections.Generic;

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
            game.pieces[move.Row, move.Col] = game.Turn == Player.CROSS ? "X" : "O";
            game.Turn = game.Turn == Player.CROSS ? Player.NOUGHT : Player.CROSS;
            return game;
        }

        private void UndoMove(Game game, Move move)
        {
            game.pieces[move.Row, move.Col] = "";
            game.Turn = game.Turn == Player.CROSS ? Player.NOUGHT : Player.CROSS;
        }

        public Move CreateMove(int row, int col)
        {
            return new Move(row, col);
        }

        private bool GameOver(Game game)
        {
            TicTacToeOutcome<Player> outcome = GameOutcome(game);

            if (outcome == TicTacToeOutcome<Player>.Undecided)
            {
                return false;
            }
            return true;
        }

        private int HeuristicScore(Game game, Player player) 
        {
            TicTacToeOutcome<Player> outcome = GameOutcome(game);

            if (outcome.IsDraw) 
            {
                return 0;
            }

            if (outcome.IsWin)
            {
                var win = outcome as TicTacToeOutcome<Player>.Win;
                if (win.winner == player) {
                    return 1;
                }
                return -1;

            }
            return -2;
        }

        private List<Move> MoveGenerator(Game game) 
        {
            List<Move> possibleMoves = new List<Move>();
            for (int i = 0; i < game.Size; i++)
            {
                for (int j = 0; j < game.Size; j++)
                {
                    if (game.getPiece(i, j) == "")
                    {
                        Move move = CreateMove(i, j);
                        possibleMoves.Add(move);
                    }
                }
            }
            return possibleMoves;
        }

        public Tuple<Move, int> IterativeMiniMax(Game game, Player perspective)
        {
            NodeCounter.Increment();
            bool over = GameOver(game);
            int alpha = int.MinValue;
            int beta = int.MaxValue;

            if (over)
            {
                int score = HeuristicScore(game, perspective);
                return new Tuple<Move, int>(null, score);
            }

            List<Move> moves = MoveGenerator(game);
            Player nextPerspective = game.Turn;

            if (nextPerspective == perspective)
            {
                int bestScore = int.MinValue;
                Move bestMove = null;

                foreach(Move move in moves)
                {
                    Game newState = ApplyMove(game, move);
                    var (_, score) = IterativeMiniMax(newState, perspective);
                    bestScore = Math.Max(bestScore, score);
                    alpha = Math.Max(alpha, bestScore);

                    if (alpha == score)
                    {
                        bestMove = move;
                    }

                    if (alpha >= beta)
                    {
                        break;
                    }
                    UndoMove(game, move);
                }
                return new Tuple<Move, int>(bestMove, bestScore);
            }
            else
            {
                int bestScore = int.MaxValue;
                Move bestMove = null;

                foreach(Move move in moves)
                {
                    Game newState = ApplyMove(game, move);
                    var (_, score) = IterativeMiniMax(newState, perspective);
                    bestScore = Math.Min(bestScore, score);
                    beta = Math.Min(beta, bestScore);

                    if (beta == score)
                    {
                        bestMove = move;
                    }

                    if (alpha >= beta)
                    {
                        break;
                    }
                    UndoMove(game, move);
                }
                return new Tuple<Move, int>(bestMove, bestScore);
            }
        }

        public Move FindBestMove(Game game) 
        {
            NodeCounter.Reset();
            var (move, _) = IterativeMiniMax(game, game.Turn);
            return move;

        }

        private List<List<Tuple<int, int>>> Lines(int size)
        {
            List<Tuple<int, int>> leftDiagonal = new List<Tuple<int, int>>();
            List<Tuple<int, int>> rightDiagonal = new List<Tuple<int, int>>();
            List<List<Tuple<int, int>>> lines = new List<List<Tuple<int, int>>>();

            for (int i = 0; i < size; i++)
            {
                List<Tuple<int, int>> horizontal = new List<Tuple<int, int>>();
                List<Tuple<int, int>> vertical = new List<Tuple<int, int>>();
                for (int j = 0; j < size; j++)
                {
                    horizontal.Add(new Tuple<int, int>(i, j));
                    vertical.Add(new Tuple<int, int>(j, i));

                    if (i == j)
                    {
                        leftDiagonal.Add(new Tuple<int, int>(i, j));
                    }

                    if (i + j == size - 1)
                    {
                        rightDiagonal.Add(new Tuple<int, int>(i, j));
                    }
                }
                lines.Add(horizontal);
                lines.Add(vertical);
            }

            lines.Add(leftDiagonal);
            lines.Add(rightDiagonal);
            return lines;
        }

        private TicTacToeOutcome<Player> CheckLine(Game game, List<Tuple<int, int>> line)
        {
            List<string> pieces = new List<string>();

            foreach (Tuple<int, int> i in line)
            {
                pieces.Add(game.getPiece(i.Item1, i.Item2));
            }

            if (pieces.Contains(""))
            {
                return TicTacToeOutcome<Player>.Undecided;
            }

            if (pieces.Contains("X") && pieces.Contains("O"))
            {
                return TicTacToeOutcome<Player>.Draw;
            }

            if (pieces.Contains("X"))
            {
                return TicTacToeOutcome<Player>.NewWin(Player.CROSS, line);
            }
            return TicTacToeOutcome<Player>.NewWin(Player.NOUGHT, line);
        }

        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {
            List<List<Tuple<int, int>>> lines = Lines(game.Size);
            List<TicTacToeOutcome<Player>> outcomes = new List<TicTacToeOutcome<Player>>();
            
            foreach (List<Tuple<int, int>> line in lines)
            {
                TicTacToeOutcome<Player> outcome = CheckLine(game, line);

                if (outcome.IsWin)
                {
                    return outcome;
                }
                else
                {
                    outcomes.Add(outcome);
                }

            }

            if (outcomes.Contains(TicTacToeOutcome<Player>.Undecided))
            {
                return TicTacToeOutcome<Player>.Undecided;
            }
            return TicTacToeOutcome<Player>.Draw;
        }

        public Game GameStart(Player first, int size)
        {
            string[,] pieces = new string[size, size];

            for (int i = 0; i < size; i++)
            {
                for (int j = 0; j < size; j++)
                {
                    pieces[i,j] = "";
                }
            }

            return new Game(first, size, pieces);
        }
    }
}