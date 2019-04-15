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

        public Tuple<Move, int> IterativeMiniMax(Game game, Player perspective, int a, int b)
        {
            NodeCounter.Increment();
            bool over = GameOver(game);
            Move bestMove = null;
            int bestScore = 0;
            int alpha = a;
            int beta = b;

            if (over)
            {
                bestScore = HeuristicScore(game, perspective);
                return new Tuple<Move, int>(bestMove, bestScore);
            }
            List<Move> moves = MoveGenerator(game);
            foreach (Move move in moves)
            {
                Game newState = ApplyMove(game, move);
                Player nextPerspective = newState.Turn;
                Tuple<Move, int> tuple = IterativeMiniMax(newState, nextPerspective, alpha, beta);
                UndoMove(game, move);
                bestMove = move;
                bestScore = tuple.Item2;

                if (nextPerspective == perspective)
                {
                    alpha = Math.Max(alpha, tuple.Item2);
                }
                else
                {
                    beta = Math.Min(beta, tuple.Item2);
                }

                if (alpha >= beta)
                {
                    break;
                }
            }
            return new Tuple<Move, int>(bestMove, bestScore);
        }

        public Move FindBestMove(Game game) 
        {
            NodeCounter.Reset();
            Tuple<Move, int> tuple = IterativeMiniMax(game, game.Turn, int.MinValue, int.MaxValue);
            return tuple.Item1;

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

            return TicTacToeOutcome<Player>.NewWin(game.Turn, line);
        }

        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {
            List<List<Tuple<int, int>>> lines = Lines(game.Size);
            List<TicTacToeOutcome<Player>> outcomes = new List<TicTacToeOutcome<Player>>();

            foreach (List<Tuple<int, int>> line in lines)
            {
                outcomes.Add(CheckLine(game, line));
            }

            if (!outcomes.Contains(TicTacToeOutcome<Player>.Undecided) && !outcomes.Contains(TicTacToeOutcome<Player>.Draw))
            {
                return outcomes.Find(o => o.IsWin);
            }

            if (outcomes.FindAll(o => o.IsUndecided).Count == outcomes.Count)
            {
                return TicTacToeOutcome<Player>.Draw;
            }
            return TicTacToeOutcome<Player>.Undecided;
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