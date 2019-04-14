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
            return game;
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
            else
            {
                return true;
            }
        }

        private int HeuristicScore(Game game, Move move)
        {
            TicTacToeOutcome<Player> outcome = GameOutcome(game);
            return outcome.Tag;
        }

        //public Tuple<Move?, int> IterativeMiniMax(Game game, Player perspective)
        //{
        //    bool over = GameOver(game);

        //    if (over)
        //    {
        //        int score =
        //    }
        //}
        public Move FindBestMove(Game game)
        {
            throw new System.NotImplementedException("FindBestMove");
        }

        private List<List<Tuple<int, int>>> Lines(int size)
        {
            List<Tuple<int, int>> horizontal = new List<Tuple<int, int>>();
            List<Tuple<int, int>> vertical = new List<Tuple<int, int>>();
            List<Tuple<int, int>> leftDiagonal = new List<Tuple<int, int>>();
            List<Tuple<int, int>> rightDiagonal = new List<Tuple<int, int>>();
            List<List<Tuple<int, int>>> lines = new List<List<Tuple<int, int>>>();

            for (int i = 0; i < size; i++)
            {
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

            }

            lines.Add(horizontal);
            lines.Add(vertical);
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
            else if (pieces.Contains("X") && pieces.Contains("O"))
            {
                return TicTacToeOutcome<Player>.Draw;
            }
            else
            {
                return TicTacToeOutcome<Player>.NewWin(game.Turn, line);
            }
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
            else if (outcomes.FindAll(o => o.IsUndecided).Count == outcomes.Count)
            {
                return TicTacToeOutcome<Player>.Draw;
            } else
            {
                return TicTacToeOutcome<Player>.Undecided;
            }
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