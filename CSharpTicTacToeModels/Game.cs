using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        private readonly string[,] pieces;

        public Game(Player first, int size, string[,] pieces)
        {
            Turn = first;
            Size = size;
            this.pieces = pieces;
        }

        public int Size { get; }
        public Player Turn { get; }
        public string getPiece(int row, int col)
        {
            return pieces[row,col];
        }
    }
}