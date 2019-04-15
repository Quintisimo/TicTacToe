using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        public string[,] pieces;
        public int Size { get; }
        public Player Turn { set; get; }

        public Game(Player first, int size, string[,] pieces)
        {
            Turn = first;
            Size = size;
            this.pieces = pieces;
        }

        public string getPiece(int row, int col)
        {
            return pieces[row,col];
        }
    }
}