using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        public string[,] pieces;
        private int size;
        private Player turn;
        public int Size => size;
        public Player Turn => turn;

        public Game(Player first, int size, string[,] pieces)
        {
            this.turn = first;
            this.size = size;
            this.pieces = pieces;
        }

        public string getPiece(int row, int col)
        {
            return pieces[row,col];
        }
    }
}